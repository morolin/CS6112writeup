(******************************************************************************)
(* The Frenetic Project                                                       *)
(* frenetic@frenetic-lang.org                                                 *)
(******************************************************************************)
(* Licensed to the Frenetic Project by one or more contributors. See the      *)
(* NOTICE file distributed with this work for additional information          *)
(* regarding copyright and ownership. The Frenetic Project licenses this      *)
(* file to you under the following license.                                   *)
(*                                                                            *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided the following conditions are met:     *)
(* - Redistributions of source code must retain the above copyright           *)
(*   notice, this list of conditions and the following disclaimer.            *)
(* - Redistributions in binary form must reproduce the above copyright        *)
(*   notice, this list of conditions and the following disclaimer in          *)
(*   the documentation or other materials provided with the distribution.     *)
(* - The names of the copyright holds and contributors may not be used to     *)
(*   endorse or promote products derived from this work without specific      *)
(*   prior written permission.                                                *)
(*                                                                            *)
(* Unless required by applicable law or agreed to in writing, software        *)
(* distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  *)
(* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   *)
(* LICENSE file distributed with this work for specific language governing    *)
(* permissions and limitations under the License.                             *)
(******************************************************************************)
(* /src/compiler/conversion.ml                                                *)
(* AST manipulations                                                          *)
(* $Id$ *)
(******************************************************************************)

module StrMap = Map.Make(String)

open Syntax

type env = (string * exp) list

let names_of_env s = 
  Data.List.fold_left (fun acc (x,_) -> StrSet.add x acc) StrSet.empty s

let string_of_env s = 
  "{" ^ Util.concat_list ", " (Data.List.map (fun (x,e) -> x ^ " -> " ^ (Pretty.string_of_exp e)) s) ^ "}" 
  
let dummy = Info.dummy("dummy info")

let fresh_cell = ref 0 

let get_fresh s =
  incr fresh_cell;
  s ^ (string_of_int !fresh_cell)

let fresh () = get_fresh "f_"

let undersc () = get_fresh "_"

let rec names_of_pat pat = match pat with
  | PWild(_) -> StrSet.empty
  | PUnit(_) -> StrSet.empty
  | PBool(_) -> StrSet.empty
  | PInteger(_) -> StrSet.empty
  | PString(_) -> StrSet.empty
  | PVar(_,(_,_,varname),_) -> StrSet.singleton varname
  | PData(_) -> Error.simple_error "data in patterns not implemented yet"
  | PPair(_, p1, p2) ->
    let s1 = names_of_pat p1 in 
    let s2 = names_of_pat p2 in 
    StrSet.union s1 s2

let make_function i pat e =
  EFun(i, Param(dummy,pat,None),e)

let make_var i var_name =
  EVar(i, (dummy,None, var_name))

let make_vpat i var_name =
  PVar(i, (dummy, None, var_name), None)

let make_application i name e =
  EApp(i, e, make_var i name) 

let rec convert_exp (top:bool) (e:exp) (s:StrSet.t) = 
  match e with
    | EVar(_) -> (e, [])
    | EApp(i,e1,e2) ->
      let e1', e2', ds = convert_exp2 top e1 e2 s in 
      (EApp(i, e1', e2'),ds)
    | EFun(i,Param(_,p,_),e1) ->
      let xs = names_of_pat p in 
      let e1', ds1 = convert_exp false e1 (StrSet.diff s xs) in
      let base = make_function i p e1' in 
      if top then (base,ds1) 
      else 
        let h = fresh () in
        let free = StrSet.diff (fv e1') (StrSet.union xs s) in 
        let f = 
          StrSet.fold
            (fun x -> make_function i (make_vpat dummy x))
            free base in 
        let free_l = StrSet.elements free in
        let ds = (h,f) :: ds1 in 
        let e' = List.fold_right (make_application i) free_l (make_var i h) in
        (e', ds)
    | ELet (i,Bind(_,pat,typ,l_exp),exp) ->
      convert_exp false (EApp(i,EFun(i,Param(i,pat,typ),exp),l_exp)) s
    | EAsc(_,exp,_) -> convert_exp top exp s
    | EOver(_,_,_) ->
      Error.simple_error "Overloaded Operator found during compilation" 
        
    | EPair(i,e1,e2) ->
      let e1', e2', ds = convert_exp2 top e1 e2 s in
      (EPair(i,e1',e2'),ds)
    | ECase (i,e,es) ->
    (*
      let e_name = undersc() in
      let e_var = make_var dummy e_name in
      let cases =
      List.map (lambda (pat, exp) -> convert_exp) es
      in
      ELet (i, Bind(dummy, make_vpat dummy e_name, cases)
    *)
      Error.simple_error "Case unimplemented"
        
    | EUnit(_) | EInteger(_) | EChar(_) | EString (_) | EBool (_) -> 
      (e,[]) 

and convert_exp2 top e1 e2 s =
  let e1', ds1 = convert_exp top e1 s in
  let e2', ds2 = convert_exp false e2 s in
  (e1', e2', ds1 @ ds2)

let mk_decl i f e = 
  DLet(i,Bind(dummy,PVar(dummy,(dummy, None, f), None), None, e))

let convert_decl (d:decl) (s:StrSet.t) = 
  match d with
    | DLet (i, Bind(_, p, _, e)) ->
      let e', ds' = convert_exp true e s in
      let s' = Data.List.fold_left (fun s (f,e) -> StrSet.add f s) s ds' in 
      let s'' = StrSet.union s' (names_of_pat p) in 
      let ds'' = 
        List.fold_left
          (fun decls (f,e) -> (mk_decl (Syntax.info_of_exp e) f e)::decls)
          [DLet(i, Bind(dummy, p, None, e'))]
          ds' in 
      (ds'',s'')
    | DType(_) -> Error.simple_error "dtype unimplemented"

let convert_decls (ds:decl list) =
  List.fold_left 
    (fun (decls,s) d -> 
      let decls',s' = convert_decl d s in 
      (decls @ decls',s'))
    ([],StrSet.empty) ds

let convert_module (Modl(i,m,decls)) =
  let decls',_ = convert_decls decls in 
  Modl(i,m,decls')
