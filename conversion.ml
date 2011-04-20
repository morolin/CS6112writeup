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

let dummy = Info.dummy("dummy info")

let fresh_cell = ref 0 

let get_fresh s =
  incr fresh_cell;
  s ^ (string_of_int !fresh_cell)

let fresh () = get_fresh "x_"

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

let make_function info pat e =
  EFun(info, Param(dummy,pat,None),e)

let make_var info var_name =
  EVar(info, (dummy,None, var_name))

let make_vpat info var_name =
  PVar(info, (dummy, None, var_name), None)

let make_application info name e =
  EApp(info, e, make_var info name) 

let rec lift (e:exp)(s:env) = match e with
  | EVar(_) -> (e, s)
  | EApp(i,f,value) ->
      let f', value', s' = lift2 f value s in 
      (EApp(i, f', value'),s')
  | EFun(i,Param(_,pat,_),exp) ->
      let e1', s' = lift exp s in
      let free = StrSet.diff (fv e1') (names_of_pat pat) in
      let h = fresh() in
      let base = make_function i pat e1' in 
      let f = StrSet.fold
          (fun name -> make_function i (make_vpat dummy name))
          free
          base
        in
      let s'' = (h,f) :: s' in
      let free_l = StrSet.elements free in
      let e' = List.fold_right (make_application i) free_l (make_var i h) in
      (e', s'')
  | ELet (i,Bind(_,pat,typ,l_exp),exp) ->
    lift (EApp(i,EFun(i,Param(i,pat,typ),exp),l_exp)) s
  | EAsc(_,exp,_) -> lift exp s
  | EOver(_,_,_) ->
    Error.simple_error "Overloaded Operator found during compilation"

  | EPair(i,e1,e2) ->
      let e1', e2', s' = lift2 e1 e2 s in
      (EPair(i,e1',e2'),s')
  | ECase (i,e,es) ->
    (*
    let e_name = undersc() in
    let e_var = make_var dummy e_name in
    let cases =
      List.map (lambda (pat, exp) -> lift) es
    in
    ELet (i, Bind(dummy, make_vpat dummy e_name, cases)
    *)
    Error.simple_error "Case unimplemented"

  | EUnit(_) | EInteger(_) | EChar(_) | EString (_) | EBool (_) -> (e,s)

and lift2 e1 e2 s =
  let (e1', s') = lift e1 s in
  let (e2', s'') = lift e2 s' in
  (e1', e2', s'')

let convert_exp (e : exp) =
  lift e []

let convert_decl (d:decl) = match d with
  | DLet (i, Bind(_, p, _, e)) ->
    let e', s = convert_exp e in
    List.fold_left
      (fun decls (f, e) ->
        DLet(i, Bind(dummy, PVar(dummy, (dummy, None, f), None), None, e)) :: decls)
      [DLet(i, Bind(dummy, p, None, e'))]
      s
  | DType(_) -> Error.simple_error "dtype unimplemented"

let convert_decls (ds : decl list) =
  List.fold_left (fun acc d -> acc @ (convert_decl d)) [] ds

let convert_module (Modl(i, m, decls)) =
  Modl(i, m, convert_decls decls)
