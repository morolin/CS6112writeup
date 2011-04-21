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

let dummy = Info.dummy("dummy info")

let fresh_cell = ref 0 

(* poor man's gensym; needs to be repleaced with a real one *)
let get_fresh s =
  incr fresh_cell;
  s ^ (string_of_int !fresh_cell)

let fresh () = get_fresh "f_"

let undersc () = get_fresh "_"

let make_function i pat e =
  EFun(i, Param(dummy,pat,None),e)

let make_var i z =
  EVar(i, (dummy,None, z))

let make_pvar i z =
  PVar(i, (dummy, None, z), None)

let make_application i name e =
  EApp(i, e, make_var i name) 

let rec convert_exp (top:bool) (vs:StrSet.t) (e:exp) = 
  match e with
    | EVar(_) -> 
      (e, [])
    | EApp(i,e1,e2) ->
      let e1',ds1' = convert_exp false vs e1 in 
      let e2',ds2' = convert_exp false vs e2 in 
      (EApp(i, e1', e2'), ds1' @ ds2')
    | EFun(i,Param(_,p,_),e1) ->
      let e1', ds1' = convert_exp top vs e1 in 
      let f1 = make_function i p e1' in 
      if top then (f1,ds1')
      else 
        let h = fresh () in
        let zs = StrSet.elements (StrSet.diff (fv e1') (StrSet.union vs (bv p))) in 
        let f = 
          List.fold_left 
            (fun f z -> make_function i (make_pvar dummy z) f)
            f1 zs in 
        let e' = 
          List.fold_right 
            (make_application i) 
            zs (make_var i h) in
        (e', (h,f) :: ds1') 
    | ELet (i,Bind(_,pat,typ,l_exp),exp) ->
      convert_exp false vs (EApp(i,EFun(i,Param(i,pat,typ),exp),l_exp))
    | EAsc(_,e1,_) -> convert_exp top vs e1
    | EOver(_,_,_) ->
      Error.simple_error "Overloaded Operator found during compilation" 
        
    | EPair(i,e1,e2) ->
      let e1',ds1' = convert_exp false vs e1 in 
      let e2',ds2' = convert_exp false vs e2 in 
      (EPair(i,e1',e2'),ds1' @ ds2')
    | ECase (i,e,es) ->
      Error.simple_error "Case unimplemented"
        
    | EUnit(_) | EInteger(_) | EChar(_) | EString (_) | EBool (_) -> 
      (e,[]) 

let mk_decl i f e = 
  DLet(i,Bind(dummy,PVar(dummy,(dummy, None, f), None), None, e))

let convert_decl (d:decl) (vs:StrSet.t) = 
  match d with
    | DLet (i, Bind(_,p,_,e)) -> 
      let vs' = match p with 
        | PVar(_,(_,None,x),_) -> StrSet.add x vs 
        | _ -> vs in 
      let e', ds' = convert_exp true vs' e in
      let ds'' = 
        List.fold_left
          (fun decls (h,f) -> 
            (mk_decl (Syntax.info_of_exp f) h f)::decls)
          [DLet(i, Bind(dummy, p, None, e'))]
          ds' in 
      (ds'',vs')
    | DType(_) -> Error.simple_error "dtype unimplemented"

let convert_decls (ds:decl list) =
  List.fold_left 
    (fun (decls,vs) d -> 
      let decls',vs' = convert_decl d vs in 
      (decls @ decls',vs'))
    ([],StrSet.empty) ds

let convert_module (Modl(i,m,decls)) =
  let decls',_ = convert_decls decls in 
  Modl(i,m,decls')
