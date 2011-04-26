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
open Printf

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

let simple_var = make_var dummy

let make_pvar i z =
  PVar(i, (dummy, None, z), None)

let make_application i name e =
  EApp(i, e, make_var i name) 

let simple_app e1 e2=
    EApp(dummy, e1, e2)

(* functions in prelude, to be moved to some other place in the future *)
(* TODO(astory): move me! *)
let globals = List.fold_left
  (fun map (k, v) -> StrMap.add k v map)
  StrMap.empty
  [
  ("_match_failure", simple_var "_match_failure");
  ("land", simple_var "land");
  ("_equals", simple_var "_equals");
  ("_is_pair", simple_var "_is_pair");
  ("_isinstance", simple_var "_isinstance");
  ("_value", simple_var "_value");
  ]

let g_find name = StrMap.find name globals

let match_exception(info) =
    EApp(info,
      g_find "_match_failure",
      EString(dummy,
        String.escaped (sprintf "Match_failure %s" (Info.string_of_t info)))
    )

let make_and e1 e2 =
    simple_app (simple_app(g_find "land") e1) e2

let make_instance e1 e2 =
    simple_app (simple_app(g_find "_isinstance") e1) e2

let make_equals (e1:exp) (e2:exp) =
    simple_app
      (simple_app (g_find "_equals") e1)
      e2

(* end of such functions *) 

let rec make_match (pat:pattern) (e:exp) =
    match pat with
    | PWild(_) -> EBool(dummy, true)
    | PUnit(_) -> make_equals e (EUnit(dummy))
    | PBool(_, value) -> make_equals e (EBool(dummy, value))
    | PInteger(_, value) -> make_equals e (EInteger(dummy, value))
    | PString(_, value) -> make_equals e (EString(dummy, value))
    | PVar(_) -> EBool(dummy, false)
    | PData(_,(_,_,name), pat_opt) -> 
      (* logic:  check that e is of the same type as name, and then recurse with
      pattern on e.value *)
      let safe = make_instance (simple_var name) e in
      (match pat_opt with
      | Some pat ->
        make_and
          safe
          (make_match pat (simple_app (g_find "_value") e))
      | None -> safe)
    | PPair(_, p1, p2) ->
      (* Logic:
       * Test if e is a pair.  If it isn't, return false.  If it is, then it's
       * safe to use a Let statement to split it into its two halves, and then
       * recursively check the two sub-{pattern,expression}s.
       *)
      let e1_name = undersc() in
      let e2_name = undersc() in
      make_and
        (simple_app (g_find "_is_pair") e)
        (ELet(
          dummy,
          Bind(dummy,
            PPair(dummy, make_pvar dummy e1_name, make_pvar dummy e2_name),
            None,
            e),
          make_and
            (make_match p1 (simple_var e1_name))
              (make_match p2 (simple_var e2_name))
          ))

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
        let zs = 
          StrSet.elements 
            (StrSet.diff (fv e1') 
               (StrSet.union 
                  (Data.List.fold_left (fun vs (f,_) -> StrSet.add f vs) vs ds1')
                  (bv p))) in 
        let f = 
          List.fold_left 
            (fun f z -> make_function i (make_pvar dummy z) f)
            f1 zs in 
        let e' = 
          List.fold_right 
            (make_application i) 
            zs (make_var i h) in
        (e', (h,f) :: ds1') 
    | ECond(i,e1,e2,e3) -> 
      let e1',ds1' = convert_exp false vs e1 in 
      let e2',ds2' = convert_exp false vs e2 in 
      let e3',ds3' = convert_exp false vs e3 in 
      (ECond(i,e1',e2',e3'),ds1' @ ds2' @ ds3')
    | ELet (i,Bind(_,pat,typ,l_exp),exp) ->
      convert_exp false vs (EApp(i,EFun(i,Param(i,pat,typ),exp),l_exp))
    | EAsc(_,e1,_) -> convert_exp top vs e1
    | EOver(_,_,_) ->
      Error.simple_error "Overloaded operator resolution not implemented" 
        
    | EPair(i,e1,e2) ->
      let e1',ds1' = convert_exp false vs e1 in 
      let e2',ds2' = convert_exp false vs e2 in 
      (EPair(i,e1',e2'),ds1' @ ds2')
    | ECase (i,e,es) ->
        let e_name = undersc() in
        let e_pat = make_pvar dummy e_name in
        let e_var = make_var dummy e_name in
        let fold_case (pat, e) else_e =
            let pat_sub_e = ELet(dummy, Bind(dummy, pat, None, e_var), e) in
            ECond(dummy, make_match pat e_var, pat_sub_e, else_e)
        in
        let expr =
            ELet(i, Bind(dummy, e_pat, None, e),
                List.fold_right fold_case es (match_exception i)
            ) in
        convert_exp false vs expr
        
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
    | DType(_) -> ([d], vs)

let convert_decls (ds:decl list) =
  List.fold_left 
    (fun (decls,vs) d -> 
      let decls',vs' = convert_decl d vs in 
      (decls @ decls',vs'))
    ([],StrSet.empty) ds

let convert_module (Modl(i,m,decls)) =
  let decls',_ = convert_decls decls in 
  Modl(i,m,decls')
