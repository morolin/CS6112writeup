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
(* /src/compiler/py.ml                                                        *)
(* Pretty printer for python output                                           *)
(* $Id$ *)
(******************************************************************************)

(* ----- imports and abbreviations ----- *)
open Syntax
open Printf

exception PyException of string

let unimp()= failwith "unimplemented"

let pybool b = match b with
  | true -> "True"
  | false -> "False"

let pystring s = sprintf "\"%s\"" s

let rec underscores = Util.count_prefix "_"

let rec get_u = Util.get_lazy_string

(* used tracks the various variables we've used for underscores since you can't
 * have two variables with the same name in the same pattern.  We can recycle
 * underscores between patterns though *)
let rec exp_pat_help fresh pat = match pat with
  | PWild(_) -> get_u fresh
  | PUnit(_) -> get_u fresh
  | PBool(_) -> get_u fresh
  | PInteger(_) -> get_u fresh
  | PString(_) -> get_u fresh
  | PVar(_,(_,_,varname),_) ->
    (varname, fresh)
  | PData(_) -> Error.simple_error "data in patterns not implemented yet"
  | PPair(_, p1, p2) ->
    let (s1, fresh') = exp_pat_help fresh p1 in
    let (s2, fresh'') = exp_pat_help fresh' p2 in
    (sprintf "(%s, %s)" s1 s2, fresh'')

let expand_pattern pat =
    let (s, _) = exp_pat_help (underscores 0) pat in
    s

(* The idea with this function is to attempt to evaluate down the pattern tree,
 * but if we run into difficulty, pass a None up the stack to serve as an error
 * flag.  This is safe because None has no other usage in this compiler. *)
let rec descend_pattern e_name (pat, exp) =
    let fexp = format_exp exp in
    match pat with
    | PWild(_) -> fexp
    | PUnit(_) ->
      let valid = 
        sprintf "(isinstance(%s, tuple) and len(%s) == 0)" e_name e_name in
      sprintf "%s if %s else None" fexp valid
    | PBool(_, value) ->
      let valid = sprintf "(%s == %s)" e_name (pybool value) in
      sprintf "%s if %s else None" fexp valid
    | PInteger(_, value) -> 
      let valid = sprintf "(%s == %s)" e_name (string_of_int value) in
      sprintf "%s if %s else None" fexp valid
    | PString(_, value) -> 
      let valid = sprintf "(%s == %s)" e_name (pystring value) in
      sprintf "%s if %s else None" fexp valid
    | PVar(_, (_,_,varname),_) ->
      sprintf "(lambda %s : fexp)(%s)" varname e_name
    | PData(_) -> Error.simple_error "data in patterns not implemented yet"
    (*| PPair(_, p1, p2) ->
      let valid = "(isinstance(%s, tuple) and len(%s) == 2)" e_name e_name
      sprintf "%s if %s else None" fexp valid *)

and build_case e_name (pat, exp) next =
    sprintf "_default(%s, %s)" next (descend_pattern e_name (pat, exp))

and format_exp exp = match exp with
  | EVar(_,(_,_,name)) -> name
  | EApp(_,f,value) -> sprintf "%s(%s)" (format_exp f) (format_exp value)
  | EFun(_,Param(_,pat,_),exp) ->
    sprintf "(lambda %s : %s)" (expand_pattern pat) (format_exp exp)
  | ELet _ -> raise (PyException "Lets should not be in finished product")
  | EAsc(_,exp,_) -> format_exp exp
  | EOver(_,_,_) ->
    raise (PyException "Overloaded Operator found during compilation")

  | EPair(_,e1,e2) -> sprintf "(%s, %s)" (format_exp e1) (format_exp e2)
  | ECase (_,e,es) ->
    let e_name = Conversion.undersc() in
    let cases = List.fold_right (build_case e_name) es "" in
    sprintf "(lambda %s : %s)(%s)" e_name cases (format_exp e)
  | EUnit(_) -> "()"
  | EInteger(_,i) -> string_of_int i
  | EChar(_,c) -> sprintf "%s" (pystring (Char.escaped c))
  | EString (_,s) -> sprintf "%s" (pystring s)
  | EBool (_,b) -> pybool b

let rec format_decl decl = match decl with
  | DLet(i, Bind(_, pat, _, exp)) ->
    (sprintf "# %s\n" (Info.string_of_t i)) ^  
    (match pat with
    | PWild(_) -> format_exp exp
    | PUnit(_) ->
        (* The assert ensures evaluation, so side effects will happen *)
        sprintf "assert %s == %s\n" "()" (format_exp exp)
    | PBool(_,b) ->
        sprintf "assert %s == %s\n" (pybool b) (format_exp exp)
    | PInteger(_,i) ->
        sprintf "assert %s == %s\n" (string_of_int i) (format_exp exp)
    | PString(_,s) ->
        sprintf "assert %s == %s\n" (pystring s) (format_exp exp)
    | PVar(_, (_,_,varname),_) ->
        sprintf "%s = %s\n" varname (format_exp exp)
    | PData(_,_,_) -> unimp()
    | PPair(_, pat1, pat2) -> unimp()
    )
  | DType(info, ids, id, _) -> unimp()


let format_modl modl = match modl with
  | Modl(_,_,decls) ->
      "from prelude import *\n" ^
    List.fold_left
        (fun file decl -> file  ^ "\n" ^ (format_decl decl))
        ""
        decls
