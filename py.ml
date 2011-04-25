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

(* Note that this is _unsafe_.  It should only be used in circumstances where we
 * know that the types will work out - i.e., not in case statments. *)
let expand_pattern pat =
    let (s, _) = exp_pat_help (underscores 0) pat in
    s

let rec format_exp exp = match exp with
  | EVar(_,(_,_,name)) -> name
  | EApp(_,f,value) -> sprintf "%s(%s)" (format_exp f) (format_exp value)
  | EFun(_,Param(_,pat,_),exp) ->
    sprintf "(lambda %s : %s)" (expand_pattern pat) (format_exp exp)
  | ECond(_,e1,e2,e3) -> 
    sprintf "(%s if %s else %s)" (format_exp e2) (format_exp e1) (format_exp e3)
  | ELet _ -> raise (PyException "Let found during compilation")
  | EAsc(_,exp,_) -> format_exp exp
  | EOver(_,_,_) ->
    raise (PyException "Overloaded Operator found during compilation")

  | EPair(_,e1,e2) -> sprintf "(%s, %s)" (format_exp e1) (format_exp e2)
  | ECase (_,e,es) -> raise (PyException "Case found during compilation")
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

let make_import k _ rest =
  (sprintf "from prelude import %s as %s\n" k k) ^ rest

let format_modl modl = match modl with
  | Modl(_,_,decls) ->
    let imports = Conversion.StrMap.fold (make_import) Conversion.globals "" in
    imports ^
    List.fold_left
        (fun file decl -> file  ^ "\n" ^ (format_decl decl))
        ""
        decls
