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

let rec format_exp exp = match exp with
  | EVar(_,(_,_,name)) -> name
  | EApp(_,f,value) -> sprintf "%s(%s)" (format_exp f) (format_exp value)
  | EFun(_,Param(_,pat,_),exp) -> (match pat with
    | PVar(_,(_,_,varname),_) ->
      sprintf "(lambda %s : %s)" varname (format_exp exp)
    | _ -> unimp()
    )
  | ELet _ -> raise (PyException "Lets should not be in finished product")
  | EAsc(_,exp,_) -> format_exp exp
  | EOver(_,_,_) ->
      raise (PyException "Overloaded Operator found during compilation")

  | EPair(_,e1,e2) -> sprintf "(%s, %s)" (format_exp e1) (format_exp e2)
  | ECase (_,_,_) -> unimp()

  | EUnit(_) -> "()"
  | EInteger(_,i) -> string_of_int i
  | EChar(_,c) -> sprintf "\"%s\"" (Char.escaped c)
  | EString (_,s) -> sprintf "\"%s\"" s
  | EBool (_,b) -> pybool b

let rec format_decl decl = match decl with
  | DLet(_, Bind(_, pat, _, exp)) ->
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
        sprintf "assert \"%s\" == %s\n" s (format_exp exp)
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
