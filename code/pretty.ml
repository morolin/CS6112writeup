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
(* /src/compiler/pretty.ml                                                    *)
(* Pretty printer                                                             *)
(* $Id$ *)
(******************************************************************************)

(* ----- imports and abbreviations ----- *)
open Syntax
let msg = Util.format

(* ----- formatters for abstract syntax trees ----- *)  
let format_variable = function
  | VVar(_, name) -> msg "@[%s@]" name
  | VAck(_, name) -> msg "@[%s`a@]" name
  | VTrue(_, name) -> msg "@[%s`b@]" name
  | VFalse(_, name) -> msg "@[%s`c@]" name

let rec format_channel = function
  | CSend(_, name, b) -> msg "@[%s!" name;
      format_boolean b;
      msg "@]"
  | CRecv(_, name, var) -> msg "@[%s?" name;
      format_variable var;
      msg "@]"
  | CBullet(_, c1, c2) -> msg "@[";
      format_channel c1;
      msg "@ .@ ";
      format_channel c2;
      msg "@]"

and format_boolean = function
  | BVar(_, var) ->
      msg "@[";
      format_variable var;
      msg "@]"
  | BLit(_, b) -> msg "@[%b@]" b
  | BProbeRecv(_, channel) -> msg "@[#%s?@]" channel
  | BProbeSend(_, channel) -> msg "@[#%s!@]" channel
  | BAnd(_, b1, b2) -> msg "@[";
      format_boolean b1;
      msg "@ and@ ";
      format_boolean b2;
      msg "@]"
  | BOr(_, b1, b2) -> msg "@[";
      format_boolean b1;
      msg "@ or@ ";
      format_boolean b2;
      msg "@]"
  | BNot(_, b) -> msg "@[~";
      format_boolean b;
      msg "@]"

let rec format_program = function
  | PGets(_, var, b) -> msg "@[";
      format_variable var;
      msg "@ := @ ";
      format_boolean b;
      msg "@]"
  | PSelect(_, s) -> format_select s
  | PLoop(_, s) -> msg "@[*";
      format_select s;
      msg "@]"
  | PChannel(_, chan) -> msg "@[";
      format_channel chan;
      msg "@]"
  | PSeq(_, p1, p2) -> msg "@[";
      format_program p1;
      msg ";@ ";
      format_program p2;
      msg "@]"
  (* TODO(astory): reduce redundent parens *)
  | PPar(_, p1, p2) -> msg "@[(";
      format_program p1;
      msg "@ ||@ ";
      format_program p2;
      msg "@])"
  | PSkip(_) -> msg "@[skip@]"

and format_select = function
  | SDet(_, sel) -> msg "@[[";
      format_select_det sel;
      msg "]@]"
  | SNonDet(_, sel) -> msg "@[[";
      format_select_nondet sel;
      msg "]@]"

and format_select_det = function
  | SDBase(_, b, p) -> msg "@[";
      format_boolean b;
      msg "@ -> @ ";
      format_program p;
      msg "@]"
  | SDRecur(_, b, p, next) -> msg "@[";
      format_boolean b;
      msg "@ -> @ ";
      format_program p;
      msg "@ []@ @]";
      format_select_det next

and format_select_nondet = function
  | SNBase(_, b, p) -> msg "@[";
      format_boolean b;
      msg "@ -> @ ";
      format_program p;
      msg "@]"
  | SNRecur(_, b, p, next) -> msg "@[";
      format_boolean b;
      msg "@ -> @ ";
      format_program p;
      msg "@ |@ @]";
      format_select_nondet next

(* ----- conversions to string ----- *)  
let to_string = Util.format_to_string
let string_of_boolean b = to_string (fun () -> format_boolean b)
let string_of_variable v = to_string (fun () -> format_variable v)
let string_of_program p = to_string (fun () -> format_program p)
let string_of_select s = to_string (fun () -> format_select s)
let string_of_select_det s = to_string (fun () -> format_select_det s)
let string_of_select_nondet s = to_string (fun () -> format_select_nondet s)
let string_of_channel c = to_string (fun () -> format_channel c)
