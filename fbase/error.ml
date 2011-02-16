(******************************************************************************)
(* The Frenetic Project                                                       *)
(* frenetic@frenetic-lang.org                                                 *)
(******************************************************************************)
(* Copyright (C) 2011 Cornell University                                      *)
(*                                                                            *)
(* This program is free software: you can redistribute it and/or modify       *)
(* it under the terms of the GNU General Public License version 3 as          *)
(* published by the Free Software Foundation.                                 *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(* GNU General Public License for more details.                               *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License          *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *)
(******************************************************************************)
(* /src/compiler/fbase/error.ml                                               *)
(* Error                                                                      *)
(* $Id$ *)
(******************************************************************************)

exception Frenetic_error of (unit -> unit)

let error thk = 
  raise (Frenetic_error thk)

let simple_error s = 
  error (fun () -> Util.format "%s" s)

let exit_if_error f = 
  try f () 
  with Frenetic_error thk -> 
    thk ();
    Util.format "@\n";
    exit 2
