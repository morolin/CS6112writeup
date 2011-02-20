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
(* /src/compiler/pretty.ml                                                    *)
(* Pretty printer                                                             *)
(* $Id$ *)
(******************************************************************************)

(* ----- imports and abbreviations ----- *)
open Syntax
let msg = Util.format

(* ----- formatters for abstract syntax trees ----- *)  
let rec format_type = function
  | TUnit -> msg "@[unit@]"
  | TBool -> msg "@[bool@]"
  | TInteger -> msg "@[int@]"
  | TChar    -> msg "@[char@]"
  | TString -> msg "@[string@]"
  | TProduct(t1,t2) -> 
      msg "@[<2>(";
      format_type t1;
      msg "@ *@ ";
      format_type t2;
      msg ")@]"
  | TData([],x) -> 
    msg "@[%s@]" (Id.string_of_t x)
  | TData([t],x) -> 
      msg "@[";
      format_type t;
      msg "@ ";
      msg "%s@]" (Id.string_of_t x)
  | TData(ts,x) ->         
      msg "@[<2>(@[<2>";
      Util.format_list ",@ " format_type ts;      
      msg "@])@ %s@]" (Id.string_of_t x)
  | TFunction(t1,t2) ->
      msg "@[(";
      format_type t1;
      msg "@ ->@ ";
      format_type t2;
      msg ")@]"
  | TVar(x) -> 
    msg "@['%s@]" (Id.string_of_t x)

let format_scheme = function 
  | Scheme(_,t) -> 
    msg "@[<2>(";
    format_type t;
    msg ")@]"

let rec format_pattern = function
  | PWild _ -> msg "_"
  | PUnit _ -> msg "()"
  | PInteger(_,n) -> msg "%d" n
  | PBool(_,b) -> msg "%b" b
  | PString(_,s) -> msg "%s" s
  | PVar(_,x,_) -> msg "%s" (Id.string_of_t x)
  | PPair(_,p1,p2) -> 
      msg "@[<2>(";
      format_pattern p1;
      msg ",@,";
      format_pattern p2;
      msg ")@]";
  | PData(_,x,None) -> msg "%s" (Id.string_of_t x)
  | PData(_,x,Some p1) ->  
    msg "@[<2>(%s@ " (Id.string_of_t x);
    format_pattern p1;
    msg ")@]"

and format_param p0 = match p0 with
  | Param(_,x,None) -> msg "@[%s@]" (Id.string_of_t x)
  | Param(_,x,Some t) -> 
    msg "@[(%s:" (Id.string_of_t x);
    format_type t;    
    msg ")@]"

and format_bind b0 = match b0 with
  | Bind (_,p,tyo,e) ->
      msg "@[";
      format_pattern p;
      (match tyo with  
        | None -> () 
        | Some t -> msg "@ :@ "; format_type t);
      msg "@ =@ ";
      format_exp e;
      msg "@]"
        
and format_exp e0 = match e0 with 
  | EApp (_,e1,e2) ->
      msg "@[<2>(";
      format_exp e1;
      msg "@ ";
      format_exp e2;
      msg ")@]"

  | EVar(_,x) -> 
      msg "@[%s@]" (Id.string_of_t x)
	
  | EFun (_,p,tyo,e) ->
      msg "@[<2>(fun@ ";
      format_param p;
      (match tyo with
	 | None -> ()
         | Some s -> msg "@ :@ "; format_type s);
      msg "@ ->@ ";
      format_exp e;
      msg ")@]";
      
  | ELet (_,b,e) ->
      msg "@[<2>let ";
      format_bind b;
      msg "@ in@ ";
      format_exp e;
      msg "@]";
  | EOver (_,o,[e1;e2]) -> 
    msg "@[<2>(";
    format_exp e1;
    msg "@ ";
    format_op o;
    msg "@ ";
    format_exp e2;
    msg ")@]"
    
  | EOver _ -> 
    Error.simple_error "Cannot pretty print ill-formed EOver expression" 

  | EPair(_,e1,e2) -> 
      msg "@[<2>(@[";
      format_exp e1;
      msg ",@,";
      format_exp e2;
      msg "@])@]"
	
  | ECase(_,e1,bs) -> 
      msg "@[<2>(match@ ";
      format_exp e1;
      msg "@ with@ ";
      Util.format_list "@ | "
        (fun (p,e) -> 
           msg "@[<2>";
           format_pattern p;
           msg "@ ->@ ";
           format_exp e;
           msg "@]")
        bs;
      msg ")@]"

  | EUnit _ -> 
    msg "()"
      
  | EInteger (_,i) ->
    msg "@[%d@]" i

  | EChar(_,c) -> 
    msg "'%s'" (Char.escaped c)

  | EString (_,s) ->
    msg "@[\"%s\"@]" (String.escaped s)

  | EBool (_,b) -> 
      msg "@[%b@]" b

and format_op = function
  | OSemi -> msg ";"
  | OMinus  -> msg "-"
  | OEqual  -> msg "="
  | OLt     -> msg "<"
  | OLeq     -> msg "<="
  | OGt     -> msg ">"
  | OGeq    -> msg ">="

and format_decl = function
    DLet (_,b) ->
      msg "@[<2>let ";
      format_bind b;
      msg "@]"

  | DType(_,vs,x,cs) ->  
      msg "@[<2>type@ ";
      (match vs with 
         | [] -> ()
         | [x] -> msg "%s" (Id.string_of_t x)
         | _ -> 
             msg "(";
             Util.format_list ",@ " (fun xi -> msg "%s" (Id.string_of_t xi)) vs;
             msg ")");
      msg "@ %s@ =@ " (Id.string_of_t x);
      Util.format_list " | "
        (fun (l,tyo) -> match tyo with
           | None -> 
             msg "%s" (Id.string_of_t l)
           | Some t -> 
             msg "(%s@ " (Id.string_of_t l);
             format_type t;
             msg ")")
        cs;
      msg "@]"        

and format_modl = function
  | Modl (_,m,ds) ->
      msg "@[module %s =@\n  @[" (Id.string_of_t m);
      Util.format_list "@\n" format_decl ds;
      msg "@\n@]@\n@]"

(* ----- conversions to string ----- *)  
let to_string = Util.format_to_string
let string_of_exp e = to_string (fun () -> format_exp e)
let string_of_bind b = to_string (fun () -> format_bind b)
let string_of_decl d = to_string (fun () -> format_decl d)
let string_of_op o = to_string (fun () -> format_op o)
let string_of_modl m = to_string (fun () -> format_modl m)
let string_of_type s = to_string (fun () -> format_type s)
let string_of_param p = to_string (fun () -> format_param p)
let string_of_pattern p = to_string (fun () -> format_pattern p)
