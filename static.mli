(* Copyright Alec Story, 2011 *)

open Syntax

type short_variable =
  | SVar of string
  | SAck of string
  | STrue of string
  | SFalse of string

module VarSet : Set.S with type elt = short_variable

exception TypeException of (Info.t * string)

val read_write_sets : program -> (VarSet.t * VarSet.t)
(** compute the sets of variables a program reads from, and the set of variables
** it writes to.  If it would start a fire, throw a type exception *)

val nofires : program -> unit
(** Try to prove that program will not start fires, otherwise, throw a type
 ** exception. *)

val non_interfering : program -> program -> bool
(** Test if two programs cannot interfere with each other through shared,
 ** non-channel variables.  Will throw an exception if either program is a fire
 ** hazard, and if the two are fire hazards if composed in parallel. *)
