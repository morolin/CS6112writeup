(* Copyright Alec Story, 2011 *)

open Syntax

exception DirectionException of string

(* Map with channel names as key *)
module ChanMap : Map.S with type key = string

(* Direction of the sender of the channel *)
type direction = Passive | Active
type node = direction * string
type dirmap = direction ChanMap.t

val label_channels : chp -> dirmap
val string_of_direction : direction -> string
val string_of_node : node -> string
