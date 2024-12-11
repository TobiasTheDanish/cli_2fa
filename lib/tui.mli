type event =
  | Key of char

val loop : 'a -> ('a -> float -> unit) -> ('a -> float -> event list -> 'a) -> unit

open Riot

type custom_time = float

type modifier = No_modifier | Ctrl

val modifier_to_string : modifier -> string

type key = 
  | Up
  | Down
  | Left
  | Right
  | Space
  | Escape
  | Backspace
  | Enter
  | Key of string

val key_to_string : key -> string

type riot_event = 
  | KeyDown of key * modifier
  | Frame of custom_time
  | Unknown of Message.t

val loop_riot : 'a -> ('a -> unit) -> ('a -> riot_event -> 'a) -> unit
