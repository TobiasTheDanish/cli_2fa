type graphic_mode = 
  | Bold
  | Dim
  | Italic
  | Underline
  | Blinking
  | Reverse
  | Hidden
  | StrikeThrough
  | Reset

type color = 
  | Black
  | Red 
  | Green 
  | Yellow 
  | Blue
  | Magenta
  | Cyan 
  | White
  | Default
  | Reset

val clear_screen : unit -> unit
val clear_line : unit -> unit

val move_cursor_home: unit -> unit
val move_cursor : int -> int -> unit
val move_cursor_up : int -> unit
val move_cursor_down : int -> unit
val move_cursor_right : int -> unit
val move_cursor_left : int -> unit
val move_cursor_up_start : int -> unit
val move_cursor_down_start : int -> unit
val move_cursor_to_col : int -> unit

val output : string -> unit
val output_line : string -> unit

val set_color : graphic_mode -> color -> color -> unit
val set_graphic_mode : graphic_mode -> unit
