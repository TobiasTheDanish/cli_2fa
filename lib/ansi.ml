open Printf

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

let mode_to_int = function
  | Bold -> 1
  | Dim -> 2
  | Italic -> 3
  | Underline -> 4
  | Blinking -> 5
  | Reverse -> 7
  | Hidden -> 8
  | StrikeThrough -> 9
  | Reset -> 0

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

let fg_color_to_int = function
  | Black   -> 30
  | Red     -> 31
  | Green   -> 32
  | Yellow  -> 33
  | Blue    -> 34
  | Magenta -> 35
  | Cyan    -> 36
  | White   -> 37
  | Default -> 39
  | Reset   -> 0

let bg_color_to_int = function
  | Black   -> 40
  | Red     -> 41
  | Green   -> 42
  | Yellow  -> 43
  | Blue    -> 44
  | Magenta -> 45
  | Cyan    -> 46
  | White   -> 47
  | Default -> 49
  | Reset   -> 0

let escape_char = "\x1B"

let clear_screen () = 
  printf "%s[2J" escape_char

let move_cursor_home () =
  printf "%s[H" escape_char

let move_cursor (row:int) (col:int) =
  printf "%s[%d;%df" escape_char row col

let move_cursor_up (rows:int) = 
  printf "%s[%dA" escape_char rows

let move_cursor_down (rows:int) = 
  printf "%s[%dB" escape_char rows

let move_cursor_right (cols:int) = 
  printf "%s[%dC" escape_char cols

let move_cursor_left (cols:int) = 
  printf "%s[%dD" escape_char cols

let move_cursor_up_start (rows:int) = 
  printf "%s[%dF" escape_char rows

let move_cursor_down_start (rows:int) = 
  printf "%s[%dE" escape_char rows

let move_cursor_to_col (col:int) =
  printf "%s[%dG" escape_char col

let output (text:string) =
  printf "%s" text

let output_line (text:string) =
  output text;
  move_cursor_down_start 1

let set_color (mode:graphic_mode) (fg:color) (bg:color) =
  printf "%s[%d;%d;%dm" escape_char (mode_to_int mode) (fg_color_to_int fg) (bg_color_to_int bg)

let set_graphic_mode (mode:graphic_mode) = 
  printf "%s[%dm" escape_char (mode_to_int mode)
