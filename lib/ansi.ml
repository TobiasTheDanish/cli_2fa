open Printf

let escape_char = "\x1B"

let clear_screen () = 
  printf "%s[2J" escape_char

let move_cursor_home () =
  printf "%s[H" escape_char

let move_cursor (row:int) (col:int) =
  printf "%s[%d;%dH" escape_char row col

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

let output (text:string) =
  printf "%s" text

let output_line (text:string) =
  output text;
  move_cursor_down_start 1
