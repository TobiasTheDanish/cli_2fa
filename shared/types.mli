type command =
  | Register of string option
  | Show
  | Ansi
  | Unknown

type context = {
  cmd : command;
  verbose : bool;
}
