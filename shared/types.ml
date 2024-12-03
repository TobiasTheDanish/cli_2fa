type command =
  | Register of string option
  | Show
  | Unknown

type context = {
  cmd : command;
  verbose : bool;
}
