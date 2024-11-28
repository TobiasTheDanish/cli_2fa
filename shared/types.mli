type command =
  | Register of string option
  | Unknown

type context = {
  cmd : command;
  verbose : bool;
}
