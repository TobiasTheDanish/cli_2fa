type command =
  | Register
  | Show
  | Ansi
  | Unknown

type context = {
  cmd : command;
  verbose : bool;
}
