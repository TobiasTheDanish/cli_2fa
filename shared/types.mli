type command =
  | Register 
  | Show
  | Ansi
  | Proc
  | Unknown

type context = {
  cmd : command;
  verbose : bool;
}
