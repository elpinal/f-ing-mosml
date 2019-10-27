infixr 4 <+>

structure Pretty : sig
  type t = string

  val <+> : t * t -> t

  val brace : t -> t
  val paren : bool -> t -> t
end = struct
  open Monoid.String

  type t = string

  fun op<+> (x, y) = x <> " " <> y

  fun brace s = "{" <> s <> "}"

  fun paren true s  = "(" <> s <> ")"
    | paren false s = s
end
