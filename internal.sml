infixr :->

infix |>

fun x |> f = f x

infixr $

fun f $ x = f x

fun print_endline s = s ^ "\n" |> print

structure Label :> sig
  include ORD

  val from_string : string -> t

  val show : t -> string
end = struct
  type t = string

  val compare = String.compare

  fun from_string s = s

  fun show s = s
end

val label = Label.from_string

structure R = Map(Label)

structure Syntax = struct
  local
    open List Pretty
  in
    datatype kind
      = @*
      | :-> of kind * kind

    fun show_kind k =
    let
      open Monoid.String

      fun loop _ @*          = "Ω"
        | loop n (k1 :-> k2) = paren (2 < n) $ loop 3 k1 <+> "->" <+> loop 2 k2
    in
      loop 0 k
    end

    signature TYPE = sig
      datatype 'a t
        = Var of 'a
        | Abs of kind * ('a -> 'a t)
        | App of 'a t * 'a t
        | Arrow of 'a t * 'a t
        | Record of 'a t R.t
        | Forall of kind * ('a -> 'a t)
        | Exist of kind * ('a -> 'a t)

      signature TQ = sig
        val v : 'a t
      end

      type tq = [TQ]

      val from_tq : tq -> 'a t

      val show : string t -> string
    end

    structure Type : TYPE = rec (X : TYPE) struct
      datatype t = datatype X.t

      signature TQ = sig
        val v : 'a t
      end

      type tq = X.tq

      fun from_tq tq = let structure M as TQ = tq in M.v end

      fun show ty0 =
      let
        open Monoid.String

        val r = ref 0

        fun show_var n = "t" <> Int.toString n

        fun with_inc f =
        let
          val n = !r
          val () = r := n + 1
        in
          f n
        end

        fun loop _ (Var(s))          = s
          | loop m (Abs(k, f))       = with_inc (fn n => paren (0 < m) $ "λ" <> show_var n <+> ":" <+> show_kind k <> "." <+> loop 0 (show_var n |> f))
          | loop m (Forall(k, f))    = with_inc (fn n => paren (0 < m) $ "∀" <> show_var n <+> ":" <+> show_kind k <> "." <+> loop 0 (show_var n |> f))
          | loop m (Exist(k, f))     = with_inc (fn n => paren (0 < m) $ "∃" <> show_var n <+> ":" <+> show_kind k <> "." <+> loop 0 (show_var n |> f))
          | loop n (App(ty1, ty2))   = paren (4 < n) $ loop 4 ty1 <+> loop 5 ty2
          | loop n (Arrow(ty1, ty2)) = paren (2 < n) $ loop 3 ty1 <+> "->" <+> loop 2 ty2
          | loop _ (Record(m))       =
          let
            fun f l ty = Label.show l <> ":" <+> loop 0 ty
          in
            R.map_with_key f m
              |> R.to_list
              |> Interpose.List.interpose ", "
              |> Foldable.List.fold [structure Monoid.String as MONOID where type t = string]
              |> brace
          end
      in
        loop 0 ty0
      end
    end
  end
end

local
  open Syntax
  open Type

  val print_show = print_endline o show

  fun f x = R.empty
    |> R.insert (label "c") (App(Var x, Var x))
    |> R.insert (label "b") (Abs(@* :-> ((@* :-> @*) :-> @*) :-> @*, fn y => App(Var y, Var x)))
    |> Record

  fun g x = App(App(Var x, Var x), App(Var x, Var x))

  fun h x = Arrow(App(Var x, Arrow(Var x, Var x)), App(Arrow(Var x, Var x), Var x))
in
  val () = List.app print_show
    [ Abs(@*, f)
    , Abs(@*, g)
    , Forall(@*, g)
    , Exist(@*, g)
    , Abs(@* :-> @*, h)
    , Abs(@*, fn x => App(Abs(@*, fn y => Var y), Abs(@*, fn y => Var y)))
    ]
end
