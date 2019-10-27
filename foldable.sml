signature FOLDABLE_MINIMAL = sig
  type 'a t

  val fold_map : 'b monoid -> ('a -> 'b) -> 'a t -> 'b
end

signature FOLDABLE = sig
  include FOLDABLE_MINIMAL

  val fold : 'a monoid -> 'a t -> 'a

  val null : 'a t -> bool
  val length : 'a t -> int

  val to_list : 'a t -> 'a list
end

structure Foldable = struct
  functor Make (X : FOLDABLE_MINIMAL) = struct
    open X

    fun fold m = fold_map m (fn x => x)

    fun null xs =
      fold_map [structure Monoid.All as MONOID where type t = bool] (fn _ => false) xs

    fun length xs =
      fold_map [structure Monoid.Sum as MONOID where type t = int] (fn _ => 1) xs

    fun to_list xs =
    let
      structure M = struct
        type t = 'a
      end
    in
      fold_map [structure Monoid.List(M) as MONOID where type t = M.t list] (fn x => [x]) xs
    end
  end

  structure List : FOLDABLE = Make(struct
    type 'a t = 'a list

    fun fold_map m f =
    let
      structure M as MONOID where type t = 'b = m
      open M
      fun loop []        = empty
        | loop (x :: xs) = f x <> loop xs
    in
      loop
    end
  end)
end
