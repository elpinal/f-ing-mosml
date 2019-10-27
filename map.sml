signature ORD = sig
  type t
  val compare : t * t -> order
end

functor Compare (X : ORD) : sig
  include ORD where type t = X.t

  val op< : t * t -> bool
end = struct
  open X

  fun x < y =
    case compare (x, y) of
         LESS => true
       | _    => false
end

signature MAP = sig
  include FOLDABLE

  type key

  exception NotFound of key

  val empty : 'a t
  val insert : key -> 'a -> 'a t -> 'a t
  val lookup : key -> 'a t -> 'a
  val member : key -> 'a t -> bool

  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_with_key : (key -> 'a -> 'b) -> 'a t -> 'b t
end

(*
* 'Map' is implemented as a generative functor because Moscow ML's applicative
* functors are not "abstraction-safe".
*)
functor Map (X : ORD) :> MAP where type key = X.t = struct
  type key = X.t

  exception NotFound of key

  structure C = Compare(X)
  open C

  datatype 'a t
    = E
    | T of 'a t * key * 'a * 'a t

  val empty = E

  fun insert k v t =
  let
    fun f E = T(E, k, v, E)
      | f (T(l, k0, v0, r)) =
      if k < k0
      then T(f l, k0, v0, r)
      else
        if k0 < k
        then T(l, k0, v0, f r)
        else T(l, k, v, r)
  in
    f t
  end

  fun lookup j E = raise NotFound(j)
    | lookup j (T(l, k, v, r)) =
    if j < k
    then lookup j l
    else
      if k < j
      then lookup j r
      else v

  fun member k t =
    let val _ = lookup k t in
      true handle NotFound(_) => false
    end

  fun map _ E               = E
    | map f (T(l, k, v, r)) =
    let
      val l = map f l
      val v = f v
      val r = map f r
    in
      T(l, k, v, r)
    end

  fun map_with_key _ E               = E
    | map_with_key f (T(l, k, v, r)) =
    let
      val l = map_with_key f l
      val v = f k v
      val r = map_with_key f r
    in
      T(l, k, v, r)
    end

  local
    structure F = struct
      type 'a t = 'a t

      fun fold_map m f =
      let
        structure M as MONOID where type t = 'b = m
        open M
        fun loop E               = empty
          | loop (T(l, _, v, r)) =
          let
            val l = loop l
            val v = f v
            val r = loop r
          in
            l <> v <> r
          end
      in
        loop
      end
    end

    structure M = Foldable.Make(F)
  in
    open M
  end
end
