infixr 4 <>

signature MONOID = sig
  type t

  val empty : t
  val op<> : t * t -> t
end

type 'a monoid = [MONOID where type t = 'a]

structure Monoid = struct
  structure Sum : MONOID = struct
    type t = int

    val empty = 0
    fun m <> n = m + n
  end

  structure Product : MONOID = struct
    type t = int

    val empty = 1
    fun m <> n = m * n
  end

  structure All : MONOID = struct
    type t = bool

    val empty = true
    fun x <> y = x andalso y
  end

  structure Any : MONOID = struct
    type t = bool

    val empty = false
    fun x <> y = x orelse y
  end

  structure String : MONOID = struct
    type t = string

    val empty = ""
    fun x <> y = x ^ y
  end

  functor List X : sig type t end : MONOID = struct
    type t = X.t list

    val empty = []
    fun x <> y = x @ y
  end
end
