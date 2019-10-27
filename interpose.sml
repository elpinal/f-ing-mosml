signature INTERPOSE = sig
  type 'a t

  val interpose : 'a -> 'a t -> 'a t
end

structure Interpose = struct
  structure List = struct
    fun interpose _ []        = []
      | interpose _ [x]       = [x]
      | interpose e (x :: xs) = x :: e :: interpose e xs
  end
end
