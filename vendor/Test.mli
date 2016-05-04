val (=>): 'a -> 'a -> unit
val test: string -> (unit -> unit) -> unit

module Assertion: sig
  val create: ('a -> Core.Std.Sexp.t) -> ('a -> 'a -> unit)
end
