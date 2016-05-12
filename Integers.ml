module Private = struct
  let fail format =
    Printf.ksprintf (fun message () -> failwith message) format

  module Char = struct
    let to_int = Char.code
    let of_int_exn = Char.chr
    let to_string = String.make 1
  end

  module Int = struct
    let (+), (-), (/), ( * ) = (+), (-), (/), ( * )
    let of_float = int_of_float
    let to_float = float_of_int
    let of_string = int_of_string
    let to_string = string_of_int
    let bit_and a b = a land b

    let is_even n = n mod 2 = 0

    (* https://en.wikipedia.org/wiki/Exponentiation_by_squaring *)
    let pow base exponent =
      if exponent < 0 then invalid_arg "exponent can not be negative" else
      let rec aux accumulator base = function
        | 0 -> accumulator
        | 1 -> base * accumulator
        | e when is_even e -> aux accumulator (base * base) (e / 2)
        | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
      aux 1 base exponent
  end

  module Int64 = struct
    include Int64

    let (+), (-), (/), ( * ) = add, sub, div, mul
    let to_int_exn = to_int
    let to_int = `Use_to_int_exn
  end

  module String = struct
    include String

    let to_list string =
      let rec aux i list =
        if i = 0 then list else aux (i - 1) (string.[i - 1] :: list) in
      aux (length string) []
  end

  let id x = x

  let unwrap value ~or_else = match value with
    | Some s -> s
    | None -> or_else ()
end

open Private

module Spec = struct
  module type Signature = sig
    val type_name: string
    val bit_width: int
    val is_signed: bool
  end
end

module type Signature = sig
  type t

  val type_name: string
  val bit_width: int
  val is_signed: bool

  val cardinality: int

  val max_value: int
  val min_value: int

  val of_int: int -> t option
  val of_int_exn: int -> t
  val to_int: t -> int

  val of_float: float -> t option
  val of_float_exn: float -> t
  val to_float: t -> float

  val of_string: string -> t option
  val of_string_exn: string -> t
  val to_string: t -> string

  val (+): [`Use_plus_question_or_plus_bang_operators_instead]
  val (+?): t -> t -> t option
  val (+!): t -> t -> t

  val (-): [`Use_minus_question_or_minus_bang_operators_instead]
  val (-?): t -> t -> t option
  val (-!): t -> t -> t

  val (/): [`Use_slash_question_or_slash_bang_operators_instead]
  val (/?): t -> t -> t option
  val (/!): t -> t ->t

  val bit_and: t -> t -> t

  val zero: t
  val one: t
  val to_int64: t -> Int64.t

  val (/* ): [`Use_slash_star_question_or_slash_star_bang_operators_instead]
  val (/*?): t -> t -> t option
  val (/*!): t -> t -> t

  val (<): t -> t -> bool
  val (<=): t -> t -> bool

  module Wrapping: sig
    val (+): t -> t -> t
    val (-): t -> t -> t
    val ( * ): t -> t -> t
  end
end

module Make (Spec: Spec.Signature): Signature = struct
  type t = int

  let type_name = Spec.type_name
  let bit_width = Spec.bit_width
  let is_signed = Spec.is_signed

  let cardinality = Int.pow 2 bit_width

  let max_value = cardinality / (if is_signed then 2 else 1) - 1
  let min_value = if is_signed then max_value - cardinality - 1 else 0

  let of_int n = if n > max_value || n < min_value then None else Some n
  let of_int_exn i = unwrap (of_int i)
    ~or_else:(fail "%s.of_int_exn got int out of range: %i" Spec.type_name i)
  let to_int = id

  let of_float f = of_int (Int.of_float f)
  let of_float_exn f = unwrap (of_float f)
    ~or_else:(fail "%s.of_float_exn got float out of range: %f"
                    Spec.type_name f)
  let to_float = Int.to_float

  let of_string s = try Some (Int.of_string s) with | Failure _ -> None
  let of_string_exn s = unwrap (of_string s)
    ~or_else:(fail "%s.of_string_exn cannot parse: %S" Spec.type_name s)
  let to_string = Int.to_string

  let _infix op left right = of_int (op (to_int left) (to_int right))

  let (+) = `Use_plus_question_or_plus_bang_operators_instead
  let (+?) = _infix Int.(+)
  let (+!) left right = unwrap (left +? right)
    ~or_else:(fail "%s.(+!) result is out of bounds" Spec.type_name)

  let (-) = `Use_minus_question_or_minus_bang_operators_instead
  let (-?) = _infix Int.(-)
  let (-!) left right = unwrap (left -? right)
    ~or_else:(fail "%s.(-!) result is out of bounds" Spec.type_name)

  let (/) = `Use_slash_question_or_slash_bang_operators_instead
  let (/?) = _infix Int.(/)
  let (/!) left right = unwrap (left /? right)
    ~or_else:(fail "%s.(/!) result is out of bounds" Spec.type_name)

  let bit_and = Int.bit_and

  (* exn: 0 and 1 are representable in all int types including u1, except i1 *)
  let zero = of_int_exn 0
  let one =  of_int_exn 1

  let to_int64 t = t |> to_int |> Int64.of_int

  let (/* ) = `Use_slash_star_question_or_slash_star_bang_operators_instead
  let (/*?) left right =
    if right = zero then None else
    let left, right = to_int64 left, to_int64 right in
    (* exn /: integer division is avoided in first-line guard clause *)
    (* exn *: overflow is avoided by promoting to Int64 *)
    (* exn to_int_exn: result of u16 / u16 * u16 fits inside u16 *)
    Int64.(left / right * right |> to_int_exn) |> of_int

  let (/*!) left right = unwrap (left /*? right)
    ~or_else:(fail "%s.(/*!) division by zero" Spec.type_name)

  let (<) = (<)
  let (<=) = (<=)

  module Wrapping = struct
    let _infix operator left right =
      if is_signed then fail "Wrapping undefined for signed integers" () else
      let left, right = to_int left, to_int right in
      (* exn: `mod cardinality` is always within range *)
      of_int_exn ((Int.(+) cardinality (operator left right)) mod cardinality)

    let (+) = _infix Int.(+)
    let (-) = _infix Int.(-)
    let ( * ) = _infix Int.( * )
  end
end

module Almost = struct
  (*
   * Modules `Std.UInt8` and `Std.UInt16` need to know about
   * each other to implement (for example) `Std.UInt8.to_uint16`
   * and `Std.UInt16.of_uint8`, but we want to avoid verbose
   * recursive modules, so we factor out independent parts of
   * these modules into `Almost.UInt8` and `Almost.UInt16`.
   *)

  module Int8 = struct
    include Make (struct
      let type_name = "Int8"
      let bit_width = 8
      let is_signed = true
    end)
  end

  module UInt1 = struct
    include Make (struct
      let type_name = "UInt1"
      let bit_width = 1
      let is_signed = false
    end)
  end

  module UInt3 = struct
    include Make (struct
      let type_name = "UInt3"
      let bit_width = 3
      let is_signed = false
    end)
  end

  module UInt8 = struct
    include Make (struct
      let type_name = "UInt8"
      let bit_width = 8
      let is_signed = false
    end)

    (* exn: `Char.to_int` returns `0..255`, so `UInt8.of_int_exn` is safe *)
    let of_byte (byte: char): t = of_int_exn (Char.to_int byte)
  end

  module UInt11 = struct
    include Make (struct
      let type_name = "UInt11"
      let bit_width = 11
      let is_signed = false
    end)
  end

  module UInt16 = struct
    include Make (struct
      let type_name = "UInt16"
      let bit_width = 16
      let is_signed = false
    end)

    let of_byte_tuple (high, low) =
      let high, low = Char.to_int high, Char.to_int low in
      (* exn: two consecutive bytes fit inside a single UInt16 *)
      of_int_exn (Int.(+) (high lsl 8) low)

    let of_bytes bytes = match String.to_list bytes with
      | [low]       -> Some (of_byte_tuple ('\x00', low))
      | [high; low] -> Some (of_byte_tuple (  high, low))
      | _           -> None

    let of_bytes_exn bytes = unwrap (of_bytes bytes)
      ~or_else:(fail "%s.of_bytes_exn got bytes out of range: %S" type_name
                                                                  bytes)

    (* exn: shift/mask make sure that value is at most 255 *)
    let lower_byte t = Char.of_int_exn (to_int t lsr 8)
    let upper_byte t = Char.of_int_exn (to_int t land 0b00000000_11111111)

    let lower t = lower_byte t |> UInt8.of_byte
    let upper t = upper_byte t |> UInt8.of_byte

    let to_byte_tuple t = lower_byte t, upper_byte t

    let to_uint8_tuple t = lower t, upper t

    let to_bytes t =
      let high, low = to_byte_tuple t in
      Char.to_string high ^ Char.to_string low
  end
end

module Std = struct
  module Int8 = struct
    include Almost.Int8

    module Cast = struct
      let to_uint8 t =
        let i = to_int t in
        Almost.UInt8.of_int_exn (if i >= 0 then i else Int.(+) i 256)
    end
  end

  module UInt1 = struct
    include Almost.UInt1

    (* TODO flip for all types *)

    (* exn: single bit xor will not affect other bits *)
    let flip t = of_int_exn (to_int t lxor 1)

    let of_bool bool = if bool then one else zero
    let to_bool bit = (bit = one)
  end

  module UInt3 = struct
    include Almost.UInt3
  end

  module UInt16 = struct
    include Almost.UInt16

    (* exn: UInt8.t is guaranteed to fit in UInt16.t *)
    let of_uint8 uint8 = of_int_exn (Almost.UInt8.to_int uint8)

    (* exn: 2x UInt8 = UInt16 *)
    let of_uint8_tuple (high, low) =
      let high, low = Almost.UInt8.to_int high, Almost.UInt8.to_int low in
      of_int_exn ((high lsl 8) lor low)

    let to_uint8_tuple t =
      let high, low = Almost.UInt16.to_byte_tuple t in
      Almost.UInt8.(of_byte high, of_byte low)
  end

  module UInt11 = struct
    include Almost.UInt11

    let to_uint16 t =
      (* exn: UInt11.t fits inside UInt16.t *)
      UInt16.of_int_exn (to_int t)
  end

  module UInt8 = struct
    include Almost.UInt8

    (* exn: sum/product of two UInt8 always fits inside UInt16 *)
    let (+) left right =
      UInt16.of_int_exn (Int.(+) (to_int left) (to_int right))
    let ( * ) left right =
      UInt16.of_int_exn (Int.( * ) (to_int left) (to_int right))

    (* exn: safe because UInt8 fits inside UInt16 *)
    let to_uint16 t = UInt16.of_int_exn (to_int t)

    let set_bit t (position: UInt3.t) (bit: UInt1.t) =
      let mask = Int.pow 2 (UInt3.to_int position) in
      let i = to_int t in
      (* exn: TODO hard to explain... *)
      of_int_exn (if bit = UInt1.one then i lor mask
                                     else i lor mask lxor mask)

    let bit t (position: UInt3.t) =
      let position = UInt3.to_int position in
      let i = to_int t in
      (* exn: TODO hard to explain... *)
      UInt1.of_int_exn (i lsr position land 1)
  end

  let i8 = Int8.of_int_exn

  let u16 = UInt16.of_int_exn
  let u11 = UInt11.of_int_exn
  let u8 = UInt8.of_int_exn
  let u3 = UInt3.of_int_exn
  let u1 = UInt1.of_int_exn
end
