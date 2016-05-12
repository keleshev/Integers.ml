let (>>>), (=>) = Test.((test), (=>))

open Integers.Std

let assert_fails ~f = try ignore (f ()); assert false with Failure _ -> ()

module TestUInt8 = struct
  module Wrapping = UInt8.Wrapping
  let min, max = UInt8.(of_int_exn min_value, of_int_exn max_value)


  let () = "Int conversion" >>> fun () ->
    UInt8.of_int 5 => Some (u8 5);
    UInt8.of_int 999 => None;
    UInt8.of_int_exn 5 => (u8 5);
    UInt8.to_int (u8 5) => 5;
    assert_fails ~f:(fun () -> UInt8.of_int_exn 999)

  let () = "Float conversion" >>> fun () ->
    UInt8.of_float 5.5 => Some (u8 5);
    UInt8.of_float 999.0 => None;
    UInt8.of_float_exn 5.5 => (u8 5);
    UInt8.to_float (u8 5) => 5.0;
    assert_fails ~f:(fun () -> UInt8.of_float_exn 999.0)

  let () = "String conversion" >>> fun () ->
    UInt8.of_string "5" => Some (u8 5);
    UInt8.of_string "X" => None;
    UInt8.of_string_exn "5" => (u8 5);
    UInt8.to_string (u8 5) => "5";
    assert_fails ~f:(fun () -> UInt8.of_string_exn "X")

  let () = "Plus operators" >>> fun () ->
    UInt8.(u8 5 +? u8 5) => Some (u8 10);
    UInt8.(u8 5 +! u8 5) => (u8 10);
    UInt8.(u8 200 +? u8 200) => None;
    assert_fails ~f:(fun () -> UInt8.(u8 200 +! u8 200))

  let () = "Minus operators" >>> fun () ->
    UInt8.(u8 10 -? u8 5) => Some (u8 5);
    UInt8.(u8 10 -! u8 5) => u8 5;
    assert_fails ~f:(fun () -> UInt8.(u8 5 -! u8 200))

  let () = "Operators that return UInt16" >>> fun () ->
    UInt8.(u8 200 + u8 200) => u16 400;
    UInt8.(u8 200 * u8 200) => u16 40000;
    UInt8.to_uint16 (u8 5) => u16 5

  let () = "Set bit" >>> fun () ->
    UInt8.set_bit (u8 0b00010000) (u3 0) (u1 1) => u8 0b00010001;
    UInt8.set_bit (u8 0b00010001) (u3 0) (u1 1) => u8 0b00010001;
    UInt8.set_bit (u8 0b00010000) (u3 0) (u1 0) => u8 0b00010000;
    UInt8.set_bit (u8 0b00010001) (u3 0) (u1 0) => u8 0b00010000;
    UInt8.set_bit (u8 0b00010000) (u3 1) (u1 1) => u8 0b00010010;
    UInt8.set_bit (u8 0b00010000) (u3 7) (u1 1) => u8 0b10010000

  let () = "Get bit" >>> fun () ->
    UInt8.bit (u8 0b00000000) (u3 0) => u1 0;
    UInt8.bit (u8 0b00000001) (u3 0) => u1 1;
    UInt8.bit (u8 0b00000000) (u3 3) => u1 0;
    UInt8.bit (u8 0b00001000) (u3 3) => u1 1;
    UInt8.bit (u8 0b11011111) (u3 5) => u1 0;
    UInt8.bit (u8 0b11111111) (u3 5) => u1 1

  let () = "Wrapping operations" >>> fun () ->
    Wrapping.(+) max (u8 1) => min;
    Wrapping.(-) min (u8 1) => max;
    Wrapping.( * ) max max => u8 1;
    Wrapping.( * ) max (u8 2) => u8 254
end

module TestUInt16 = struct
  let () = "From/to bytes" >>> fun () ->
    UInt16.of_byte_tuple ('\x03', '\x05') => u16 (3 * 256 + 5);
    UInt16.(to_byte_tuple (u16 1234)) => ('\x04', '\xD2');
    UInt16.to_bytes (u16 1234) => "\x04\xD2";
    UInt16.of_bytes "\x04" => Some (u16 4);
    UInt16.of_bytes "\x04\xD2" => Some (u16 1234);
    UInt16.of_bytes "\x04\xD2\xFF" => None

  let () = "From/to UInt8" >>> fun () ->
    UInt16.of_uint8 (UInt8.of_byte '\x05') => u16 5;
    UInt16.(to_uint8_tuple (u16 1234)) =>
      UInt8.(of_byte '\x04', of_byte '\xD2')

  let () = "/* operators" >>> fun () ->
    UInt16.(u16 10000 /*? u16 2048) => Some (u16 8192);
    UInt16.(u16 0 /*? u16 12345) => Some (u16 0);
    UInt16.(u16 10000 /*? u16 0) => None
end

module TestInt8 = struct
  let () = "Cast to UInt8" >>> fun () ->
    let i8_to_u8 = [127, 127;
                    126, 126;
                      2,   2;
                      1,   1;
                      0,   0;
                     -1, 255;
                     -2, 254;
                   -126, 130;
                   -127, 129;
                   -128, 128] in
    i8_to_u8 |> List.iter (fun (left, right) ->
      Int8.Cast.to_uint8 (i8 left) => u8 right)

  let () = "Wrapping is undefined for signed integers" >>> fun () ->
    (* TODO: I wish this was a compile-time error instead *)
    assert_fails ~f:(fun () -> Int8.Wrapping.(i8 1 + i8 2))
end

module TestUInt1 = struct
  let () = "UInt1 of boolean" >>> fun () ->
    UInt1.of_bool true  => u1 1;
    UInt1.of_bool false => u1 0
end
