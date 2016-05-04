let (>>) f g x = g (f x)

module Escape = struct
  let codes codes =
    if codes = [] then "" else
    let strings = List.map string_of_int codes in
    "\027[" ^ String.concat ";" strings ^ "m"

  let code code = "\027[" ^ (string_of_int code) ^ "m"
end

let escape = Escape.code

module Text = struct
  let reset = escape 0

  module Mode = struct
    type t =
      | Bold
      | Underlined
      | Blinking
      | Reverse
      | Concealed

    let to_code = function
      | Bold       -> 1
      | Underlined -> 4
      | Blinking   -> 5
      | Reverse    -> 7
      | Concealed  -> 8

    let escape = to_code >> escape

    let render t text = escape t ^ text ^ reset

    let bold       = render Bold
    let underlined = render Underlined
    let blinking   = render Blinking
    let reverse    = render Reverse
    let concealed  = render Concealed
  end

  module Color = struct
    type t =
      | Black
      | Red
      | Green
      | Yellow
      | Blue
      | Magenta
      | Cyan
      | White

    let to_code = function
      | Black   -> 30
      | Red     -> 31
      | Green   -> 32
      | Yellow  -> 33
      | Blue    -> 34
      | Magenta -> 35
      | Cyan    -> 36
      | White   -> 37

    module Background = struct
      let to_code = to_code >> (+) 10

      let escape = to_code >> escape

      let render color text = escape color ^ text ^ reset
    end

    let escape = to_code >> escape

    let render color text = escape color ^ text ^ reset

    let black   = render Black
    let red     = render Red
    let green   = render Green
    let yellow  = render Yellow
    let blue    = render Blue
    let magenta = render Magenta
    let cyan    = render Cyan
    let white   = render White
  end

  module Style = struct
    type t =
      | Foreground of Color.t
      | Background of Color.t
      | Mode of Mode.t

    type _style = t

    let to_code = function
      | Foreground color -> Color.to_code color
      | Background color -> Color.Background.to_code color
      | Mode mode -> Mode.to_code mode

    let escape = to_code >> escape

    let render style text = escape style ^ text ^ reset

    module Set = struct
      type t = _style list

      let escape = List.map to_code >> Escape.codes

      let render list text = escape list ^ text ^ reset
    end
  end
end

module Template = struct
  type 'a t =
    | Var of 'a
    | Text of string
    | Styled of Text.Style.Set.t * 'a t
    | Join of 'a t * 'a t

  module Compiled = struct
    module Item = struct
      type 'a t =
        | Var of 'a
        | Text of string
    end

    type 'a t = 'a Item.t list
  end

  let text source = Text source

  let var label = Var label

  let rec render ?(vars=fun _ -> assert false) ?(context=[]) = function
    | Text source -> source
    | Var label -> vars label
    | Styled (styles, template) ->
        let restored = Text.Style.Set.escape context in
        let source = render template ~vars ~context:styles in
        Text.Style.Set.render styles source ^ restored
    | Join (left, right) ->
        render left ~vars ~context ^ render right ~vars ~context

  let print ?(vars=fun _ -> assert false) template =
    print_string (render template ~vars)

  let compile template = template (* TODO *)

  let (^) left right = Join (left, right)

  let _foreground color template =
    Styled ([Text.Style.Foreground color], template)

  let black   template = _foreground Text.Color.Black   template
  let red     template = _foreground Text.Color.Red     template
  let green   template = _foreground Text.Color.Green   template
  let yellow  template = _foreground Text.Color.Yellow  template
  let blue    template = _foreground Text.Color.Blue    template
  let magenta template = _foreground Text.Color.Magenta template
  let cyan    template = _foreground Text.Color.Cyan    template
  let white   template = _foreground Text.Color.White   template

  let _background color template =
    Styled ([Text.Style.Background color], template)

  let on_black   template = _background Text.Color.Black   template
  let on_red     template = _background Text.Color.Red     template
  let on_green   template = _background Text.Color.Green   template
  let on_yellow  template = _background Text.Color.Yellow  template
  let on_blue    template = _background Text.Color.Blue    template
  let on_magenta template = _background Text.Color.Magenta template
  let on_cyan    template = _background Text.Color.Cyan    template
  let on_white   template = _background Text.Color.White   template
end
