let (=>), (>>>) = Test.((=>), test)

module Template = Terminal.Template

let _ = "Render text" >>> fun _ ->
  Template.(render (text "foo")) => "foo"

let _ = "Render variable" >>> fun _ ->
  Template.(render (var `foo) ~vars:(function `foo -> "bar")) => "bar"

let _ = "Render color" >>> fun _ ->
  Template.(render (red   (text "  red  "))) => "\027[31m  red  \027[0m";
  Template.(render (green (text " green "))) => "\027[32m green \027[0m"

let _ = "Template concatenation" >>> fun _ ->
  Template.(render (red (text " red ") ^ text " white "))
    => "\027[31m red \027[0m white "

let _ = "Render background" >>> fun _ ->
  Template.(render (red (on_green (text "*"))))
    => "\027[31m\027[42m*\027[0m\027[31m\027[0m"

let _ = "Nested colors" >>> fun _ ->
  Template.(render (red (text " a " ^ (green (text " b ")) ^ (text " c "))))
    => "\027[31m a \027[32m b \027[0m\027[31m c \027[0m"

module TestRestoringContext = struct
  let context = [Terminal.Text.(Style.Foreground Color.Red)]

  let _ = "Text does not interfere, so no need to restore context" >>> fun _ ->
    Template.(render (text "a") ~context) => "a";
    Template.(render (var `v) ~context ~vars:(fun _ -> "v")) => "v"

  let _ = "Styled text restores context" >>> fun _ ->
    Template.(render (green (text " x ")) ~context)
      => "\027[32m x \027[0m\027[31m";
end
