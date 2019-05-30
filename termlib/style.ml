type t =
  {
    intensity: Term.intensity;
    underline: bool;
    fg: Term.color;
    bg: Term.color;
  }

let show style =
  Printf.sprintf "{ %s, %b, %s, %s }"
    (Term.show_intensity style.intensity)
    style.underline
    (Term.show_color style.fg)
    (Term.show_color style.bg)

let default =
  {
    intensity = Normal;
    underline = false;
    fg = Default;
    bg = Default;
  }

let revert style =
  let fg =
    match style.bg with
      | Default -> Term.Black
      | color -> color
  in
  let bg =
    match style.fg with
      | Default -> Term.White
      | color -> color
  in
  { style with fg; bg }

let make
    ?(intensity = Term.Normal)
    ?(underline = false)
    ?(fg = Term.Default)
    ?(bg = Term.Default)
    () =
  { intensity; underline; fg; bg }

let normal
    ?(underline = false)
    ?(fg = Term.Default)
    ?(bg = Term.Default)
    () =
  { intensity = Normal; underline; fg; bg }

let bold
    ?(underline = false)
    ?(fg = Term.Default)
    ?(bg = Term.Default)
    () =
  { intensity = Bold; underline; fg; bg }

let faint
    ?(underline = false)
    ?(fg = Term.Default)
    ?(bg = Term.Default)
    () =
  { intensity = Faint; underline; fg; bg }

let random
    ?intensity
    ?underline
    ?fg
    ?bg
    () =
  let intensity: Term.intensity =
    match intensity with
      | Some intensity ->
          intensity
      | None ->
          match Random.int 3 with
            | 0 -> Normal
            | 1 -> Bold
            | _ -> Faint
  in
  let underline =
    match underline with
      | Some underline ->
          underline
      | None ->
          Random.bool ()
  in
  let random_color: _ -> Term.color = function
    | Some color ->
        color
    | None ->
        match Random.int 9 with
          | 0 -> Default
          | 1 -> Black
          | 2 -> Red
          | 3 -> Green
          | 4 -> Yellow
          | 5 -> Blue
          | 6 -> Magenta
          | 7 -> Cyan
          | _ -> White
  in
  let fg = random_color fg in
  let bg = random_color bg in
  { intensity; underline; fg; bg }

let fg fg =
  make ~fg ()

let bg bg =
  make ~bg ()
