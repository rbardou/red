type t =
  {
    intensity: Term.intensity;
    underline: bool;
    fg: Term.color;
    bg: Term.color;
  }

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
