type t =
  {
    left: int;
    top: int;
    width: int;
    height: int;
    frame: Render.frame;
  }

let all frame =
  {
    left = 0;
    top = 0;
    width = Render.width frame;
    height = Render.height frame;
    frame;
  }

let sub ctx x y w h =
  {
    left = ctx.left + x;
    top = ctx.top + y;
    width = min w (ctx.width - x);
    height = min h (ctx.height - x);
    frame = ctx.frame;
  }

let set ctx x y cell =
  if x >= 0 && y >= 0 && x < ctx.width && y < ctx.height then
    Render.set ctx.frame (ctx.left + x) (ctx.top + y) cell

let get ctx x y =
  if x >= 0 && y >= 0 && x < ctx.width && y < ctx.height then
    Render.get ctx.frame (ctx.left + x) (ctx.top + y)
  else
    Render.empty_cell

let render_text ?style ctx x y string =
  Render.text_to_frame ?style ctx.frame (ctx.left + x) (ctx.top + y)
    (ctx.width - x) string
