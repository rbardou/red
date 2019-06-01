let focus = Style.make ~fg: Black ~bg: Red ()

let literal = Style.make ~fg: Cyan ()
let other = Style.make ~fg: Yellow ()
let invalid = Style.make ~fg: Red ()
let keyword = Style.make ~fg: Magenta ()
let identifier = Style.default
let constructor = Style.default
let module_ = Style.make ~fg: Blue ()
let integer = literal
let float = literal
let comment = Style.make ~fg: Green ()
let char = literal
let string = literal
let string_escaped = Style.make ~fg: Blue ()
let type_variable = Style.make ~fg: Blue ()
