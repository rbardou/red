let info m = Debug.echo m
let infof x = Printf.ksprintf info x

let warn m = infof "Warning: %s" m
let warnf x = Printf.ksprintf warn x

let error m = infof "Error: %s" m
let errorf x = Printf.ksprintf error x
