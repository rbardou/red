let file_exists filename =
  try
    Sys.file_exists filename
  with _ ->
    false
