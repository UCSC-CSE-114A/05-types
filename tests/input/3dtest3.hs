top =
  let f n = if n <= 1 then 1 else f (n - 1) || False in
  f 5 