top =
  let f n = if n <= 1 then 1 else f True in
  f 5 