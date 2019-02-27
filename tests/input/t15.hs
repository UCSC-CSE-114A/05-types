top =
  let f1 x y = x in
  let f2 x y = y in
  let f3 = if True then f1 else f2 in
  f3 5