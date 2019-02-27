top =
  let f = \it -> \x -> it x + 1 in
  let incr = \z -> z + 1 in
  f incr 10