top =
  let const = \x -> \y -> x in
  let a1 = const True (\z -> z) in
  const 5 True
