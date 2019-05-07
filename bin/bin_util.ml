let time f = 
  Unix.gettimeofday () |> fun a -> 
  f(); 
  Unix.gettimeofday () |> fun b ->
  b -. a
