let cumulative_histogram speed_matrix it =
  let h = Array.make (it + 1) 0 in
  for y = 0 to (Array.length speed_matrix) - 1 do
    for x = 0 to (Array.length speed_matrix.(y)) - 1 do
      let s = speed_matrix.(y).(x) in
      if s >= 0 then h.(s) <- h.(s) + 1;
    done;
  done;
  let ch = Array.make (it + 1) 0 in
  for i = 1 to it do
    ch.(i) <- ch.(i - 1) + h.(i)
  done;
  ch

  
                 
