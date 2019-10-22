let cumulative_histogram speed_matrix it =
  (* histogram *)
  let h = Array.make ((it * 256) + 1) 0 in
  for y = 0 to Array.length speed_matrix - 1 do
    for x = 0 to Array.length speed_matrix.(y) - 1 do
      let s = speed_matrix.(y).(x) in
      match s with
      | None -> ()
      | Some s' ->
          let sint = int_of_float (s' *. 256.) in
          h.(sint) <- h.(sint) + 1 
    done
  done ;
  (* cumulative histogram *)
  let ch = Array.make (it * 256 + 1) 0 in
  for i = 1 to it * 256 do
    ch.(i) <- ch.(i - 1) + h.(i)
  done ;
  ch
