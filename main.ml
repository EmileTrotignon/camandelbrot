type complex = float * float

let (+%) (xr, xi) (yr, yi) =
  xr +. yr, xi +. yi

let squarred (xr, xi) =
  ((xr *. xr) -. (xi *. xi)), (2. *. xr *. xi)

let norm2 (xr, xi) = xr *. xr +. xi *. xi
  
let znp1 zn c = (squarred zn) +% c

let is_bounded c it =
  let zn = ref (0., 0.) in
  let n = ref 0 in
  while norm2 !zn < 4. && !n < it do
    zn := znp1 !zn c;
    n := !n + 1;
  done;
  if !n = it then None
  else Some it
