#load "graphics.cma";;
open Graphics
open Printf

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
  else Some !n

let ft_height x_def y_def width =
  width *. (float_of_int y_def) /. (float_of_int x_def)

let pixel_to_complex x y x_def y_def (x_center, y_center) width height =
  let x_corner = x_center -. (width /. 2.) in
  let y_corner = y_center +. (height /. 2.) in
  (x_corner, y_corner)
               
let draw_mandel_brot_set x_def y_def (x_center, y_center) width it =
  let height = ft_height x_def y_def width in
  let x_corner = x_center -. (width /. 2.) in
  let y_corner = y_center +. (height /. 2.) in
  let x_pas = width /. (float_of_int x_def) in
  let y_pas = height /. (float_of_int y_def) in
  (*printf "%f, %f, %f, %f\n" x_corner y_corner x_pas y_pas;*)
  open_graph "";
  let x_complex = ref x_corner in
  let y_complex = ref y_corner in
  let bounded = ref (is_bounded (!x_complex, !y_complex) it) in
  for y = 0 to y_def do
    for x = 0 to x_def do
      bounded := is_bounded (!x_complex, !y_complex) it;
      (*printf "%d %d %f %f" x y !x_complex !y_complex;*)
      set_color (match !bounded with
                 | None -> (*printf " None\n";*) black
                 | Some x -> (*printf " %d\n" x;*) white);
      plot x y;
      x_complex := !x_complex +. x_pas;
    done;
    y_complex := !y_complex -. y_pas;
    x_complex := x_corner
  done
        
                    
                      
      
