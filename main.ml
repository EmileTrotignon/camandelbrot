(*
#require "parmap";;
#load "parmap.cma";;
#load "graphics.cma";;
*)
open Graphics
open Printf
open Histogram
open Color

let log = open_out "log"
let foi = float_of_int
let iof = int_of_float

let (+%) (xr, xi) (yr, yi) =
  xr +. yr, xi +. yi

let squarred (xr, xi) =
  ((xr *. xr) -. (xi *. xi)), (2. *. xr *. xi)

let norm2 (xr, xi) = xr *. xr +. xi *. xi
  
let znp1 zn c = (squarred zn) +% c

let is_bounded it c =
  let zn = ref (0., 0.) in
  let n = ref 0 in
 
  while norm2 !zn < 4. && !n < it do
    zn := znp1 !zn c;
    n := !n + 1;
  done;
  if !n = it then -1
  else !n


let pixel_color it c =
  match is_bounded it c with
  | -1 -> white
  | x -> black - (white) / (it / (it -x))
  
let ft_height x_def y_def width =
  width *. (float_of_int y_def) /. (float_of_int x_def)

let pixel_to_complex x y x_def y_def (x_center, y_center) width height =
  let x_corner = x_center -. (width /. 2.) in
  let y_corner = y_center -. (height /. 2.) in
  let x_pas = width /. (float_of_int (x_def - 1)) in
  let y_pas = height /. (float_of_int (y_def - 1)) in
  printf "%F %F %F %F\n" x_corner y_corner y_pas x_pas;
  (x_corner +. (x_pas *. (float_of_int x)), y_corner +. (y_pas *. (float_of_int y)))
               
let draw_mandelbrot_set_BW x_def y_def (x_center, y_center) width it =
  let height = ft_height x_def y_def width in
  let x_corner = x_center -. (width /. 2.) in
  let y_corner = y_center -. (height /. 2.) in
  let x_pas = width /. (float_of_int x_def) in
  let y_pas = height /. (float_of_int y_def) in
  let x_complex = ref x_corner in
  let y_complex = ref y_corner in
  let bounded = ref (is_bounded it (!x_complex, !y_complex)) in
  for y = 0 to y_def do
    for x = 0 to x_def do
      bounded := is_bounded it (!x_complex, !y_complex);
     
      set_color (match !bounded with
                 | -1 -> black
                 | _ -> white);
      plot x y;
      x_complex := !x_complex +. x_pas;
    done;
    y_complex := !y_complex +. y_pas;
    x_complex := x_corner
  done
  
let draw_mandelbrot_set_BW_multicore x_def y_def (x_center, y_center) width it =
  let log = open_out "log" in
  let pixel_color_BW it c =
  match is_bounded it c with
  | -1 -> white
  | _ -> black
  in
  printf "draw_mandelbrot %d %d (%F, %F) %F %d;;" x_def y_def x_center y_center width it;
  fprintf log "draw_mandelbrot %d %d (%F, %F) %F %d;;" x_def y_def x_center y_center width it;
  print_newline ();
  let height = ft_height x_def y_def width in
  let x_corner = x_center -. (width /. 2.) in
  let y_corner = y_center +. (height /. 2.) in
  let complex_matrix = Array.make y_def (Array.make 1 (0., 0.)) in
  let x_complex = ref x_corner in
  let y_complex = ref y_corner in
  let x_pas = width /. (float_of_int (x_def - 1)) in
  let y_pas = height /. (float_of_int (y_def -1)) in
  for y = 0 to y_def - 1 do
    complex_matrix.(y) <- Array.make x_def (0., 0.);
    for x = 0 to x_def - 1 do
      complex_matrix.(y).(x) <- (!x_complex, !y_complex);
      x_complex := !x_complex +. x_pas;
    done;
    y_complex := !y_complex -. y_pas;
    x_complex := x_corner;
  done;

  let color_matrix = Parmap.array_parmap (Array.map (pixel_color_BW it)) complex_matrix in
  (*Array.iter (printf "ENDLINE\n"; Array.iter (printf "%d ")) b_matrix*)
  let img = make_image color_matrix in
  draw_image img 0 0
  (*
  printf "complex_matrix : %d\n" (Array.length complex_matrix);*)

let draw_mandelbrot_set_multicore x_def y_def (x_center, y_center) width it color_array =

  printf "draw_mandelbrot %d %d (%F, %F) %F %d;;" x_def y_def x_center y_center width it;
  fprintf log "draw_mandelbrot %d %d (%F, %F) %F %d;;" x_def y_def x_center y_center width it;
  print_newline ();
  let height = ft_height x_def y_def width in
  let x_corner = x_center -. (width /. 2.) in
  let y_corner = y_center +. (height /. 2.) in
  let complex_matrix = Array.make y_def (Array.make 1 (0., 0.)) in
  let x_complex = ref x_corner in
  let y_complex = ref y_corner in
  let x_pas = width /. (float_of_int (x_def - 1)) in
  let y_pas = height /. (float_of_int (y_def -1)) in
  for y = 0 to y_def - 1 do
    complex_matrix.(y) <- Array.make x_def (0., 0.);
    for x = 0 to x_def - 1 do
      complex_matrix.(y).(x) <- (!x_complex, !y_complex);
      x_complex := !x_complex +. x_pas;
    done;
    y_complex := !y_complex -. y_pas;
    x_complex := x_corner;
  done;
  let speed_matrix = Parmap.array_parmap (Array.map (is_bounded it)) complex_matrix in
  let ch = cumulative_histogram speed_matrix it in
  let maj = ch.(Array.length ch - 1) + 1 in
  let ncolor = Array.length color_array - 1 in
  let pixel_color s =
    match s with
    | -1 -> white
    | _ -> let coeff = ((foi ch.(s)) /. (foi maj)) in
           let c1 = (foi ncolor *. coeff *. coeff) in
           let coeff_c1 = c1 -. (floor c1) in
           rgb_mean   color_array.(iof (ceil c1)) color_array.(iof (floor c1)) coeff_c1
  in
  let color_matrix = Parmap.array_parmap (Array.map pixel_color) speed_matrix in
  let img = make_image color_matrix in
  draw_image img 0 0;
  printf "done\n"

let x_def = 1920 
let y_def = 1080
;;
open_graph (String.concat "" [" "; (string_of_int x_def); "x"; (string_of_int y_def);])
;;
let center = ref (0., 0.)
let ncenter = ref (0., 0.)
let width = ref 4. 
let it = ref 100
let color_array = [|black; 0xff00ff; yellow|]
;;
draw_mandelbrot_set_multicore x_def y_def !center !width !it color_array;
let b = ref true in
while !b do
  let s = wait_next_event [Key_pressed; Button_down] in
  if s.keypressed then (
    if (s.key = '+' || s.key = '=') then width := !width /. 2.;
    if s.key = '-' then width := !width *. 2.;
    if s.key = 'u' then (
      center := !ncenter;
      draw_mandelbrot_set_multicore x_def y_def !center !width !it color_array);
    if s.key = 'Q' then b := false;
    if s.key = '.' || s.key = '>' then it := !it + 100;
    if s.key = ',' || s.key = '<' then it := !it - 100;
    if s.key = '/' || s.key = '?' then it := !it * 2;
    if s.key = 'm' || s.key = 'M' then it := !it / 2;
    if s.key = 'w' then ncenter := !ncenter +% (0., (ft_height x_def y_def !width) /. 2.);
    if s.key = 's' then ncenter := !ncenter +% (0., -.(ft_height x_def y_def !width) /. 2.);
    if s.key = 'w' then ncenter := !ncenter +% (!width /. 2., 0.);
  );
  if s.button then (
    ncenter := (pixel_to_complex s.mouse_x s.mouse_y x_def y_def (!center) (!width)
                  (ft_height x_def y_def !width));
    printf "%d %d\n" s.mouse_x s.mouse_y;
    printf "%F %F" (fst !ncenter) (snd !ncenter);
    print_newline ();
  )
done;
exit 0
;;
      

