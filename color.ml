type rgb = {r: int; g: int; b: int}
type hsl = {h: float; s: float; l: float}

let print_rgb c =
  Printf.printf "{r=%d; g=%d; b=%d}" c.r c.g c.b

let print_hsl c =
   Printf.printf "{h=%F; s=%F; l=%F}" c.h c.s c.l
         
let rec list_max u =
  match u with
  | [] -> failwith "max of empty list"
  | [x] -> x
  | x :: xs -> let m = (list_max xs) in
               if x > m then x else m

let rec list_min u =
  match u with
  | [] -> failwith "min of empty list"
  | [x] -> x
  | x :: xs -> let m = (list_min xs) in
               if x < m then x else m

let rgb_of_int i =
  {r= i / (256 * 256); g = (i / 256) mod 256; b = i mod 256}

let int_of_rgb c =
  (c.r * 256 * 256) + (c.g * 256) + c.b 


let positive_float x y = mod_float (mod_float x y +. y) y
  
let hsl_of_rgb co =
  let conv_1_255 i =
    (float_of_int (i * 2)) /. 255.
  in
  let (r, g, b) = (conv_1_255 co.r, conv_1_255 co.g, conv_1_255 co.b) in
  let cl =  [r/.2.; g/.2.; b/.2.] in
  let lmax = list_max cl
  and lmin = list_min cl in
  let l = (lmax +. lmin) /. 2. in
  let s = if lmax = lmin then 0.
          else (if l < 0.5 then
                  (lmax -. lmin) /. (lmax +. lmin)
                else (lmax -. lmin) /. (2. -. lmax -. lmin))
  in
  let h = if co.r = co.g && co.r = co.b then 0.
          else let h' =(if co.r >= co.g && co.r >= co.b then g -. b 
                        else if co.g >= co.r && co.g >= co.b then 2. +. (b -. r)
                        else 4. +. (r -. g))
                       /. ((lmax -. lmin) *. 12.)
               in
               if h' < 0. then h' +. 1. else h'
  in
  {h=h; s=s; l=l}

let rgb_of_hsl  co =
  let conv_255_1 f =
    int_of_float (255. *. f)
  in
  let shove_in_0_1 f =
    if f > 1. then f -. 1.
    else if f < 0. then f +. 1.
    else f
  in
  let calc_color tc t1 t2 =
    if 6. *. tc < 1. then t2 +. (t1 -. t2) *. 6. *. tc
    else if 2. *. tc < 1. then t1
    else if 3. *. tc < 2. then t2 +. (t1 -. t2) *. ((2. /. 3.) -. tc) *. 6.
    else t2
  in
  if co.s = 0. then {r=conv_255_1 co.l; g=conv_255_1 co.l; b=conv_255_1 co.l}
  else (
    let t1 = if co.l < 0.5 then co.l *. (1. +. co.s) else co.l +. co.s -. (co.l *. co.s) in
    let t2 = shove_in_0_1 (2. *. co.l -. t1) in
    let tr = shove_in_0_1 (co.h +. (1. /. 3.)) in
    let tg = shove_in_0_1 co.h in
    let tb = shove_in_0_1 (co.h -. (1. /. 3.)) in
    let r = conv_255_1 (calc_color tr t1 t2) in
    let g = conv_255_1 (calc_color tg t1 t2) in
    let b = conv_255_1 (calc_color tb t1 t2) in
    {r=r; g=g; b=b} 
  )

let int_of_hsl c = int_of_rgb (rgb_of_hsl c)

let hsl_of_int c =
  hsl_of_rgb (rgb_of_int c)

let float_mean f1 f2 coeff =
  let coeff' = 1. -. coeff in
  (f1 *. coeff +. f2 *. coeff')
  
let hsl_mean c1 c2 coeff =
  let c1hsl = hsl_of_int c1 in
  let c2hsl = hsl_of_int c2 in
  (*print_hsl c1hsl;
  print_hsl c2hsl;*)
  let rhsl = {h = float_mean c1hsl.h c2hsl.h coeff;
              s = float_mean c1hsl.s c2hsl.s coeff;
              l = float_mean c1hsl.l c2hsl.l coeff}
  in
  (*print_hsl rhsl;*)
  int_of_hsl rhsl
