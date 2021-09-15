
open Format
open Graphics

let rterre = 6378140. (* m *)
let rterre3 = rterre *. rterre *. rterre
let mterre = 5.98e24

let rlune = 1738000.
let rlune3 = rlune *. rlune *. rlune
let mlune = 7.35e22

let dterrelune = 392739000.

let mv = 63217. (* masse vaisseau *)

let g = 6.67e-11
let gmt = g *. mterre
let gml = g *. mlune

let sizex = 1000
let sizey = 600
let _ = open_graph (sprintf " %dx%d+0+0" sizex sizey)

let w = (dterrelune +. rterre +. rlune) *. 1.1
let delta = float sizex /. w
let ox = sizex / 2 - truncate (delta *. dterrelune /. 2.)
let oy = sizey / 2
let d d = truncate (d *. delta)
let gx x = ox + d x
let gy y = oy + d y

let _ =
  set_color black;
  fill_rect 0 0 sizex sizey;
  set_color blue;
  fill_circle (gx 0.) (gy 0.) (d rterre);
  set_color white;
  fill_circle (gx dterrelune) (gy 0.) (d rlune)

let dt = 5. (* secondes *)

let pause () = for i = 1 to 10000 do () done

let burn msg n dv vx vy =
  let v = sqrt (vx *. vx +. vy *. vy) in
  let f = 1. +. dv /. v in
  printf "%s, at count %d, burn v=%f + dv=%f\n" msg n v dv; print_flush ();
  vx *. f, vy *. f

let splashdown () = printf "Splashdown!\n"; raise Exit

let mooncrash () = printf "Moon crash!\n"; raise Exit

let rec loop x y vx vy n =
  set_color black;
  plot (gx x) (gy y);
  let ax, ay =
    let rt3 = (x *. x +. y *. y) ** 1.5 in
    if rt3 < rterre3 then splashdown();
    let dlx = dterrelune -. x in
    let rl3 = (dlx *. dlx +. y *. y) ** 1.5 in
    if rl3 < rlune3 then mooncrash ();
    -. gmt *. x /. rt3 +. gml *. dlx /. rl3,
    -. gmt *. y /. rt3 -. gml *. y /. rl3
  in
  let vx' = vx +. ax *. dt in
  let vy' = vy +. ay *. dt in
  let x' = x +. vx' *. dt in
  let y' = y +. vy' *. dt in
  set_color yellow;
  plot (gx x') (gy y');
  if key_pressed () then begin
    printf "n=%d x=%f y=%f vx=%f vy=%f\n" n x y vx vy; raise Exit end;
  pause ();
  if n = 1975 then
    let vx'',vy'' = burn "TLI" n 3000. vx' vy' in
    loop x' y' vx'' vy'' (succ n)
  else if n = 29600 then
    let vx'',vy'' = burn "LOI" n (-1500.) vx' vy' in
    loop x' y' vx'' vy'' (succ n)
  else if n = 40600 then
    let vx'',vy'' = burn "TEI" n 2000. vx' vy' in
    loop x' y' vx'' vy'' (succ n)
  else
    loop x' y' vx' vy' (succ n)


let _ =
  try
    loop 8922000. 0. 0. 7793. 0
  with Exit ->
    close_graph ()
