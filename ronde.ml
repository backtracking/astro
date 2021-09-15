
open Printf
open Graphics
open Astro
open Planets

let size = 600

let _ = open_graph (sprintf " %dx%d" size size)
let _ = auto_synchronize false

let univers_radius = 6. (*40.*)
let factor = float size /. univers_radius

let xy long r =
  size / 2 + truncate ((r *. factor) *. cos long),
  size / 2 + truncate ((r *. factor) *. sin long)

let clear () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 size size;
  set_color yellow;
  let ox,oy = xy 0. 0. in
  fill_circle ox oy 5

let mercury_color = green
let mercury_radius = 2
let venus_color = white
let venus_radius = 4
let earth_color = blue
let earth_radius = 4
let mars_color = red
let mars_radius = 3
let jupiter_color = red
let jupiter_radius = 7
let saturn_color = yellow
let saturn_radius = 6
let uranus_color = green
let uranus_radius = 5
let neptune_color = blue
let neptune_radius = 5

let draw jd =
  clear ();
  let planet p pcolor pradius =
    let pos = p jd in
    let x,y = xy pos.h_longitude pos.h_distance in
    set_color pcolor;
    fill_circle x y pradius
  in
  planet mercury mercury_color mercury_radius;
  planet venus venus_color venus_radius;
  planet earth earth_color earth_radius;
  planet mars mars_color mars_radius;
  planet jupiter jupiter_color jupiter_radius;
  planet saturn saturn_color saturn_radius;
  planet uranus uranus_color uranus_radius;
  planet neptune neptune_color neptune_radius;
  synchronize ()

let jj1 = jour_julien (Lib.now ())
let jj2 = jj1 +. 365.
let step =
  if Array.length Sys.argv > 1 then float_of_string Sys.argv.(1) else 1.

let pause () = for _ = 1 to 100000000 do () done

let _ =
  let jj = ref jj1 in
  try
    while true (* !jj <= jj2 *)  do
      draw !jj;
      let st = wait_next_event [ Poll ; Key_pressed ] in
      if st.keypressed  && (st.key = 'q' || st.key = '\027') then raise Exit;
      jj := !jj +. step
    done
  with Exit ->
    ()
