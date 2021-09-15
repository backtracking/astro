
(* Carte du ciel *)

open Format
open Graphics
open Astro
open Lib
open Stars
open Planets
open Arg

let nowd = now ()
let jj = ref (jour_julien nowd)
let step = ref (1. /. 24.)

let set_date s = jj := jour_julien (date_of_string s)
let set_step f = step := f /. 24.

(* IGN: Massy = 02°16'38'' Est, 48°43'53'' Nord *)
let massy = { obs_longitude = -2.2772; obs_latitude = 48.7314 }
let observateur = ref massy
let set_long l = observateur := { !observateur with obs_longitude = l }
let set_lat l = observateur := { !observateur with obs_latitude = l }

let _ =
  parse [ "-jj", Float ((:=) jj), "jour_julien";
	  "-d",  String set_date, "date";
	  "-s", Float set_step, "pas (en heures, defaut = 1)";
	  "-long", Float set_long, "longitude (observateur, def = Massy)";
	  "-lat", Float set_lat, "latitude (observateur, def = Massy)" ]
      (fun _ -> ()) "usage: ciel [options]"


(* la fenêtre de Marion *)
let fenetre = ref false
let az1 = 30. (* input "azimut 1 : " float_of_string *)
let az2 = 130. (* input "azimut 2 : " float_of_string *)
let hm = 60. (* input "hauteur max : " float_of_string *)

let size = 700
let r = (size / 2) - 10
let _ = assert (r > 0)
let ox = size / 2
let oy = size / 2

let xy a h =
  assert (h >= 0.);
  let rr = float r *. (90. -. h) /. 90. in
  ox - truncate (rr *. sind a), oy - truncate (rr *. cosd a)

let moveto a h = let x,y = xy a h in moveto x y
let lineto a h = let x,y = xy a h in lineto x y

let _ = open_graph (sprintf " %dx%d" size size)
let _ = auto_synchronize false
let text_w,text_h = text_size "M"

let clear () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 size size;
  set_color yellow;
  draw_circle ox oy r;
  if !fenetre then begin
    moveto az1 0.; lineto az1 hm;
    moveto az2 0.; lineto az2 hm;
    let r = truncate (float r *. (90. -. hm) /. 90.) in
    let angle a = truncate (270. -. a) in
    draw_arc ox oy r r (angle az1) (angle az2)
  end

let rsun = 10
let draw_sun jj =
  let s = soleil jj in
  let d = date jj in
  let a,h =
    let c = horizontales d !observateur s.pos_equ_apparente in
    c.azimut, c.hauteur
  in
  if h >= 0. then begin
    set_color yellow;
    let x,y = xy a h in
    fill_circle x y rsun
  end

let rec draw_sphere x y r a k cl cd =
  let angle a = truncate (a -. 360. *. floor (a /. 360.)) in
  let demi_arc a rx ry c =
    set_color c;
    let a = angle a in
    let a1,a2 = if a < 180 then a, a+180 else a-360,a-180 in
    fill_arc x y rx ry a1 a2
  in
  if k = 1. then begin
    set_color cl;
    fill_circle x y r
  end else if k > 0. then
    if k > 0.5 then
      draw_sphere x y r (a +. 180.) (1. -. k) cd cl
    else begin
      demi_arc a r r cl;
      demi_arc (a +. 180.) r r cd;
      if k < 0.5 then
	let r' = truncate (float r *. (1. -. 2. *. k)) in
	demi_arc a r' r cd
    end

let rmoon = 10
let draw_moon jj =
  let l = lune jj in
  let d = date jj in
  let a,h =
    let c = horizontales d !observateur l.l_pos_equ in c.azimut, c.hauteur
  in
  if h >= 0. then begin
    set_color white;
    let x,y = xy a h in
    draw_circle x y rmoon;
    draw_sphere x y rmoon
      l.angle_partie_eclairee l.fraction_illuminee white black
  end

let stars = read_file "stars.dat"
let stars_pos = Queue.create ()

let draw_stars jj =
  Queue.clear stars_pos;
  let draw_star s =
    let d = date jj in
    let equ = {ascension_droite = s.ascension; declinaison = s.declination} in
    let a,h = let c = horizontales d !observateur equ in c.azimut, c.hauteur in
    if h >= 0. then begin (* visible *)
      let r,g,b = s.color in
      let to255 f = truncate (255. *. f) in
      set_color (rgb (to255 r) (to255 g) (to255 b));
      let x,y = xy a h in plot x y;
      Queue.add (x, y, s.name) stars_pos
    end
  in
  List.iter draw_star stars

let sqr x = x * x

let show_star_at x y =
  let closest = ref None in
  let distance xs ys = sqr (x - xs) + sqr (y - ys) in
  let update ((xs,ys,ns) as s) = match !closest with
    | None ->
	closest := Some (distance xs ys, s)
    | Some (d,_) ->
	let ds = distance xs ys in if ds < d then closest := Some (ds, s)
  in
  Queue.iter update stars_pos;
  match !closest with
    | None ->
	assert false
    | Some (_, (xs,ys,s)) ->
	set_color red;
	Graphics.moveto (xs + 2) (ys + 2);
	draw_string s; synchronize ();
	ignore (wait_next_event [ Button_up ])

let planets = ref true
let draw_planets jj =
  let d = date jj in
  let draw_planet (s,pl) =
    let p = pl jj in
    let g,_ = geocentriques jj p in
    let hz = horizontales d !observateur (equatoriales jj g) in
    let a,h = hz.azimut, hz.hauteur in
    if h >= 0. then begin (* visible *)
      set_color blue;
      let x,y = xy a h in plot x y;
      Graphics.moveto (x + 2) (y + 2);
      draw_string s
    end
  in
  List.iter draw_planet
    [ "Mercure", mercury;
      "Venus", venus;
      "Mars", mars;
      "Jupiter", jupiter;
      "Saturne", saturn;
      "Uranus", uranus;
      "Neptune", neptune ]

let draw jj =
  let d = date jj in
  (* printf "%s (%s, JJ=%f)\n" (natural_date d) (string_of_date d) jj; *)
  print_flush ();
  clear ();
  draw_stars jj;
  draw_sun jj;
  if !planets then draw_planets jj;
  draw_moon jj;
  set_color white;
  Graphics.moveto 0 0;
  draw_string (natural_date_s d ^ " UTC");
  Graphics.moveto 0 text_h;
  draw_string (let h,m,s = hms !step in
	       sprintf "dt = %02d h %02d mn %02d s" h m (truncate s));
  synchronize ()

let _ =
  try
    while true do
      draw !jj;
      let st = wait_next_event [ Key_pressed; Button_down ] in
      if st.keypressed  then begin match st.key with
	| 'q' | '\027' -> raise Exit
	| 'f' -> fenetre := not !fenetre
	| 'p' -> planets := not !planets
	| '+' -> step := !step *. 2.
	| '-' -> step := !step /. 2.
	| 'b' -> jj := !jj -. !step
	| c ->
	    jj := !jj +. !step;
	    if c <> ' ' then begin
	      printf "%c, keycode = %d\n" c (Char.code c); flush stdout
	    end
      end;
      if st.button then	show_star_at st.mouse_x st.mouse_y
    done
  with Exit ->
    ()
