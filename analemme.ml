
open Format
open Arg
open Astro
open Lib
open Graphics

let annee = ref 2007
let heure = ref 0.5
let massy = { obs_longitude = -2.2772; obs_latitude = 48.7314 }
let observateur = ref massy
let set_long l = observateur := { !observateur with obs_longitude = l }
let set_lat l = observateur := { !observateur with obs_latitude = l }

let _ =
  parse [ "-annee", Set_int annee, "année (def = 2007)";
	  "-heure", Set_float heure, "heure dans 0..1 (def = 0.5)";
	  "-long", Float set_long, "longitude (observateur, def = Massy)";
	  "-lat", Float set_lat, "latitude (observateur, def = Massy)" ]
      (fun _ -> ()) "usage: analemme [options]"

let mag = 4
let magf = float mag
let w = mag * 360
let h = mag * 90
let () = open_graph (sprintf " %dx%d" w h)

let () =
  set_color black;
  for m = 1 to 12 do
    for j = 1 to nb_jours_mois !annee m do
      let d = { annee = !annee; mois = m; jour = j; fraction_jour = !heure } in
      let s = soleil (jour_julien d) in
      let a,h =
	let c = horizontales d !observateur s.pos_equ_apparente in
	c.azimut, c.hauteur
      in
      let a = if a < -180. then a +. 360. else a in
      let a = if a >= 180. then a -. 360. else a in
      if h >= 0. then plot (w/2 + truncate (magf *. a)) (truncate (magf *. h))
    done
  done

let () = ignore (read_key ())
