
(* Calculs astronomiques *)

open Format

(* Dates *)

type date = {
  annee : int;
  mois : int; (* 1..12 *)
  jour : int; (* 1..31 *)
  fraction_jour : float; (* 0..1 *)
}

exception DateInvalide

let annee_bissextile y = (y mod 400 = 0) || (y mod 4 = 0 && y mod 100 <> 0)

let nb_jours_mois y = function
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if annee_bissextile y then 29 else 28
  | _ -> assert false

let date_valide d =
  d.mois >= 1 && d.mois <= 12 &&
  d.jour >= 1 && d.jour <= nb_jours_mois d.annee d.mois &&
  d.fraction_jour >= 0. && d.fraction_jour < 1.

let date_of_string s =
  try
    let n = String.length s in
    let d = String.index s '.' in
    let a = int_of_string (String.sub s 0 d) in
    let m = int_of_string (String.sub s (d + 1) 2) in
    let j = int_of_string (String.sub s (d + 3) 2) in
    let nf = n - d - 5 in
    let f =
      if nf = 0 then
	0.
      else
	float_of_string ("0." ^ String.sub s (d + 5) nf)
    in
    let d = { annee = a; mois = m; jour = j; fraction_jour = f } in
    if not (date_valide d) then raise DateInvalide;
    d
  with _ ->
    raise DateInvalide

let string_of_date d =
  sprintf "%d.%02d%02d%s" d.annee d.mois d.jour
    (let f = d.fraction_jour in
     if f = 0. then "" else sprintf "%03d" (truncate (f *. 1000.)))

(* Jour Julien *)

type jour_julien = float

let ent x = float (truncate x)

let jour_julien d =
  if not (date_valide d) then raise DateInvalide;
  let y,m = if d.mois > 2 then d.annee, d.mois else d.annee - 1, d.mois + 12 in
  let gregorien =
    d.annee >= 1583 ||
    (d.annee = 1582 &&
     let dd = truncate (100. *. d.fraction_jour) in
     d.mois * 10000 + d.jour * 100 + dd >= 101500)
  in
  let jj = ent (365.25 *. float y) +. ent (30.6001 *. (float (m + 1))) +.
	   float d.jour +. d.fraction_jour +. 1720994.5
  in
  if gregorien then
    jj +. float (let a = y / 100 in 2 - a + a / 4)
  else
    jj

let jjs s = jour_julien (date_of_string s)

(* exemple *)
let lancement_Spoutnik_1 = jour_julien (date_of_string "1957.100481")
let _ = assert (lancement_Spoutnik_1 = 2436116.31)

(* inversement... *)

let date jj =
  assert (jj > 0.);
  let jj = jj +. 0.5 in
  let z = floor jj in
  let f = jj -. z in
  let a =
    if z < 2299161. then
      z
    else
      let alpha = ent ((z -. 1867216.25) /. 36524.25) in
      z +. 1. +. alpha -. ent (alpha /. 4.)
  in
  let b = a +. 1524. in
  let c = ent ((b -. 122.1) /. 365.25) in
  let d = ent (365.25 *. c) in
  let e = ent ((b -. d) /. 30.6001) in
  let dddd = b -. d -. ent (30.6001 *. e) +. f in
  let m = if e < 13.5 then truncate e - 1 else truncate e - 13 in
  let y = if m >= 3 then truncate c - 4716 else truncate c - 4715 in
  (* on ne garde que 4 décimales de la fraction de jour *)
  let fj = (floor (10000. *. (dddd -. floor dddd))) /. 10000. in
  { annee = y; mois = m; jour = truncate dddd; fraction_jour = fj }

let _ = assert (date lancement_Spoutnik_1 = date_of_string "1957.100481")

(* s Jour de la semaine *)

type jour = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche

let jour_de_la_semaine d =
  let jj = jour_julien { d with fraction_jour = 0. } +. 1.5 in
  match (truncate jj) mod 7 with
    | 0 -> Dimanche
    | 1 -> Lundi
    | 2 -> Mardi
    | 3 -> Mercredi
    | 4 -> Jeudi
    | 5 -> Vendredi
    | 6 -> Samedi
    | _ -> assert false

let _ = assert (jour_de_la_semaine (date_of_string "1954.063000") = Mercredi)

(*s Jour de l'année *)

let jour_de_l_annee d =
  if annee_bissextile d.annee then
    ((275 * d.mois) / 9) - ((d.mois + 9) / 12) + d.jour - 30
  else
    ((275 * d.mois) / 9) - 2 * ((d.mois + 9) / 12) + d.jour - 30

let _ = assert (jour_de_l_annee (date_of_string "1978.111400") = 318)
let _ = assert (jour_de_l_annee (date_of_string "1980.042200") = 113)

(*s Temps sidéral à Greenwich (page 27) *)

let siecles_juliens jj = (jj -. 2415020.) /. 36525.

let temps_sideral d =
  let jj = jour_julien { d with fraction_jour = 0. } in
  let t = siecles_juliens jj in
  let th0 = 6.6460656 +. 2400.051262 *. t +. 0.00002581 *. t *. t in
  let th = th0 +. 1.002737908 *. 24. *. d.fraction_jour in
  (* on ramène dans l'intervalle 0-24 heures *)
  th -. 24. *. floor (th /. 24.)

let temps_sideral_hms d =
  let ts = temps_sideral d in
  let h = floor ts in
  let ts = (ts -. h) *. 60. in
  let m = floor ts in
  let s = (ts -. m) *. 60. in
  truncate h, truncate m, s

(* NOTE: différence de précision flottante entre ocamlc et ocamlopt;
   meilleure pour ocamlc pour lequel on peut prendre 1e-10 *)
let precision = 1e-5
let eq_float x y = abs_float (x -. y) < precision

let _ =
  assert (let h,m,s = temps_sideral_hms (date_of_string "1978.1113") in
	  h = 3 && m = 27 && eq_float s 1.33157320077)

(*s Normalisation des angles. Ramène un angle dans l'intervale 0 (inclus)
    à 360 (exclus), qu'il soit positif ou négatif. *)

let norm_degres d = d -. 360. *. floor (d /. 360.)

let pi = 3.14159265358979323846
let pi_sur_180 = pi /. 180.
let _180_sur_pi = 180. /. pi

let degres r = _180_sur_pi *. r
let radians d = pi_sur_180 *. d

let sind x = sin (x *. pi_sur_180)
let cosd x = cos (x *. pi_sur_180)
let tand x = tan (x *. pi_sur_180)

let arcsind x = _180_sur_pi *. asin x
let arccosd x = _180_sur_pi *. acos x
let arctand x = _180_sur_pi *. atan x

(* version particulière pour le calcul de [arctand (y/x)], pour obtenir
   un résultat dans le bon quadrant *)
let arctanq y x =
  let a = arctand (y /. x) in
  if x < 0. then a +. 180. else a

(*s Coordonnées *)

type coordonnees_ecliptiques = {
  longitude : float; (* degrés *)
  latitude : float;  (* degrés, positive au nord, négative au sud *)
}

type coordonnees_equatoriales = {
  ascension_droite : float; (* degrés *)
  declinaison : float; (* degrés, positive au nord, négative au sud *)
}

type coordonnees_horizontales_locales = {
  azimut : float;
  hauteur : float;
}

(*s Obliquité de l'écliptique *)

let obliquite_ecliptique_t t =
  let t2 = t *. t in
  23.452294 -. 0.0130125 *. t -. 0.00000164 *. t2 +. 0.000000503 *. t *. t2

let obliquite_ecliptique jj =
  obliquite_ecliptique_t (siecles_juliens jj)

(*s Transformation des coordonnées *)

let equatoriales_eps eps ecl =
  let lambda = ecl.longitude in
  let beta = ecl.latitude in
  let sineps = sind eps in
  let coseps = cosd eps in
  let sinlam = sind lambda in
  let alpha =
    arctanq (sinlam *. coseps -. tand beta *. sineps) (cosd lambda)
  in
  let delta = arcsind (sind beta *. coseps +. cosd beta *. sineps *. sinlam) in
  { ascension_droite = norm_degres alpha; declinaison = delta }

let equatoriales jj =
  equatoriales_eps (obliquite_ecliptique jj)

let ecliptiques jj ecl =
  let eps = obliquite_ecliptique jj in
  let alpha = ecl.ascension_droite in
  let delta = ecl.declinaison in
  let sinalpha = sind alpha in
  let sineps = sind eps in
  let coseps = cosd eps in
  let lambda =
    arctanq (sinalpha *. coseps +. tand delta *. sineps) (cosd alpha)
  in
  let beta =
    arcsind (sind delta *. coseps -. cosd delta *. sineps *. sinalpha)
  in
  { longitude = lambda; latitude = beta }

type observateur = {
  obs_longitude : float;
  obs_latitude : float
}

let horizontales d obs equ =
  let alpha = equ.ascension_droite in
  let delta = equ.declinaison in
  let l = obs.obs_longitude in
  let phi = obs.obs_latitude in
  let th0 = 15. *. (temps_sideral d) in (* en degrés ! *)
  let _H = th0 -. l -. alpha in
  let sinphi = sind phi in
  let cosphi = cosd phi in
  let cosH = cosd _H in
  let a = arctanq (sind _H) (cosH *. sinphi -. tand delta *. cosphi) in
  let h = arcsind (sinphi *. sind delta +. cosphi *. cosd delta *. cosH) in
  { azimut = a; hauteur = h }

(*s Soleil (chapitre 15 page 55) *)

type soleil = {
  s_longitude_moyenne : float; (* L *)
  s_anomalie_moyenne : float;  (* M *)
  excentricite_orbite_terrestre : float; (* e *)
  pos_ecl_vraie : coordonnees_ecliptiques;
  pos_ecl_apparente : coordonnees_ecliptiques;
  pos_equ_vraie : coordonnees_equatoriales;
  pos_equ_apparente : coordonnees_equatoriales;
  s_anomalie_vraie : float; (* v *)
  s_rayon_vecteur : float; (* R, distance Terre-Soleil, en UA *)
}

let soleil jj =
  let t = siecles_juliens jj in
  let t2 = t *. t in
  let t3 = t *. t2 in
  let l = 279.69668 +. 36000.76892 *. t +. 0.0003025 *. t2 in
  let m = 358.47583 +. 35999.04975 *. t -. 0.000150 *. t2 -. 0.0000033 *. t3 in
  (* normalisation tout de suite pour améliorer le calcul de c ci-dessous *)
  let m = norm_degres m in
  let e = 0.01675104 -. 0.0000418 *. t -. 0.000000126 *. t2 in
  let c = (1.919460 -. 0.004789 *. t -. 0.000014 *. t2) *. sind m +.
	  (0.020094 -. 0.000100 *. t) *. sind (2. *. m) +.
	  0.000293 *. sind (3. *. m)
  in
  let lv = norm_degres (l +. c) in
  let v = m +. c in
  let r = (1.0000002 *. (1. -. e *. e)) /. (1. +. e *. cosd v) in
  let omega = norm_degres (259.18 -. 1934.142 *. t) in
  let lv_app = norm_degres (lv -. 0.00569 -. 0.00479 *. sind omega) in
  (* version simplifiée de [equatoriales] lorsque la latitude est nulle *)
  let equatoriales_s eps long =
    let sinlong = sind long in
    let alpha = norm_degres (arctanq (cosd eps *. sinlong) (cosd long)) in
    { ascension_droite = alpha;
      declinaison = arcsind (sind eps *. sinlong) }
  in
  let eps = obliquite_ecliptique_t t in
  let eps_app = eps +. 0.00256 *. cosd omega in
  { s_longitude_moyenne = norm_degres l;
    s_anomalie_moyenne = m;
    excentricite_orbite_terrestre = e;
    pos_ecl_vraie = { longitude = lv; latitude = 0. };
    pos_ecl_apparente = { longitude = lv_app; latitude = 0. };
    pos_equ_vraie = equatoriales_s eps lv;
    pos_equ_apparente = equatoriales_s eps_app lv_app;
    s_anomalie_vraie = norm_degres v;
    s_rayon_vecteur  = r;
  }

(* exemple 15-a page 58 *)
let _ =
  assert (let jj = jour_julien (date_of_string "1978.1112") in
	  let s = soleil jj in
	  eq_float s.pos_ecl_vraie.longitude 229.25049 &&
	  eq_float s.pos_ecl_apparente.longitude 229.24429 &&
	  eq_float s.pos_equ_apparente.ascension_droite 226.79147 &&
	  eq_float s.pos_equ_apparente.declinaison (-17.53682))

(*s Lune (chapitre 27 page 105) *)

type lune = {
  l_longitude_moyenne : float;
  l_anomalie_moyenne : float;
  l_pos_ecl : coordonnees_ecliptiques;
  l_pos_equ : coordonnees_equatoriales;
  fraction_illuminee : float;
  angle_partie_eclairee : float;
  l_parallaxe : float;
  l_rayon_vecteur : float; (* distance centres Terre-Lune, en kms *)
}

let lune jj =
  let t = siecles_juliens jj in
  let t2 = t *. t in
  let t3 = t *. t2 in
  let s = soleil jj in
  let l' = 270.434164 +. 481267.8831 *. t -. 0.001133 *. t2 +. 0.0000019 *. t3
  in
  let l' = norm_degres l' in
  let m = s.s_anomalie_moyenne in
  let m' = 296.104608 +. 477198.8491 *. t +. 0.009192 *. t2 +. 0.0000144 *. t3
  in
  let m' = norm_degres m' in
  let deuxm' = norm_degres (2. *. m') in
  let d = 350.737486 +. 445267.1142 *. t -. 0.001436 *. t2 +. 0.0000019 *. t3
  in
  let d = norm_degres d in
  let deuxd = norm_degres (2. *. d) in
  let f = 11.250889 +. 483202.0251 *. t -. 0.003211 *. t2 -. 0.0000003 *. t3 in
  let f = norm_degres f in
  let omega = 259.183275 -. 1934.1420 *. t +. 0.002078 *. t2 +. 0.0000022 *. t3
  in
  let omega = norm_degres omega in
  (* longitude et latitude géocentriques *)
  let lambda = l' +. 6.288750 *. sind m'
                  +. 1.274018 *. sind (deuxd -. m')
		  +. 0.658309 *. sind deuxd
		  +. 0.213616 *. sind deuxm'
		  (* etc *)
  in
  let lambda = norm_degres lambda in
  let b =    5.128189 *. sind f
	  +. 0.280606 *. sind (m' +. f)
	  +. 0.277693 *. sind (m' -. f)
	  +. 0.173238 *. sind (deuxd -. f)
	  (* etc *)
  in
  let omega1 = 0.0004664 *. cosd omega in
  let omega2 = 0.0000754 *. cosd (omega +. 275.05 -. 2.30 *. t) in
  let beta = b *. (1. -. omega1 -. omega2) in
  let beta = norm_degres beta in
  let beta = if beta > 90. then beta -. 360. else beta in
  let ecl = { longitude = lambda; latitude = beta } in
  let equ = equatoriales_eps (obliquite_ecliptique_t t) ecl in
  (* parallaxe et rayon vecteur *)
  let pa = 0.950724
	+. 0.051818 *. cosd m'
	+. 0.009531 *. cosd (deuxd -. m')
	+. 0.007843 *. cosd deuxd
        +. 0.002824 *. cosd deuxm'
        (* etc *)
  in
  let r = 6378.14 /. sind pa in
  (* fraction illuminee *)
  let i =
    let s_long = s.pos_ecl_vraie.longitude in
    let d = arccosd (cosd (lambda -. s_long) *. cosd beta) in
    180. -. d -. 0.1468 *. sind d *. (1. -. 0.0549 *. sind m')
                                  /. (1. -. 0.0167 *. sind m)
  in
  let k = (1. +. cosd i) /. 2. in
  (* angle partie éclairée (chapitre 10 page 41) *)
  let x =
    let a_a' = s.pos_equ_vraie.ascension_droite -. equ.ascension_droite in
    let d = s.pos_equ_vraie.declinaison in
    let d' = equ.declinaison in
    arctanq (cosd d *. sind a_a')
            (cosd d' *. sind d -. sind d' *. cosd d *. cosd a_a')
  in
  { l_longitude_moyenne = l';
    l_anomalie_moyenne = m;
    l_pos_ecl = ecl;
    l_pos_equ = equ;
    fraction_illuminee = k;
    angle_partie_eclairee = x;
    l_parallaxe = pa;
    l_rayon_vecteur = r;
  }

(* exemple 27-a page 109 *)
let _ =
  assert (let jj = jour_julien (date_of_string "1979.1207") in
	  let l = lune jj in
	  truncate l.l_pos_ecl.longitude = 113 &&
          truncate l.l_pos_ecl.latitude = -3)

(* exemple 10-a page 41 (angle partie éclairée) *)
(*i PROBLEME
let _ =
  assert (let jj = jjs "1979.0202875" in
	  let l = lune jj in
	  ...)
i*)

(* transformation des coordonées héliocentriques en coordonnées géocentriques
   (chapitre 21, page 77) *)

type coordonnees_heliocentriques = {
  h_longitude : float;
  h_latitude : float;
  h_distance : float
}

let geocentriques_soleil s ch =
  let r = ch.h_distance in
  let l = ch.h_longitude in
  let b = ch.h_latitude in
  let lS = s.pos_ecl_vraie.longitude in
  let rS = s.s_rayon_vecteur in
  let l_lS = l -. lS in
  let cos_b = cosd b in
  let cos_l_lS = cosd l_lS in
  let lambda =
    arctanq (r *. cos_b *. sind l_lS) (r *. cos_b *. cos_l_lS +. rS) +. lS
  in
  let delta =
    sqrt (rS *. rS +. r *. r +. 2.0 *. r *. rS *. cos_b *. cos_l_lS)
  in
  let beta = arcsind (r *. sind b /. delta) in
  { longitude = norm_degres lambda; latitude = beta }, delta

let geocentriques jj = geocentriques_soleil (soleil jj)
