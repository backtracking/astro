
open Format
open Astro
open Lib
open Arg

let soleil_o = ref false
let lune_o = ref false
let planetes_o = ref false
let date_o = ref ""

let _ = parse [ "-s", Set soleil_o, "coordonnées Soleil";
		"-l", Set lune_o,   "coordonnées Lune";
		"-p", Set planetes_o, "coordonnées planètes";
		"-d", String ((:=) date_o), "date" ] (fun _ -> ()) ""

let jh = now ()

let date =
  if !date_o = "" then
    let msg =
      sprintf "date [%s = %s]: " (string_of_date jh) (natural_date jh) in
    input msg (function "" -> jh | s -> date_of_string s)
  else
    date_of_string !date_o

let string_of_jour = function
  | Lundi -> "Lundi"
  | Mardi -> "Mardi"
  | Mercredi -> "Mercredi"
  | Jeudi -> "Jeudi"
  | Vendredi -> "Vendredi"
  | Samedi -> "Samedi"
  | Dimanche -> "Dimanche"

let float_dec f =
  let s = sprintf "%.3f" f in
  String.sub s 1 (String.length s - 1)

let degres f =
  let d = truncate f in
  let f = abs_float (f -. float d) *. 60. in
  let m = truncate f in
  let f = (f -. float m) *. 60. in
  let f,s = modf f in
  let f = if f = 0. then "" else float_dec f in
  sprintf "%d°%02d'%02d''%s" d m (truncate s) f

let jj = jour_julien date

let _ =
  printf "\ndate : %s = %s\n" (string_of_date date) (natural_date date);
  printf "jour julien : JJ = %f\n" jj;
  let js = jour_de_la_semaine date in
  printf "jour de la semaine : %s\n" (string_of_jour js);
  printf "jour de l'année : %d\n" (jour_de_l_annee date);
  let ts = temps_sideral date in
  let h,m,s = temps_sideral_hms date in
  printf "temps sidéral MOYEN : %f = %02d:%02d:%f" ts h m s

let angle_hms a =
  let h,m,s = hms (a /. 360.) in
  sprintf "%dh %02dm %.3fs" h m s

let massy = { obs_longitude = -2.2772; obs_latitude = 48.7314 }

let print_ecliptiques fmt e =
  fprintf fmt "@[longitude : %f° = %s@\nlatitude  : %f° = %s@]"
    e.longitude (degres e.longitude) e.latitude (degres e.latitude)

let print_equatoriales fmt e =
  let a = e.ascension_droite in
  let d = e.declinaison in
  fprintf fmt "@[ascension droite : %f° = %s@\ndéclinaison      : %f° = %s@]"
    a (angle_hms a) d (degres d)

let print_horizontales fmt c =
  let a = c.azimut in
  let h = c.hauteur in
  fprintf fmt "@[azimut  A = %f° = %s@\nhauteur h = %f° = %s@]"
    a (degres a) h (degres h)

let soleil fmt () =
  let soleil = soleil jj in
  fprintf fmt "@[Soleil :@\n";
  fprintf fmt "  Position écliptique vraie :@\n    %a@\n"
    print_ecliptiques soleil.pos_ecl_vraie;
  fprintf fmt "  Position écliptique apparente :@\n    %a@\n"
    print_ecliptiques soleil.pos_ecl_apparente;
  fprintf fmt "  Position équatoriale vraie :@\n    %a@\n"
    print_equatoriales soleil.pos_equ_vraie;
  fprintf fmt "  Position équatoriale apparente :@\n    %a@\n"
    print_equatoriales soleil.pos_equ_apparente;
  fprintf fmt "  À Massy : %a@\n" print_horizontales
    (horizontales date massy soleil.pos_equ_apparente);
  fprintf fmt "  Excentricité orbite terreste : %f@\n"
    soleil.excentricite_orbite_terrestre;
  fprintf fmt "  Rayon vecteur : %f UA@\n" soleil.s_rayon_vecteur;
  fprintf fmt "@]"

let _ = if !soleil_o then printf "@[%a@\n@]@?" soleil ()

let lune fmt () =
  let lune = lune jj in
  fprintf fmt "@[Lune :@\n";
  fprintf fmt "  Position écliptique :@\n    %a@\n"
    print_ecliptiques lune.l_pos_ecl;
  fprintf fmt "  Position équatoriale :@\n    %a@\n"
    print_equatoriales lune.l_pos_equ;
  fprintf fmt "  À Massy : %a@\n" print_horizontales
    (horizontales date massy lune.l_pos_equ);
  fprintf fmt "  Fraction illuminée : %f@\n" lune.fraction_illuminee;
  fprintf fmt "  Angle partie éclairée : %f°@\n" lune.angle_partie_eclairee;
  fprintf fmt "  Parallaxe : %f° = %s@\n" lune.l_parallaxe (degres lune.l_parallaxe);
  fprintf fmt "  Rayon vecteur : %f kms@\n" lune.l_rayon_vecteur;
  fprintf fmt "@]"

let _ = if !lune_o then printf "@[%a@\n@]@?" lune ()

open Planets

let planetes fmt () =
  fprintf fmt "@[Positions planètes / Soleil :@\n";
  let planete fmt pl =
    let p = pl jj in
    fprintf fmt "@[Position héliocentrique :@\n";
    let l = p.h_longitude in
    fprintf fmt "  @[longitude = %f° = %s@\n" l (degres l);
    let l = p.h_latitude in
    fprintf fmt "latitude  = %f° = %s@\n" l (degres l);
    fprintf fmt "distance  = %f UA@]@\n" p.h_distance;
    if pl != earth then begin
      let g,r = geocentriques jj p in
      fprintf fmt "Position ecliptique :@\n  @[%a@\ndistance = %f@]@\n"
	print_ecliptiques g r;
      fprintf fmt "À Massy :@\n  %a@\n" print_horizontales
	(horizontales date massy (equatoriales jj g))
    end;
    fprintf fmt "@]"
  in
  fprintf fmt "Mercure : %a@\n" planete mercury;
  fprintf fmt "Venus   : %a@\n" planete venus;
  fprintf fmt "Terre   : %a@\n" planete earth;
  fprintf fmt "Mars    : %a@\n" planete mars;
  fprintf fmt "Jupiter : %a@\n" planete jupiter;
  fprintf fmt "Saturn  : %a@\n" planete saturn;
  fprintf fmt "Uranus  : %a@\n" planete uranus;
  fprintf fmt "Neptune : %a@\n" planete neptune;
  fprintf fmt "@]"

let _ = if !planetes_o then printf "@[%a@\n@]@?" planetes ()
