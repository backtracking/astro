
(* Calculs astronomiques.

   D'après l'ouvrage de Jean MEEUS "Calculs astronomiques à l'usage des
   amateurs"

*)

(*s Dates *)

type date = {
  annee : int;
  mois : int; (* 1..12 *)
  jour : int; (* 1..31 *)
  fraction_jour : float; (* 0..1 *)
}

(* [date_of_string] convertit une chaine de caractères de la forme
   "YYYY.MMDDdd" en une date, où "YYYY" désigne l'année (non nécessairement
   4 chiffres, éventuellement négatif), "MM" le mois, "DD" le jour
   et "dd" la fraction du jour (sur un nombre quelconque de chiffres,
   y compris zéro). *)

val date_of_string : string -> date
val string_of_date : date -> string

exception DateInvalide

(*s Jour Julien (page 18).

    L'exception [DateInvalide] est levée si les champs de la date ne sont
    pas dans les intervales indiqués ci-dessus. *)

type jour_julien = float

val jour_julien : date -> jour_julien

val date : jour_julien -> date

(*s Jour de la semaine (page 21). *)

type jour = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche

val jour_de_la_semaine : date -> jour

(*s Jour de l'année (page 22). Entre 1 (1er janvier) et 365 ou 366 pour les
    années bissextiles. *)

val jour_de_l_annee : date -> int

(*s Temps sidéral MOYEN (page 27). *)

  (* en heures : *)
val temps_sideral : date -> float

   (* en heures, minutes, secondes : *)
val temps_sideral_hms : date -> int * int * float

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
  azimut : float; (* degrés *)
  hauteur : float; (* degrés, positive = visible *)
}

(*s Transformation des coordonnées.

    Étant donné un jour julien [jj],
    [equatoriales jj ecl] transforment des coordonnées écliptiques [ecl] en
    des coordonnées équatoriales,
    et [ecliptiques jj equ] transforment inversement des coordonnées
    équatoriales [equ] en des coordonnées écliptiques. *)

val equatoriales :
  jour_julien -> coordonnees_ecliptiques -> coordonnees_equatoriales

val ecliptiques :
  jour_julien -> coordonnees_equatoriales -> coordonnees_ecliptiques

type observateur = {
  obs_longitude : float;
  obs_latitude : float
}

val horizontales :
  date -> observateur ->
  coordonnees_equatoriales -> coordonnees_horizontales_locales

(*s Soleil *)

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

val soleil : jour_julien -> soleil

(*s Lune *)

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

val lune : jour_julien -> lune

(* transformation des coordonées héliocentriques en coordonnées géocentriques
   (chapitre 21, page 77). Le second résultat est la distance à la Terre,
   en UA. *)

type coordonnees_heliocentriques = {
  h_longitude : float; (* degrés *)
  h_latitude : float; (* degrés *)
  h_distance : float (* en UA *)
}

val geocentriques :
  jour_julien -> coordonnees_heliocentriques -> coordonnees_ecliptiques * float
val geocentriques_soleil :
  soleil -> coordonnees_heliocentriques -> coordonnees_ecliptiques * float

(*s To be re-used in test programs *)

val norm_degres : float -> float (* ramène dans [0,360[ *)

val degres : float -> float (* radians -> degres *)
val radians : float -> float (* degres -> radians *)

val cosd : float -> float
val sind : float -> float

val annee_bissextile : int -> bool
val nb_jours_mois : (*annee*)int -> (*mois*)int -> int
