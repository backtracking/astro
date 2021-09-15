
(* Planets positions given by longitude, latitude (in degrees)
   and distance from the Sun in AU. *)

type planet_position = Astro.coordonnees_heliocentriques

type julian_day = float

val mercury : julian_day -> planet_position
val venus   : julian_day -> planet_position
val earth   : julian_day -> planet_position
val mars    : julian_day -> planet_position
val jupiter : julian_day -> planet_position
val saturn  : julian_day -> planet_position
val uranus  : julian_day -> planet_position
val neptune : julian_day -> planet_position

val all_planets : (julian_day -> planet_position) list
