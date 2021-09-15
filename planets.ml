
open Astro
open Vsop87

type planet_position = Astro.coordonnees_heliocentriques

type julian_day = float

let sum t a =
  let n = Array.length a in
  assert (n mod 3 == 0);
  let rec sumrec s i =
    if i == n then s /. 1.0e8
    else sumrec (s +. a.(i) *. cos (a.(i + 1) +. a.(i + 2) *. t)) (i + 3)
  in
  sumrec 0. 0

let horner4 t a0 a1 a2 a3 =
  sum t a0 +. t *. (sum t a1 +. t *. (sum t a2 +. t *. sum t a3))

let horner5 t a0 a1 a2 a3 a4 =
  sum t a0 +.
    t *. (sum t a1 +. t *. (sum t a2 +. t *. (sum t a3 +. t *. sum t a4)))

let horner6 t a0 a1 a2 a3 a4 a5 =
  sum t a0 +.
    t *. (sum t a1 +.
	    t *. (sum t a2 +.
		    t *. (sum t a3 +. t *. (sum t a4 +. t *. sum t a5))))

let make l b r =
  { h_longitude = norm_degres (degres l);
    h_latitude = norm_degres (degres b);
    h_distance = r }

let mercury jd =
  let t = (jd -. 2451545.0) /. 365250.0 in
  let l =
    horner6 t mercuryL0 mercuryL1 mercuryL2 mercuryL3 mercuryL4 mercuryL5 in
  let b = horner5 t mercuryB0 mercuryB1 mercuryB2 mercuryB3 mercuryB4 in
  let r = horner4 t mercuryR0 mercuryR1 mercuryR2 mercuryR3 in
  make l b r

let venus jd =
  let t = (jd -. 2451545.0) /. 365250.0 in
  let l =
    horner6 t venusL0 venusL1 venusL2 venusL3 venusL4 venusL5 in
  let b = horner5 t venusB0 venusB1 venusB2 venusB3 venusB4 in
  let r = horner5 t venusR0 venusR1 venusR2 venusR3 venusR4 in
  make l b r

let earth jd =
  let t = (jd -. 2451545.0) /. 365250.0 in
  let l = horner6 t earthL0 earthL1 earthL2 earthL3 earthL4 earthL5 in
  let b = sum t earthB0 +. t *. sum t earthB1 in
  let r = horner5 t earthR0 earthR1 earthR2 earthR3 earthR4 in
  make l b r

let mars jd =
  let t = (jd -. 2451545.0) /. 365250.0 in
  let l = horner6 t marsL0 marsL1 marsL2 marsL3 marsL4 marsL5 in
  let b = horner5 t marsB0 marsB1 marsB2 marsB3 marsB4 in
  let r = horner5 t marsR0 marsR1 marsR2 marsR3 marsR4 in
  make l b r

let jupiter jd =
  let t = (jd -. 2451545.0) /. 365250.0 in
  let l =
    horner6 t jupiterL0 jupiterL1 jupiterL2 jupiterL3 jupiterL4 jupiterL5 in
  let b =
    horner6 t jupiterB0 jupiterB1 jupiterB2 jupiterB3 jupiterB4 jupiterB5 in
  let r =
    horner6 t jupiterR0 jupiterR1 jupiterR2 jupiterR3 jupiterR4 jupiterR5 in
  make l b r

let saturn jd =
  let t = (jd -. 2451545.0) /. 365250.0 in
  let l = horner6 t saturnL0 saturnL1 saturnL2 saturnL3 saturnL4 saturnL5 in
  let b = horner6 t saturnB0 saturnB1 saturnB2 saturnB3 saturnB4 saturnB5 in
  let r = horner6 t saturnR0 saturnR1 saturnR2 saturnR3 saturnR4 saturnR5 in
  make l b r

let uranus jd =
  let t = (jd -. 2451545.0) /. 365250.0 in
  let l = horner5 t uranusL0 uranusL1 uranusL2 uranusL3 uranusL4 in
  let b = horner5 t uranusB0 uranusB1 uranusB2 uranusB3 uranusB4 in
  let r = horner5 t uranusR0 uranusR1 uranusR2 uranusR3 uranusR4 in
  make l b r

let neptune jd =
  let t = (jd -. 2451545.0) /. 365250.0 in
  let l = horner5 t neptuneL0 neptuneL1 neptuneL2 neptuneL3 neptuneL4 in
  let b = horner5 t neptuneB0 neptuneB1 neptuneB2 neptuneB3 neptuneB4 in
  let r = horner4 t neptuneR0 neptuneR1 neptuneR2 neptuneR3 in
  make l b r

let all_planets =
  [ mercury; venus; earth; mars; jupiter; saturn; uranus; neptune ]
