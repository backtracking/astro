
(* Numerical constants *)

module MKSA = struct

  let pi = 3.14159265358979323846

  let earth_radius = 6378140. (* m *)
  let moon_radius = 1738000. (* m *)
  let ua = 1.49e11 (* m *)

end

module type S = sig

  val earth_radius : float
  val moon_radius : float

  val m : float -> float
  val km : float -> float
  val er : float -> float
  val ua : float -> float

  val to_m : float -> float

end

(* unit = meter *)

module M : S = struct

  let m x = x
  let km x = x *. 1000.
  let er x = x *. MKSA.earth_radius
  let ua x = x *. MKSA.ua

  let earth_radius = MKSA.earth_radius
  let moon_radius = m MKSA.moon_radius

  let to_m x = x
  let to_km x = x /. 1000.

end

(* unit = Earth radius *)

module ER = struct

  let m x = x /. MKSA.earth_radius
  let km x = x /. (MKSA.earth_radius /. 1000.)
  let er x = x

  let one_ua =  MKSA.ua /. MKSA.earth_radius
  let ua x = x *. one_ua

  let earth_radius = 1.0
  let moon_radius = MKSA.moon_radius /. MKSA.earth_radius

  let to_m x = x *. MKSA.earth_radius
  let to_km x = x *. (MKSA.earth_radius /. 1000.)
end
