
open Format
open Astro

let hms f =
  let f = f *. 24. in
  let h = truncate f in
  let f = (f -. float h) *. 60. in
  let m = truncate f in
  let s = (f -. float m) *. 60. in
  h, m, s

let natural_date d =
  let h,m,s = hms d.fraction_jour in
  sprintf "%d/%d/%d %02d:%02d:%f" d.jour d.mois d.annee h m s

let natural_date_s d =
  let h,m,s = hms d.fraction_jour in
  sprintf "%d/%d/%d %02d:%02d:%02d" d.jour d.mois d.annee h m (truncate s)

let rec input msg k =
  printf "%s" msg; print_flush ();
  let s = read_line () in
  try k s with _ -> input msg k

(* the current date; given by Unix.time *)

open Unix

let now () =
  let tm = gmtime (time ()) in
  { annee = 1900 + tm.tm_year;
    mois = tm.tm_mon + 1;
    jour = tm.tm_mday;
    fraction_jour = (float tm.tm_hour +. float tm.tm_min /. 60. +.
		     float tm.tm_sec /. 3600.) /. 24. }
