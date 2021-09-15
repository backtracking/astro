
type spectral_type = O | B | A | F | G | K | M | C | S | W | P

type star = {
  name : string;
  ascension : float;
  declination : float;
  magnitude : float;
  spectral_type : spectral_type;
  color : float * float * float (* each in 0..1 *)
}

val read_file : string -> star list
