
type spectral_type = O | B | A | F | G | K | M | C | S | W | P

type star = {
  name : string;
  ascension : float;
  declination : float;
  magnitude : float;
  spectral_type : spectral_type;
  color : float * float * float (* each in 0..1 *)
}

let pi = 3.14159265358979323846
let _180_sur_pi = 180. /. pi
let degres r = _180_sur_pi *. r

let spectral_type_color = function
  | O -> 0.8, 0.8, 1.0
  | B -> 0.9, 0.9, 1.0
  | A -> 1.0, 1.0, 1.0
  | F -> 1.0, 1.0, 0.8
  | G -> 1.0, 1.0, 0.7
  | K -> 1.0, 0.9, 0.8
  | M | C | S -> 1.0, 0.6, 0.6
  | W | P -> 1.0, 1.0, 1.0 (* ??? *)

let star_color m st =
  let r,g,b = spectral_type_color st in
  let m = 3.0 *. 2.42 /. ((2.46 +. m) *. 2.42) in
  let m = m *. 1.2 in
  let m = if m > 1.0 then 1.0 else m in
  r *. m, g *. m, b *. m

let spectral_type = function
  | 'O' -> O
  | 'B' -> B
  | 'A' -> A
  | 'F' -> F
  | 'G' -> G
  | 'K' -> K
  | 'M' -> M
  | 'C' -> C
  | 'S' -> S
  | 'W' -> W
  | 'p' -> P
  | c -> assert false

let read_file f =
  let cin = open_in f in
  let stars = ref [] in
  try
    while true do
      let s = input_line cin in
      let n = String.length s in
      if n > 0 && s.[0] <> '#' then begin
	let i = String.index s ',' in
	let star =
	  Scanf.sscanf (String.sub s (succ i) (n - i - 1)) "%f,%f,%f,%c"
	    (fun a d m st ->
	       let st = spectral_type st in
	       { name = String.sub s 0 i;
		 ascension = degres a; declination = degres d; magnitude = m;
		 spectral_type = st; color = star_color m st })
	in
	stars := star :: !stars
      end
    done;
    assert false
  with End_of_file ->
    close_in cin;
    List.rev !stars
