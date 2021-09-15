
let x  =
  let jj = 2443825.5  in
  let t = (jj -. 2415020.) /. 36525. (* 0.788651608487 *) in
  let th0 = 6.6460656 +. 2400.051262 *. t +. 0.00002581 *. t *. t in
  let ts = th0 -. 24. *. floor (th0 /. 24.) in
  let h = floor ts in
  let ts' = (ts -. h) *. 60. in
  let m = floor ts' in
  (ts' -. m) *. 60.

let _ =
  Printf.printf "%E\n" (x -. 1.33157320077)
