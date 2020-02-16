
let trace (s: string) (v: 'a): 'a =
  (* print_string s; *)
  let out = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 "/tmp/log.fstar.trace" in
  output_string out s;
  close_out out;
  v

(* let main =
 *   trace "hey" "xo";
 *   trace "hoho" 234;
 *   () *)
