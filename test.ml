open Io;;
include Io;;

(* Tests *)

(* p(x) \/- !p(x) *)
let my_f1 = Neg(
  OrN(
    Pos(
      PosAtom("p",
              [V("x")])
    ),
    Neg(
      NegAtom("p",
              [V("x")])
    )
  )
)
;;

(* p(x) \/+ !p(x) *)
let my_f2 = Pos(
  OrP(
    Pos(
      PosAtom("p",
              [V("x")])
    ),
    Neg(
      NegAtom("p",
              [V("x")])
    )
  )
)
;;

(* !p(x) \/+ p(x) : tourne en boucle si recherche de preuve en profondeur*)
let my_f3 = Pos(
  OrP(
    Neg(
      NegAtom("p",
	      [])
    ),
    Pos(
      PosAtom("p",
	      [])
    )
  )
)
;;

(* (a \/- b) \/- (!a /\- !b) *)

let my_f4 = Neg(
  OrN(
    Neg(
      OrN(
        (Pos(
           PosAtom("a", []))),
        (Pos(
           PosAtom("b", [])))
      )
    ),
    Neg(
      AndN(
        Neg(
          NegAtom("a", [])), 
        Neg(
          NegAtom("b", []))
      )
    )
  )
)
;; 

(* (a \/+ b) \/- (!a /\- !b) *)
let my_f5 = Pos(
  OrP(
    Pos(
      OrP(
        (Pos(
           PosAtom("a", []))),
        (Pos(
           PosAtom("b", [])))
      )
    ),
    Neg(
      AndN(
        Neg(
          NegAtom("a", [])), 
        Neg(
          NegAtom("b", []))
      )
    )
  )
)
;; 

(* (!a \/+ !b) pas prouvable - algo naif tourne en boucle *)
let my_f6 = Pos(
  OrP(
    Neg(
      NegAtom("a", [])), 
    Neg(
      NegAtom("b", []))
  )
)
;; 

(* old code
let my_z = add_left my_f5 empty_zip;;
let my_e = EntUF(empty_zip, my_z, empty_zip, []);;
let my_e = EntUF((add_left ("p",[]) empty_zip), my_z, empty_zip, []);;
*)

let orig_seq my_formula = EntUF(empty_zip,(add_left my_formula empty_zip), empty_zip, []);;

let orig_situation my_seq = fun () -> lk_solve (fun x->x) false my_seq;;

let go formula = 
  print_endline("===");
  failcount := 0;
  tiredcount := 0;
  successcount := 0;
  let h = orig_seq formula in solve (add_left (orig_situation h,h) empty_zip);;

write_to_file "latex/eurecaml.tex" (printanswer (go my_f1)^"\\vspace{30pt}"^
				      (printanswer (go my_f2))^"\\vspace{30pt}"^
				      (printanswer (go my_f3))^"\\vspace{30pt}"^
				      (printanswer (go my_f4))^"\\vspace{30pt}"^
				      (printanswer (go my_f5))^"\\vspace{30pt}"^
				      (printanswer (go my_f6))
				   )
;;


(*
let treatfile filename = print_list  (list_from_string (read_from_file(filename)) [] 0);;
*)

let treatfile filename =  print_string ((read_from_file(filename)));;
let treatfile filename = print_endline(printformula (perp(generate_cnf(parse_cnf_file (list_from_string (read_from_file(filename)) [] 0)))));;
let treatfile filename = go (perp(generate_cnf(parse_cnf_file (list_from_string (read_from_file(filename)) [] 0))));;


let treatdir a = 
  let b = Sys.readdir(a) in
  for i=0 to Array.length b-1 do
    treatfile (a^Filename.dir_sep^b.(i));
    done
;;

(* treatdir("sat-2002-beta/generated/gen-9/gen-9.1");; *)

treatfile "test.cnf";;
(* write_to_file "latex/eurecaml.tex" (printanswer (treatfile "test.cnf"));;*)

