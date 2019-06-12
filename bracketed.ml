open Turtle;;
open Char;;
open List;;
open Geometric;;

type symbol = char;;

type bracketed =
  | Sym of symbol
  | Sequence of bracketed list
  | Branch of bracketed
;;

type rewriting_system = (symbol * bracketed) list;;

type interpretation = (symbol * turtle_command list) list;;

let string_of_symbol : symbol -> string = (fun s ->
	escaped s
);;

let rec fully_string_of_bracketed b =
	match b with
			| Sym x -> 
				"Sym " ^ string_of_symbol x
			| Sequence s -> 
				"Sequence [" ^
				fold_left 
					begin fun x y ->
						x ^ " ; " ^ (fully_string_of_bracketed y)
					end
					(fully_string_of_bracketed (hd s)) (tl s)
				^ "]"
			| Branch b -> 
				"Branch (" ^ fully_string_of_bracketed b ^ ")"
;;

let rec string_of_bracketed b =
	match b with
		| Sym x -> 
			string_of_symbol x
		| Sequence s -> 
			fold_left 
				begin fun x y ->
					x ^ " " ^ (string_of_bracketed y)
				end
				(string_of_bracketed (hd s)) (tl s)
		| Branch b -> 
			"[" ^ string_of_bracketed b ^ "]"
;;

let rewrite_symbol : symbol -> rewriting_system -> bracketed = (fun s rs ->
	fold_left
		begin fun acc (sym,trad) ->
			if s = sym then
				trad
			else
				acc
		end
		(Sym s) rs
);;

let rec rewrite_bracketed b rs =
	match b with
		| Sym x -> rewrite_symbol x rs
		| Sequence s -> Sequence (map (fun x -> rewrite_bracketed x rs) s)
		| Branch b -> Branch (rewrite_bracketed b rs)
;;

let fully_string_of_rewriting_system rs =
	let tostr (sym,brack) =
		"(" ^ (string_of_symbol sym) ^ ", " ^ (fully_string_of_bracketed brack) ^ ")"
	in
	"[" ^
	fold_left
		begin fun acc e ->
			acc ^ " ; " ^ (tostr e)
		end
		(tostr (hd rs)) (tl rs)
	^ "]"
;;

let string_of_rewriting_system rs =
	let tostr (sym,brack) =
		(string_of_symbol sym) ^ " -> " ^ (string_of_bracketed brack)
	in
	fold_left
		begin fun acc e ->
			acc ^ "\n" ^ (tostr e)
		end
		(tostr (hd rs)) (tl rs)
;;

let rec iterate n b rs =
	if n <= 0 then
		b
	else
		iterate (n-1) (rewrite_bracketed b rs) rs
;;

let interprate_symbol : symbol -> interpretation -> turtle_command list = (fun s com ->
	fold_left
		begin fun acc (sym,instr) ->
			if s = sym then
				instr
			else
				acc
		end
		[] com
);;

let rec interprate brack turtle inter =
	match brack with
		| Sym x ->
			execute_command_list turtle (interprate_symbol x inter)
		| Sequence s ->
			fold_left
				begin fun (trtl,drw) el ->
					let trtl' , drw' = interprate el trtl inter
					in
					(trtl' , fusion drw drw')
				end
				(turtle , empty_drawing) s
		| Branch b ->
			let _ , drw = interprate b turtle inter
			in
			turtle , drw
;;

let drawing_of_bracketed brack turtle inter =
	let _ , d = interprate brack turtle inter
	in
	d
;;

let string_of_interpretation rs =
	let tostr (sym,inter) =
		(string_of_symbol sym) ^ " -> " ^ (string_of_command_list inter)
	in
	fold_left
		begin fun acc e ->
			acc ^ "\n" ^ (tostr e)
		end
		(tostr (hd rs)) (tl rs)
;;

let fully_string_of_interpretation rs =
	let tostr (sym,inter) =
		"(" ^ (string_of_symbol sym) ^ ", " ^ (fully_string_of_command_list inter) ^ ")"
	in
	"[" ^
	fold_left
		begin fun acc e ->
			acc ^ " ; " ^ (tostr e)
		end
		(tostr (hd rs)) (tl rs)
	^ "]"
;;
