open Genlex;;
open List;;
open Char;;
open Graphics;;
open Geometric;;

type turtle =
	int * int * int * color
;;

type turtle_command =
  | Move of int
  | Line of int
  | Turn of int
;;

let new_turtle : int -> int -> int -> color-> turtle = (fun x y a c ->
	x , y , a , c
);;

let reset : turtle -> unit = (fun t ->
	let x , y , _ , _ = t
	in
	moveto x y ;
);;

let move x y a n =
	let pi = 3.1415926535
	and n = float_of_int n
	in
	let alpha = (((float_of_int a) +. 90.) *. pi) /. 180.
	in
	x + int_of_float (n *. (cos alpha)) , y + int_of_float (n *. (sin alpha))
;; 

let execute_command : turtle -> turtle_command -> turtle * geobject = (fun t c ->
	let x , y , a , col = t
	in
	match c with
		| Move n -> 
			let x' , y' = move x y a n
			in (x' , y' , a , col) , Void
		| Line n -> 
			let x' , y' = move x y a n
			in (x' , y' , a , col) , Line (Point (x,y) , Point (x',y'))
		| Turn a' ->
			(x , y , (a + a') , col) , Void 
);;

let string_of_command c =
	match c with
		| Move n -> "Move " ^ (string_of_int n)
		| Line n -> "Line " ^ (string_of_int n)
		| Turn n -> "Turn " ^ (string_of_int n)
;;

let rec execute_command_list t c = 
	match c with
		| [] -> 
			t , empty_drawing
		| head :: tail ->  
			let t' , geo = execute_command t head in
			let t'' , drw = execute_command_list t' tail in 
			t'' , add drw geo
;;

let pgcd l =
	let rec pgcd' a b =
		let r = a mod b
		in
		if r = 0 then
			b
		else
			(pgcd' b r)
	in
	fold_left pgcd' (hd l) (tl l)
;;

let reduce_scale_command_list list_of_command_list =
	let res =
		pgcd
			(map
				begin fun c ->
					match c with
								| Move n -> n
								| Line n -> n
								| _ -> failwith ""
				end
				(filter 
					begin fun c ->
						match c with
							| Move _ -> true
							| Line _ -> true
							| _ -> false
					end
					(flatten list_of_command_list)
				)
			)
	in
	map
		begin fun lc ->
			map 
				begin function c ->
					match c with
						| Move n -> Move (n/res)
						| Line n -> Line (n/res)
						| Turn n -> Turn n
					
				end
				lc
		end
		list_of_command_list
;;

let string_of_command_list rs =
	fold_left
		begin fun acc e ->
			acc ^ ", " ^ (string_of_command e)
		end
		(string_of_command (hd rs)) (tl rs)
;;

let fully_string_of_command_list rs =
	"[" ^
	fold_left
		begin fun acc e ->
			acc ^ " ; " ^ (string_of_command e)
		end
		(string_of_command (hd rs)) (tl rs)
	^ "]"
;;
