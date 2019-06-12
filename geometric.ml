open List;;
open Graphics;;

type point = Point of int * int;;

type geobject =
	| Void
	| Line of point * point;;

type drawing = geobject list;;

let string_of_geobject g =
	match g with
		| Void -> 
			"void"
		| Line (Point(x,y),Point(x',y')) ->
			let x = string_of_int x
			and x' = string_of_int x'
			and y = string_of_int y
			and y' = string_of_int y'
			in
			"(" ^ x ^ "," ^ y ^ ") - (" ^ x' ^ "," ^ y' ^ ")"
;;

let empty_drawing : drawing = 
	[]
;;

let add : drawing -> geobject -> drawing = (fun d g ->
	match g with
		| Line _ -> g :: d
		| Void -> d
);;

let fusion : drawing -> drawing -> drawing = (fun d d' ->
	d @ d'
);;

let draw : drawing -> unit = (fun d ->
	iter 
		begin fun g ->
			match g with
				| Line (Point (x,y) , Point (x',y')) -> 
					moveto x y;
					lineto x' y';
				| Void -> 
					()
		end
		d
);;

let nb_line_drawing : drawing -> int = (fun d ->
	length d
);;

let min_max_point : drawing -> int * int * int * int = (fun d ->
	let points =
		fold_left
			begin fun acc g ->
				match g with
					| Void -> acc
					| Line (p,p') -> p :: p' :: acc
				end
				[] d
	in
	let Point (x , y) = hd points
	in
	fold_left
		begin fun acc p ->
			let x,x',y,y' = acc
			and Point (xp,yp) = p
			in min x xp , max x' xp , min y yp , max y' yp
		end
		(x,x,y,y) (tl points)
);;

let size_drawing d =
	let x,x',y,y' = min_max_point d in
	x' - x , y' - y
;;

let partition d =
	let d = map
		begin fun g -> 
			match g with
				| Line(Point(x,y),Point(x',y')) -> x' - x , y' - y , [g]
				| _ -> failwith ""
		end
		d
	in let rec aux l =
		match l with
			| [] -> 
				[]
			| [x] -> 
				[[x]]
			| (x,y,l) :: t ->
				let t = aux t in 
				let x',y',_ = hd (hd t) in
				(* *)
				if (compare (x,y) (x',y')) = 0 then
					t
				else
					t
	in
	aux (sort compare d)
;;

let map_drawing f (d : drawing) = 
	map begin fun g ->
		match g with 
		| Void -> Void
		| Line (Point(x,y),Point(x',y')) -> 
			let x,y = f (x,y) in
			let x',y' = f (x',y') in
			Line (Point(x,y),Point(x',y'))
	end d
;;

let string_of_drawing rs =
	if rs = [] then
		""
	else
		fold_left
			begin fun acc e ->
				acc ^ "\n" ^ (string_of_geobject e)
			end
			(string_of_geobject (hd rs)) (tl rs)
;;
