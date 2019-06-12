open Bracketed;;
open Turtle;;
open Sys;;
open Graphics;;
open Geometric;;

let draw_system system inter alpha_init =
	let color = blue in
	let border = 15 in
	let drawing = drawing_of_bracketed system (new_turtle 0 0 alpha_init color) inter in
	let minx,maxx,miny,maxy = min_max_point drawing in
	let window_width = maxx - minx + 2*border in
	let window_height = maxy - miny + 2*border in
	let centered_drawing = map_drawing begin fun (x,y) ->
		(x-minx+border, y-miny+border)
	end drawing in
	resize_window window_width window_height;
	draw centered_drawing;	
;;

let lsys init hered inter n alpha =
	let system = iterate n init hered in
	open_graph "";
	draw_system system inter alpha;
	ignore (wait_next_event [Button_down]);
	close_graph ()
;;

let _ =
	if Array.length argv != 3 && Array.length argv != 4 then
		failwith ("Usage : " ^ argv.(0) ^ " filename n [angle]")
	else try
		let file_drawing = open_in argv.(1) in 
		let init , hered , inter = Parser.main Lexer.token (Lexing.from_channel file_drawing) in
		let n = int_of_string argv.(2) in
		let angle = 
			if Array.length argv == 4 then
				int_of_string argv.(3)
			else
				0
		in
		lsys init hered inter n angle		
	with 
	| Graphics.Graphic_failure _ -> ()
	| Failure "int_of_string" ->
		print_string "argument 'n' and 'angle' have to be numbers";
		print_newline()
	| Sys_error err 
	| Failure err ->
		print_string err;
		print_newline()

;;
