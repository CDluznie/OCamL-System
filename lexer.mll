{
	open Parser        (* The type token is defined in parser.mli *)
	exception Eof
}

rule token = parse
	  [' ' '\t']     
		{ token lexbuf }
	| ['\n' ]        
		{ token lexbuf }
	| "Init"
		{ INIT }
	| "Hered"
		{ HERED }
	| "Inter"
		{ INTER }
	| eof
		{ EOF }
	| '['
		{ LBRANCH }
	| ']'
		{ RBRANCH }
	| ','
		{ VRG }
	| ';'
		{ PV }
	| ':'
		{ DPOINT }
	| "->"
		{ ARROW }
	| ['A'-'Z'] as s
		{ SYMBOL (s) }
	| '-'?['0'-'9']+ as n 
		{ INT (int_of_string n) }
	| "Move"
		{ MOVE }
	| "move"
		{ MOVE }
	| "MOVE"
		{ MOVE }
	| "Line"
		{ LINE }
	| "line"
		{ LINE }
	| "LINE"
		{ LINE }
	| "Turn"
		{ TURN }
	| "turn"
		{ TURN }
	| "TURN"
		{ TURN }
	| eof
		{ raise Eof }
