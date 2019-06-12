%{
open Turtle;;
open Bracketed;;
%}

%token INIT HERED INTER EOF
%token LBRANCH RBRANCH
%token VRG PV
%token DPOINT
%token ARROW
%token <char> SYMBOL
%token <int> INT
%token MOVE LINE TURN
%start main
%type <Bracketed.bracketed * Bracketed.rewriting_system * Bracketed.interpretation> main

%%

main		: initbloc heredbloc interbloc EOF
				{ $1 , $2 , $3}
			;

initbloc	: INIT DPOINT init                  
				{ $3 }
			;

init		: lsys PV                   
				{ $1 }
			;
	
heredbloc	: HERED DPOINT hered
				{ $3 }
			;

hered		: SYMBOL ARROW lsys PV
				{ [ ($1 , $3 ) ] }
			| SYMBOL ARROW lsys PV hered
				{ ($1 , $3 ) :: $5 }
			;

interbloc	: INTER DPOINT inter
				{ $3 }
			;

inter		: SYMBOL ARROW commandlist PV
				{ [ ($1 , $3) ] }
			| SYMBOL ARROW commandlist PV inter
				{ ($1 , $3) :: $5 }
			;

lsys		: listlsys
				{ Sequence $1 }
			;

listlsys	: SYMBOL
				{ [Sym $1] }
			| SYMBOL listlsys
				{ (Sym $1) :: $2 }
			| LBRANCH listlsys RBRANCH
				{ [Branch (Sequence $2)] }
			| LBRANCH listlsys RBRANCH listlsys
				{ [Branch (Sequence $2) ; Sequence $4] (* (Branch (Sequence $2)) :: $4 *) }
			;

commandlist	: command
				{ [ $1 ] }
			| command VRG commandlist
				{ $1 :: $3 }
			;

command		: MOVE INT
				{ Move $2 }
			| LINE INT
				{ Line $2 }
			| TURN INT
				{ Turn $2 }
			;
