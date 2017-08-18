%:- use_module(library(render)).
%:- use_rendering(table,
%		 [header(h('Owner', 'Pet', 'Cigarette', 'Drink', 'Color'))]).


dynamic sessionid_fact/2.


	%processfacts(X).
	%forall(X,(_,Hs)),


	%maplist((member(h(_,_,_),Hs),Hs),X),
	%portray_clause(user_error,Hs).

asserter(X):-
	  assertz(sessionid_fact(member(h(english,_,_,_,red), Hs),Hs)),
		assertz(sessionid_fact(member(h(spanish,dog,_,_,_), Hs),Hs)),
		assertz(sessionid_fact(member(h(_,_,_,coffee,green), Hs),Hs)),
		assertz(sessionid_fact(member(h(ukrainian,_,_,tea,_), Hs),Hs)),
		assertz(sessionid_fact(member(h(_,snake,winston,_,_), Hs),Hs)),
		assertz(sessionid_fact(member(h(_,_,kool,_,yellow),Hs), Hs)),
		assertz(sessionid_fact(member(h(_,_,lucky,juice,_), Hs),Hs)),
		assertz(sessionid_fact(member(h(japonese,_,kent,_,_), Hs),Hs)),
		assertz(sessionid_fact(next(h(_,_,_,_,green), h(_,_,_,_,white), Hs),Hs)),
		assertz(sessionid_fact(next(h(_,fox,_,_,_), h(_,_,chesterfield,_,_), Hs),Hs)),
		assertz(sessionid_fact(next(h(_,_,kool,_,_), h(_,horse,_,_,_), Hs),Hs)),
		assertz(sessionid_fact(next(h(norwegian,_,_,_,_), h(_,_,_,_,blue), Hs),Hs)),
		assertz(sessionid_fact(Hs = [_,_,h(_,_,_,milk,_),_,_],Hs)),
		assertz(sessionid_fact(Hs = [h(norwegian,_,_,_,_)|_],Hs)),
		assertz(sessionid_fact(member(h(_,_,_,water,_), Hs),Hs)),
		assertz(sessionid_fact(member(h(_,zebra,_,_,_), Hs),Hs)),
		X = "done".

zebra_owner(Query,Owner) :-
	houses(Hs),
	member(h(Owner,Query,_,_,_), Hs).

water_drinker(Drinker) :-
	houses(Hs),
	member(h(Drinker,_,_,water,_), Hs).

houses(Hs) :-
	length(Hs, 5),
	findall((Rule,Hz),sessionid_fact(Rule,Hz),X),
	portray_clause(user_error,X),
	read_term_from_atom_(X,Hs).
	%maplist(read_term_from_atom_,Hs,X).

read_term_from_atom_([]).
read_term_from_atom_([X],Hs) :-
			writeln(user_error,"wtf"),
			X = (Rule,Hs),
			Rule.
read_term_from_atom_([X|T],Hs) :-
    	X = (Rule,Hs),
			Rule,
			read_term_from_atom_(T,Hs).


next(A, B, Ls) :- append(_, [A,B|_], Ls).
next(A, B, Ls) :- append(_, [B,A|_], Ls).

%processfacts([],Hz).
%processfacts([H|T],Hz) :- process(H,Hz), processfacts(T,Hz).
%process(Z,C) :-
