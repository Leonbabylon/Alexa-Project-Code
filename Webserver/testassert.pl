dynamic sessionid_fact/2.


populateDb(X):-
			assertz(sessionid_fact(member(h(nor,_,dog), Hs),Hs)),
			assertz(sessionid_fact(member(h(pig,fat,head),Hs),Hs)),
			assertz(sessionid_fact(member(h(_,mem,_),Hs),Hs)),
			X = "asserted facts".


checkDb(Hsolved):-
	length(Hs, 2),
	findall((Rule,Hz),sessionid_fact(Rule,Hz),X),
	portray_clause(user_error,X),
	forall(X,(_,Hs)),
	%maplist((member(h(_,_,_),Hs),Hs),X),
	portray_clause(user_error,Hs).
