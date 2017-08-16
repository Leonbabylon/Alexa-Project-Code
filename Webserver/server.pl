:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(random)).
:- use_module(library(listing)).

:- dynamic sessionid_fact/2. %hmmm
:- dynamic sessionid_fact/3.
:- dynamic '$copy'/1.				 %hmmm
:- op(600, xfy, '=>').			 %hmmm


:- http_handler(/, alexa, [prefix]).

handle_request(_Request) :-
	alexa(Request).

alexa(Request):-
	http_read_json_dict(Request,DictIn),
	handle_dict(DictIn,DictOut),
	%my_json_answer(hello,DictOut),
	reply_json(DictOut).


handle_dict(DictIn,DictOut) :-
	setup_call_cleanup(
			   open('recieved.txt',append,Stream,[]),
			   (get_id(DictIn,Id),
%			    print(Id),
			    format(Stream,"Id: ~w\n",[Id])),
			   close(Stream)
			  ),
	application_id(Id),
	get_intent(DictIn,IntentName),
	intent_dictOut(IntentName,DictIn,DictOut).

handle_dict(_DictIn,DictOut):-
	DictOut = _{
	      shouldEndSession: false,
	      response: _{outputSpeech:_{type: "PlainText", text: "Error Id did not match"}},
              version:"1.0"
	     }.



get_intent(DictIn,IntentName):-
	get_dict(request,DictIn,RequestObject),
	get_dict(intent,RequestObject,IntentObject),
	get_dict(name,IntentObject,IntentName).

/*
 *  Steps needed
* 1. check the app id
  2. Check the time stamp
* 3. Make the json responce
*/

intent_dictOut("smell",_,DictOut):-
	my_json_answer("lina sucks at mario kart", DictOut).

intent_dictOut("getANewFact",_,DictOut):-
	answers(RandomMessage),
	my_json_answer(RandomMessage,DictOut).

intent_dictOut("remember",DictIn,DictOut):-
	get_dict(session,DictIn,SessionObject),
	get_dict(sessionId,SessionObject,SessionId),
	get_dict(request,DictIn,RequestObject),
	get_dict(intent,RequestObject,IntentObject),
	get_dict(slots,IntentObject,SlotsObject),
	get_dict(rememberSlot,SlotsObject,MySlotObject),
	get_dict(value,MySlotObject,Value),
	split_string(Value," ","",StringList),
	maplist(string_lower,StringList,StringListLow),
	maplist(atom_string,AtomList,StringListLow),
	(phrase(sentence(Rule),AtomList) ->
	 (assertz(sessionid_fact(SessionId,Rule)),
	  my_json_answer(Value,DictOut));
	  my_json_answer(Value,DictOut)).

intent_dictOut("question",DictIn,DictOut):-
	writeln(user_error,walrus),
	get_dict(session,DictIn,SessionObject),
	get_dict(sessionId,SessionObject,SessionId),
	get_dict(request,DictIn,RequestObject),
	get_dict(intent,RequestObject,IntentObject),
	get_dict(slots,IntentObject,SlotsObject),
	get_dict(questionSlot,SlotsObject,MySlotObject),
	get_dict(value,MySlotObject,Value),
	portray_clause(user_error,Value),
	((
	  split_string(Value," ","",StringList),
	  maplist(string_lower,StringList,StringListLow),
	  maplist(atom_string,AtomList,StringListLow),

	  phrase(question(Query),AtomList),prove_question(Query,SessionId,Answer)) ->
	 my_json_answer(Answer,DictOut);
	 my_json_answer(Value,DictOut)
	).
intent_dictOut("directmember",DictIn,DictOut):-
		writeln(user_error,fishboy),
		get_dict(session,DictIn,	SessionObject),
		writeln(user_error,hmmm),
		get_dict(sessionId,SessionObject,SessionId),
		writeln(user_error,hmmm),
		get_dict(request,DictIn,RequestObject),
		writeln(user_error,hmmm),
		get_dict(intent,RequestObject,IntentObject),
		writeln(user_error,hmmm),
		get_dict(slots,IntentObject,SlotsObject),
		writeln(user_error,hmmm),
		get_dict(member,SlotsObject,Valuem),
		writeln(user_error,hmmm),
		get_dict(property,SlotsObject,Valuep),
		writeln(user_error,hmmm),
		downcase_atom(Valuem,MemberLow),
		writeln(user_error,hmmm),
		dowmcase_atom(Valuep,PropLow),
		writeln(user_error,hmmm),
		writeln(user_error,oK),
		directmember(MemberLow,PropLow,R),
		assertz(sessionid_fact(SessionId,member(R, Hs),Hs)),
		my_json_answer("direct fact accepted",DictOut).


/*
intent_dictOut("nextmember",DictIn,DictOut):-
		writeln(user_error,slark),
		get_dict(session,DictIn,SessionObject),
		get_dict(sessionId,SessionObject,SessionId),
		get_dict(request,DictIn,RequestObject),
		get_dict(intent,RequestObject,IntentObject),
		get_dict(slots,IntentObject,SlotsObject),
*/



intent_dictOut(_,_,DictOut):-
	my_json_answer('Error parsing',DictOut).

prove_question(Query,SessionId,Answer):-
	findall(Rule,sessionid_fact(SessionId,Rule),Rulebase),
	prove_rb(Query,Rulebase),
	transform(Query,Clauses),
	phrase(sentence(Clauses),AnswerAtomList),
	atomics_to_string(AnswerAtomList," ",Answer).



get_id(Dict,Id):-
	get_dict(session,Dict,SessionObject),
	get_dict(application,SessionObject,ApplicationObject),
	get_dict(applicationId,ApplicationObject,Id).

application_id(X):-
	X= "amzn1.ask.skill.dcc7c1a0-8ac6-4bd1-8ba1-78a56e8313c4".

my_json_answer(Message,X):-
	X = _{
	      response: _{
			  shouldEndSession: false,
			  outputSpeech:_{type: "PlainText", text: Message}
			 },
              version:"1.0"

	     }.

answers(X):-
	random_member(X,["lina married nick crompton in 2028", "lina likes aubergines", "lina eats molluscum", "linas live in herds","linas have two large tusks"]).

weirdstuff(X):-
	X = "haha".



sentence(C) --> determiner(N,M1,M2,C),
                noun(N,M1),
                verb_phrase(N,M2).

sentence([(L:-true)]) --> proper_noun(N,X),
                          verb_phrase(N,X=>L).

verb_phrase(s,M) --> [is],property(s,M).
verb_phrase(p,M) --> [are], property(p,M).

property(s,M) --> [a], noun(s,M).
property(p,M) --> noun(p,M).

property(_N,X=>mortal(X)) --> [mortal].

determiner(s,X=>B,X=>H,[(H:-B)]) --> [every].
determiner(p, sk=>H1, sk=>H2, [(H1:-true),(H2 :- true)]) -->[some].

proper_noun(s,leon) --> [leon].
noun(s,X=>human(X)) --> [human].
noun(p,X=>human(X)) --> [humans].
noun(s,X=>living_being(X)) --> [living],[being].
noun(p,X=>living_being(X)) --> [living],[beings].


question(Q) --> [who],[is], property(s,_X=>Q).
question(Q) --> [is], proper_noun(N,X),
                property(N,X=>Q).
question((Q1,Q2)) --> [are],[some],noun(p,sk=>Q1),
	property(p,sk=>Q2).

prove_rb(true,_Rulebase):-!.
prove_rb((A,B),Rulebase):-!,
    prove_rb(A,Rulebase),
    prove_rb(B,Rulebase).

prove_rb(A,Rulebase):-
    find_clause((A:-B),Rulebase),
    prove_rb(B,Rulebase).

find_clause(Clause,[Rule|_Rules]):-
    my_copy_element(Clause,Rule).

find_clause(Clause,[_Rule|Rules]):-
    find_clause(Clause,Rules).

transform((A,B),[(A:-true)|Rest]):-!,
    transform(B,Rest).

transform(A,[(A:-true)]).


get_input(Input):-
    write('? '), flush, read(Input).

show_answer(Answer):-
    write('! '), flush, write(Answer),nl.

my_copy_element(X,Ys):-
    member(X1,Ys),
    copy_term(X1,X).


		zebra_owner(Owner) :-
			houses(Hs),
			member(h(Owner,zebra,_,_,_), Hs).

		water_drinker(Drinker) :-
			houses(Hs),
			member(h(Drinker,_,_,water,_), Hs).


		houses(Hs) :-
			% each house in the list Hs of houses is represented as:
			%      h(Nationality, Pet, Cigarette, Drink, Color)
			length(Hs, 5),                                            %  1
			member(h(english,_,_,_,red), Hs),                         %  2
			member(h(spanish,dog,_,_,_), Hs),                         %  3
			member(h(_,_,_,coffee,green), Hs),                        %  4
			member(h(ukrainian,_,_,tea,_), Hs),                       %  5
			next(h(_,_,_,_,green), h(_,_,_,_,white), Hs),             %  6
			member(h(_,snake,winston,_,_), Hs),                       %  7
			member(h(_,_,kool,_,yellow), Hs),                         %  8
			Hs = [_,_,h(_,_,_,milk,_),_,_],                           %  9
			Hs = [h(norwegian,_,_,_,_)|_],                            % 10
			next(h(_,fox,_,_,_), h(_,_,chesterfield,_,_), Hs),        % 11
			next(h(_,_,kool,_,_), h(_,horse,_,_,_), Hs),              % 12
			member(h(_,_,lucky,juice,_), Hs),                         % 13
			member(h(japonese,_,kent,_,_), Hs),                       % 14
			next(h(norwegian,_,_,_,_), h(_,_,_,_,blue), Hs),          % 15
			member(h(_,_,_,water,_), Hs),		% one of them drinks water
			member(h(_,zebra,_,_,_), Hs).		% one of them owns a zebra

		next(A, B, Ls) :- append(_, [A,B|_], Ls).
		next(A, B, Ls) :- append(_, [B,A|_], Ls).


direct_member(M,P,Z):-
			nationalities(M,P,Na),
			ciggies(M,P,Ci),
			colours(M,P,Co),
			pets(M,P,Pe),
			drinks(M,P,Dr),
    	replace([],Na,_,Nae),
    	replace([],Ci,_,Cie),
			replace([],Co,_,Coe),
    	replace([],Pe,_,Pee),
    	replace([],Dr,_,Dre),
    	R = [Nae,Pee,Cie,Dre,Coe],
			Z = h(R).



nationalities(M,P,R):-
			(
			Na = [ukrainian,englishman,spaniard,norwegian,japanese],
			intersection([M,P],Na,R)
			).

ciggies(M,P,R):-
			(
			Ci = [kools,lucky_strike,parliaments,chesterfields,old_gold],
			intersection([M,P],Ci,R)
			).

colours(M,P,R):-
			(
			Co = [red,green,white,blue,yellow],
			intersection([M,P],Co,R)
			).

pets(M,P,R):-
			(
			Pe = [fox,dog,horse,snails,zebra],
			intersection([M,P],Pe,R)
			).

drinks(M,P,R):-
			(
			Dr = [coffee,tea,milk,orange_juice,water],
			intersection([M,P],Dr,R)
			).

replace(Element, Element, NElement, NElement).
replace(_, X, _, X).
