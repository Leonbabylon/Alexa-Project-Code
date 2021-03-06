:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(random)).
:- use_module(library(listing)).
:- use_module(library(lists)).

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
	reply_json(DictOut).


handle_dict(DictIn,DictOut) :-
	setup_call_cleanup(
			   open('recieved.txt',append,Stream,[]),
			   (get_id(DictIn,Id),
			    format(Stream,"Id: ~w\n",[Id])),
			   close(Stream)
			  ),
	application_id(Id),
	get_intent(DictIn,IntentName),
	intent_dictOut(IntentName,DictIn,DictOut).

handle_dict(_DictIn,DictOut):-
	DictOut = _{
	      shouldEndSession: false,
	      response: _{outputSpeech:_{type: "PlainText", text: "Skill Id did not match"}},
              version:"1.0"
	     }.


get_intent(DictIn,IntentName):-
	get_dict(request,DictIn,RequestObject),
	get_dict(intent,RequestObject,IntentObject),
	get_dict(name,IntentObject,IntentName).


intent_dictOut("getANewFact",_,DictOut):-
	answers(RandomMessage),
	my_json_answer(RandomMessage,DictOut).

intent_dictOut("directmember",DictIn,DictOut):-
		writeln(user_error,fishboy),
		get_dict(session,DictIn,	SessionObject),
		get_dict(sessionId,SessionObject,SessionId),
		get_dict(request,DictIn,RequestObject),
		get_dict(intent,RequestObject,IntentObject),
		get_dict(slots,IntentObject,SlotsObject),
		get_dict(member,SlotsObject,Valuem),
		get_dict(property,SlotsObject,Valuep),
		get_dict(value,Valuem,Member),
		get_dict(value,Valuep,Property),
		string_lower(Member,MemberLow),
		atomic_list_concat(Words, ' ', MemberLow),
		atomic_list_concat(Words, '_', Atomember),
		string_lower(Property,PropLow),
		atomic_list_concat(Words1, ' ', PropLow),
		atomic_list_concat(Words1, '_', Atoprop),
		direct_member(Atomember,Atoprop,R),
		portray_clause(user_error,R),
		assertz(sessionid_fact(SessionId,member(R, Hs),Hs)),
		writeln(user_error,veryok),
		my_json_answer("direct fact accepted",DictOut).


intent_dictOut("nextmember",DictIn,DictOut):-
		writeln(user_error,slark),
		get_dict(session,DictIn,	SessionObject),
		get_dict(sessionId,SessionObject,SessionId),
		get_dict(request,DictIn,RequestObject),
		get_dict(intent,RequestObject,IntentObject),
		get_dict(slots,IntentObject,SlotsObject),
		get_dict(member,SlotsObject,Valuem),
		get_dict(property,SlotsObject,Valuep),
		get_dict(location,SlotsObject,Valuel),
		get_dict(value,Valuem,Member),
		get_dict(value,Valuep,Property),
		get_dict(value,Valuel,_),
		string_lower(Member,MemberLow),
		atomic_list_concat(Words, ' ', MemberLow),
		atomic_list_concat(Words, '_', Atomember),
		string_lower(Property,PropLow),
		atomic_list_concat(Words1, ' ', PropLow),
		atomic_list_concat(Words1, '_', Atoprop),
		next_member(Atomember,Atoprop,(H1|H2)),
		portray_clause(user_error,H1),
		portray_clause(user_error,H2),
		assertz(sessionid_fact(SessionId,next(H1,H2, Hs),Hs)),             %  6
		writeln(user_error,superok),
		my_json_answer("neighbour fact accepted",DictOut).

intent_dictOut("locationmember",DictIn,DictOut):-
		writeln(user_error,slardar),
		get_dict(session,DictIn,	SessionObject),
		get_dict(sessionId,SessionObject,SessionId),
		get_dict(request,DictIn,RequestObject),
		get_dict(intent,RequestObject,IntentObject),
		get_dict(slots,IntentObject,SlotsObject),
		get_dict(member,SlotsObject,Valuem),
		get_dict(position,SlotsObject,Valuep),
		get_dict(value,Valuem,Member),
		get_dict(value,Valuep,Position),
		string_lower(Member,MemberLow),
		atomic_list_concat(Words, ' ', MemberLow),
		atomic_list_concat(Words, '_', Atomember),
		string_lower(Position,PosLow),
		atomic_list_concat(Words1, ' ', PosLow),
		atomic_list_concat(Words1, '_', Atopos),
		position_member(Atomember,Atopos,Z),
		portray_clause(user_error,Z),
		assertz(sessionid_fact(SessionId,Hs = Z,Hs)),             %  6
		writeln(user_error,megaOk),
		my_json_answer("location fact accepted",DictOut).


intent_dictOut("query",DictIn,DictOut):-
		writeln(user_error,naga),
		get_dict(session,DictIn,	SessionObject),
		get_dict(sessionId,SessionObject,SessionId),
		get_dict(request,DictIn,RequestObject),
		get_dict(intent,RequestObject,IntentObject),
		get_dict(slots,IntentObject,SlotsObject),
		get_dict(queryType,SlotsObject,SlotT),
		get_dict(questionSlot,SlotsObject,SlotQ),
		get_dict(value,SlotQ,ValueQ),
		get_dict(value,SlotT,ValueT),
		string_lower(ValueQ,QLow),
		atomic_list_concat(Words, ' ', QLow),
		atomic_list_concat(Words, '_', AtomQ),
		string_lower(ValueT,TLow),
		atomic_list_concat(Words1, ' ', TLow),
		atomic_list_concat(Words1, '_', AtomT),
		zebra_solve(SessionId,AtomT,AtomQ,Z),            %  6
		writeln(user_error,ultraOK),
		portray_clause(user_error,Z),
		my_json_answer(Z,DictOut).


intent_dictOut(_,_,DictOut):-
	my_json_answer('Error parsing',DictOut).


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
	random_member(X,["bought u some extra time", "i just stopped you timing out", "you have another 8 seconds to say something useful"]).

weirdstuff(X):-
	X = "haha".



zebra_solve(SessionId,who,Query,Result) :-
			length(Hs, 5),
			findall((Rule,Hz),sessionid_fact(SessionId,Rule,Hz),Rulebase),
			processrb(Rulebase,Hs),
			direct_member_query(Query,Result,R),
			member(R, Hs).

zebra_solve(SessionId,where,Query,Result) :-
			length(Hs, 5),
			findall((Rule,Hz),sessionid_fact(SessionId,Rule,Hz),Rulebase),
			processrb(Rulebase,Hs),
			direct_member(Query,Query,R),
			positionator(R,Hs,Result).

zebra_solve(_,_,_,Result) :-
			Result = "query failed".



processrb([]).
processrb([X],Hs) :-
			X = (Rule,Hs),
			Rule.
processrb([X|T],Hs) :-
			X = (Rule,Hs),
			Rule,
			processrb(T,Hs).



next(A, B, Ls) :- append(_, [A,B|_], Ls).
next(A, B, Ls) :- append(_, [B,A|_], Ls).


direct_member(M,P,R):-
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
    	R = h(Nae,Pee,Cie,Dre,Coe),!.

direct_member_query(M,Query,R):-
			ciggies(M,M,Ci),
			colours(M,M,Co),
			pets(M,M,Pe),
			drinks(M,M,Dr),
    	replace([],Ci,_,Cie),
			replace([],Co,_,Coe),
    	replace([],Pe,_,Pee),
    	replace([],Dr,_,Dre),
    	R = h(Query,Pee,Cie,Dre,Coe).



next_member(M,P,(Firsthouse|Secondhouse)):-
			nationalities(M,M,Na),
			ciggies(M,M,Ci),
			colours(M,M,Co),
			pets(M,M,Pe),
			drinks(M,M,Dr),
			replace([],Na,_,Nae),
			replace([],Ci,_,Cie),
			replace([],Co,_,Coe),
			replace([],Pe,_,Pee),
			replace([],Dr,_,Dre),
			Firsthouse = h(Nae,Pee,Cie,Dre,Coe),
			nationalities(P,P,Na2),
			ciggies(P,P,Ci2),
			colours(P,P,Co2),
			pets(P,P,Pe2),
			drinks(P,P,Dr2),
			replace([],Na2,_,Nae2),
			replace([],Ci2,_,Cie2),
			replace([],Co2,_,Coe2),
			replace([],Pe2,_,Pee2),
			replace([],Dr2,_,Dre2),
			Secondhouse = h(Nae2,Pee2,Cie2,Dre2,Coe2),!.

position_member(M,P,Z):-
			nationalities(M,M,Na),
			ciggies(M,M,Ci),
			colours(M,M,Co),
			pets(M,M,Pe),
			drinks(M,M,Dr),
			replace([],Na,_,Nae),
			replace([],Ci,_,Cie),
			replace([],Co,_,Coe),
			replace([],Pe,_,Pee),
			replace([],Dr,_,Dre),
			Firsthouse = h(Nae,Pee,Cie,Dre,Coe),
			locationator(Firsthouse,P,Z).


locationator(F,middle,Z):-
			Z = [_,_,F,_,_].
locationator(F,last,Z):-
			Z = [_|F].
locationator(F,far_right,Z):-
			Z = [_|F].
locationator(F,first,Z):-                       %  9
			Z = [F|_].
locationator(F,'1st',Z):-                       %  9
			Z = [F|_].
locationator(F,'far_left',Z):-
			Z = [F|_].

positionator(Q,Hs,Z):-
			[Q,_,_,_,_] = Hs,
			Z = first.
positionator(Q,Hs,Z):-
			[_,Q,_,_,_] = Hs,
			Z = second.
positionator(Q,Hs,Z):-
			[_,_,Q,_,_] = Hs,
			Z = third.
positionator(Q,Hs,Z):-
			[_,_,_,Q,_] = Hs,
			Z = fourth.
positionator(Q,Hs,Z):-
			[_,_,_,_,Q] = Hs,
			Z = fifth.

nationalities(M,P,R):-
    	(
        Na = [ukrainian,englishman,spaniard,norwegian,japanese,lady_winslow,countess_contee,madam_natsiou,baroness_finch,doctor_marcolla],
				intersection([M,P],Na,Z),
        Z = [R|_];
        R = []
        ).

ciggies(M,P,R):-
    	(
        Ci = [kools,lucky_strike,parliaments,chesterfields,old_gold,fraeport,dabokva,dunwall,karnaca,baleton],
				intersection([M,P],Ci,Z),
        Z = [R|_];
        R = []
        ).
colours(M,P,R):-
    	(
        Co = [red,green,ivory,blue,yellow,purple,white],
				intersection([M,P],Co,Z),
        Z = [R|_];
        R = []
        ).

pets(M,P,R):-
    	(
        Pe = [fox,dog,horse,snails,zebra,war_medal,snuff_tin,ring,bird_pendant,diamond],
				intersection([M,P],Pe,Z),
        Z = [R|_];
        R = []
        ).

drinks(M,P,R):-
    	(
        Dr = [coffee,tea,milk,orange_juice,water,whiskey,rum,wine,beer,absinthe],
				intersection([M,P],Dr,Z),
        Z = [R|_];
        R = []
        ).

replace(Element, Element, NElement, NElement).
replace(_, X, _, X).
