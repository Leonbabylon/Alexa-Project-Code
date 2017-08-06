:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

% The predicate server(+Port) starts the server. It simply creates a
% number of Prolog threads and then returns to the toplevel, so you can
% (re-)load code, debug, etc.
server(Port) :-
        http_server(http_dispatch, [port(Port)]).

% Declare a handler, binding an HTTP path to a predicate.
% Here our path is / (the root) and the goal we'll query will be
% say_hi. The third argument is for options
:- http_handler('/hello.txt', say_hi(pleasant), []).
:- http_handler('/hi.txt',    say_hi(foul), []).

/* The implementation of /. The single argument provides the request
details, which we ignore for now. Our task is to write a CGI-Document:
a number of name: value -pair lines, followed by two newlines, followed
by the document content, The only obligatory header line is the
Content-type: <mime-type> header.
Printing can be done using any Prolog printing predicate, but the
format-family is the most useful. See format/2.   */

say_hi(Kappa,_Request) :-
        pleasant :-
          format('Content-type: text/plain~n~n'),
          format('Hello World!~n').
        foul :-
          format('Content-type: text/plain~n~n'),
          format('Hi World!~n').
          % natural language shell
          nl_shell(Rulebase):- get_input(Input),handle_input(Input,Rulebase).


%rulebase and AI (pokedex)


% handle input from user
handle_input(stop,_Rulebase):-!.
handle_input(show,Rulebase):-!,show_rules(Rulebase),nl_shell(Rulebase).
handle_input(Sentence,Rulebase):-phrase(sentence(Rule),Sentence),!,nl_shell([Rule|Rulebase]).
handle_input(Question,Rulebase):-phrase(question(Query),Question),prove_rb(Query,Rulebase),!,
	transform(Query,Clauses),phrase(sentence(Clauses),Answer),show_answer(Answer),nl_shell(Rulebase).
handle_input(_Question,Rulebase):-show_answer('No'),nl_shell(Rulebase).

% show current rulebase
show_rules([]).
show_rules([Rule|Rules]):-phrase(sentence(Rule),Sentence),show_answer(Sentence),show_rules(Rules).

% meta-interpreter
prove_rb(true,_Rulebase):-!.
prove_rb((A,B),Rulebase):-!,prove_rb(A,Rulebase),prove_rb(B,Rulebase).
prove_rb(A,Rulebase):-find_clause((A:-B),Rulebase),prove_rb(B,Rulebase).

% find applicable clause in rulebase
find_clause(Clause,[Rule|_Rules]):- copy_element(Clause,Rule).
find_clause(Clause,[_Rule|Rules]):- find_clause(Clause,Rules).

copy_element(X,Ys):-element(X1,Ys),copy_term(X1,X).

% element(X,Ys) <- X is an element of the list Ys
element(X,[X|_Ys]).
element(X,[_Y|Ys]):-element(X,Ys).

% transform query to answer
transform((A,B),[(A:-true)|Rest]):-!,transform(B,Rest).
transform(A,[(A:-true)]).

% get input from user
get_input(Input):-write('? '),read(Input).

% show answer to user
show_answer(Answer):-write('! '),write(Answer),nl.
/*
 This part is inherited from others.
*/
:-op(600,xfy,'=>').
sentence(C)                   --> determiner(N,M1,M2,C), noun(N,M1), verb_phrase(N,M2).
sentence([(L:-true)])         --> proper_noun(N,X), verb_phrase(N,X=>L).
verb_phrase(s,M)              --> [is],property(s,M).
verb_phrase(p,M)              --> [are],property(p,M).
property(s,M)                 --> [a],noun(s,M).
property(p,M)                 --> noun(p,M).
property(_N,X=>mortal(X))      --> [mortal].
determiner(s,X=>B,X=>H,[(H:-B)]) --> [every].
determiner(p,sk=>H1,sk=>H2,[(H1:-true),(H2:-true)]) --> [some].
proper_noun(s,socrates)       --> [socrates].
noun(s,X=>human(X))           --> [human].
noun(p,X=>human(X))           --> [humans].
noun(s,X=>living_being(X))    --> [living],[being].
noun(p,X=>living_being(X))    --> [living],[beings].

question(Q)          --> [who],[is],property(s,_X=>Q).
question(Q)          --> [is],proper_noun(N,X),
                         property(N,X=>Q).
question((Q1,Q2))    --> [are],[some],noun(p,sk=>Q1),
                         property(p,sk=>Q2).
/*
 This is the end of inheritance.
*/
