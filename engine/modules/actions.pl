
:- module(actions, []).
:- multifile register/3.
:- use_module(korrvigs(ctx)).

%! list(+ACTIONS)
%  Get a list of actions ordered by score. An action is a name and
%  a code to execute.
find_action([ PRIORITY, DESC, CODE ]) :-
  register(PRIORITY, DESC, CODE).
tail(LIST, TL) :- append([_], TL, LIST).
list(ACTIONS) :-
  bagof(ACT, find_action(ACT), ACTS), !,
  sort(1, @>=, ACTS, SORTED),
  maplist(tail, SORTED, ACTIONS).
list([]).

%! run_action(-ACTION)
%  Execute the code of an action.
run([_,CODE]) :- call(CODE).
