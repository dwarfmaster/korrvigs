
:- module(actions,
         [ register/3
         , list/1
         , run/1
         ]).
:- multifile register/3.

%! register(+PRIORITY, +DESC, +CODE)
%  Each success is an action with a priority, a description and a code
%  to execute. It should fail if the action is not meant to be applied.

%! list(+ACTIONS)
%  Get a list of actions ordered by score. An action is a name and
%  a code to execute.
find_action([ PRIORITY, DESC, CODE ]) :-
  register(PRIORITY, DESC, CODE).
tail(LIST, TL) :- append([_], TL, LIST).
list(ACTIONS) :-
  bagof(ACT, find_action(ACT), ACTS),
  sort(1, @>=, ACTS, SORTED),
  maplist(tail, SORTED, ACTIONS).

%! run_action(-ACTION)
%  Execute the code of an action.
run([_,CODE]) :- call(CODE).
