
:- module(actions, []).
:- multifile register/3.
:- use_module(korrvigs(fzf)).
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

%! select
%  Use fzf to select an action to run.
select :-
  list(ACTS),
  maplist(reverse, ACTS, OPTS),
  fzf:select(OPTS, CODE),
  call(CODE).

%! register(+PRIORITY, +DESC, +CODE)
%  Each success is an action with a priority, a description and a code
%  to execute. It should fail if the action is not meant to be applied.
register(0, "Select an action to run", actions:select) :- ctx:get(desktop, true).
