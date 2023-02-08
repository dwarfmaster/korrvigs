
:- module(ctx, []).
:- use_module(library(uri)).

%! pointing(-ELEM)
%  Give the currently pointed to element.
:- multifile pointing/1.
:- dynamic pointing/1.

%! editing(-FILE)
%  Give the currently edited file
:- multifile editing/1.

%! viewing(-ELEM)
%  Give the currently viewed element
:- multifile viewing/1.
viewing(file(FILE)) :- editing(FILE).

%! desktop()
%  On success indicates korrvigs is running on a desktop
:- multifile desktop/0.

%! context_stored(+NAME, +VALUE)
%  A dynamic predicate storing the context
:- dynamic context_stored/2 as local.

%! memoise(+NAME, +PRED, -VALUE)
%  PRED must be a unary predicate. Run PRED to get value the first time it
%  is run a session, otherwise use the memoised value.
memoise(NAME, _, VALUE) :-
  context_stored(NAME, VALUE), !.
memoise(NAME, PRED, VALUE) :-
  call(PRED, VALUE),
  asserta(context_stored(NAME, VALUE)).

%! clear()
%  Clear all memoised values.
clear() :-
  retractall(context_stored(_, _)),
  retractall(pointing(_)).
