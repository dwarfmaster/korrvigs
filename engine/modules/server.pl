
:- module(server, []).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(yall)).
:- use_module(korrvigs(actions)).
:- use_module(korrvigs(ctx)).

ctx_from_request(REQ) :-
  member(search(SEARCH), REQ), !,
  forall(
    member(CTX = VAL, SEARCH),
    ctx:set(CTX, VAL)).
ctx_from_request(_).

actions_handler(REQ) :-
  ctx_from_request(REQ),
  actions:list(ACTS), !,
  maplist([[ NAME, ACT ], O]>> (term_string(ACT, CODE), 
                                O = _{'name': NAME, 'code': CODE}), 
          ACTS, JSON),
  format("Content-Type: text/json~n~n"),
  json_write(current_output, JSON).
% No actions found
actions_handler(_) :-
  format("Content-Type: text/json~n~n"),
  format("[]~n").

registered_handler(PRED, REQ) :-
  ctx_from_request(REQ),
  format("Content-Type: test/plain~n~n"),
  ( catch(
      call(PRED),
      EXCEPT,
      ( format("~w~n", EXCEPT), fail ))
    -> format("Success~n")
    ;  format("Failure~n") ).

register(NAME, PRED) :-
  concat("actions/", NAME, PATH),
  http_handler(root(PATH), registered_handler(PRED), [ methods([get]), spawn() ]).

:- register("select", actions:select).

run :-
  http_handler(root(actions), actions_handler, [ methods([get]), spawn() ]),
  http_server(http_dispatch, [port(1588)]).
