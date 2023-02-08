
:- module(server, []).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(yall)).
:- use_module(korrvigs(actions)).
:- use_module(korrvigs(ctx)).

handle_request(REQ) :-
  member(search(SEARCH), REQ), !,
  ( member(pointing = FILE, SEARCH) -> asserta(ctx:pointing(FILE)) ).
handle_request(_).

actions_handler(REQ) :-
  handle_request(REQ),
  actions:list(ACTS), !,
  ctx:clear,
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
  handle_request(REQ),
  format("Content-Type: test/plain~n~n"),
  ( catch(
      call(PRED),
      EXCEPT,
      ( format("~w~n", EXCEPT), fail ))
    -> format("Success~n")
    ;  format("Failure~n") ),
  ctx:clear.

register(NAME, PRED) :-
  concat("actions/", NAME, PATH),
  http_handler(root(PATH), registered_handler(PRED), [ methods([get]), spawn() ]).

run :-
  http_handler(root(actions), actions_handler, [ methods([get]), spawn() ]),
  http_server(http_dispatch, [port(1588)]).
