
:- module(server, []).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(yall)).
:- use_module(korrvigs(actions)).
:- use_module(korrvigs(ctx)).

actions_handler(_) :-
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

run :-
  http_handler(root(actions), actions_handler, [ methods([get]), spawn() ]),
  http_server(http_dispatch, [port(1588)]).
