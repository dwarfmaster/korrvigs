
:- module(server, []).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(yall)).
:- use_module(korrvigs(actions)).
:- use_module(korrvigs(ctx)).

actions_handler(_) :-
  asserta(ctx:desktop()),
  actions:list(ACTS),
  maplist([[ NAME, ACT ], O]>> (term_string(ACT, CODE), 
                                O = _{'name': NAME, 'code': CODE}), 
          ACTS, JSON),
  json_write(current_output, JSON).

run :-
  http_handler(root(actions), actions_handler, [ methods([get]), spawn() ]),
  http_server(http_dispatch, [port(1588)]).
