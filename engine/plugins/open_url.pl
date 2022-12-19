
:- module(open_url, []).
:- use_module(korrvigs(plugins)).

plugins:run_action_impl(1, open_url(URL), CTX) :-
  member(terminal, CTX),
  process_create(path(lynx), [ URL ], []).
plugins:run_action_impl(1, open_url(URL), CTX) :-
  member(gui(_), CTX),
  process_create(path(firefox), [ URL ], [ detached(true) ]).
plugins:run_action_impl(10, open_url(URL), CTX) :-
  member(gui(_), CTX),
  process_create(path("firefox-launcher"), [ URL ], [ detached(true) ]).
plugins:is_available(CTX, open_url(URL), DESC, 100) :-
  member(pointing(url(URL)), CTX),
  concat("Follow ", URL, DESC).
