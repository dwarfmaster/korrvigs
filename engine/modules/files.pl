
:- module(files, []).
:- discontiguous actions:register/3.
:- use_module(korrvigs(nvim)).
:- use_module(korrvigs(actions)).

view_text(PATH) :- nvim:sock(SOCK), !, nvim:edit(SOCK, PATH).
edit_text(PATH) :- nvim:sock(SOCK), !, nvim:edit(SOCK, PATH).
actions:register(100, DESC, files:edit_text(PATH)) :-
  ctx:desktop(),
  ctx:pointing(path(PATH)),
  exists_file(PATH),
  file_base_name(PATH, NAME),
  concat("Open ", NAME, DESC).

view_url(URL) :-
  process_create(path("firefox-launcher"), [ URL ], [ detached(true) ]).
actions:register(100, "Open", files:view_url(URL)) :-
  ctx:desktop(),
  ctx:pointing(url(URL)).

