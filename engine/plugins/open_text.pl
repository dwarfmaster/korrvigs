
:- module(open_text, []).
:- use_module(plugins).

plugins:run_action_impl(1, open_text(PATH), CTX) :-
  member(terminal, CTX),
  process_create(path(less), [ PATH ], []).
plugins:run_action_impl(1, open_text(PATH), CTX) :-
  member(gui(_), CTX),
  concat("less \"", PATH, CMD1),
  concat(CMD1, "\"", CMD),
  spawn_terminal(CMD).
plugins:run_action_impl(10, open_text(PATH), CTX) :-
  member(use_editor(EDITOR), CTX),
  process_create(path(EDITOR), [ PATH ], [ detached(true) ]).
plugins:run_action_impl(50, open_text(PATH), CTX) :-
  member(neovim(SOCK), CTX),
  process_create(path(nvim), [ "--server", SOCK, "--remote", PATH ], [ ]).

plugins:is_available(CTX, open_text(PATH), 70) :-
  member(pointing(text_file(PATH)), CTX).
