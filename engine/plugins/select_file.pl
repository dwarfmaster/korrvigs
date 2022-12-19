
:- module(select_file, []).
:- use_module(korrvigs(plugins)).

plugins:run_action_impl(1, select_file(FILES, SELECTED), CTX) :-
  member(gui(_), CTX),
  setup_call_cleanup(
    process_create(path(rofi), [ "-dmenu" ], [ stdin(pipe(IN)), stdout(pipe(OUT)), detached(true) ]),
    ( forall(member(FILE,FILES), format(IN, "~w~n", FILE)),
      close(IN),
      read_string(OUT, _, SELECTION),
      concat(SELECTED, "\n", SELECTION)),
    close(OUT)).
