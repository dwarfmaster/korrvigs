
:- module(fzf,
         [ run/3
         , select/2
         , select_text/2
         ]).

%! run(+CHOICES, +OPTS, -R)
%  Given a list CHOICES of pair of values and description, open a fuzzy finder
%  on the descriptions and return the selected associated value. Additional
%  options can be given to fzf with OPTS. Values may not include spaces.
run(CHOICES, OPTS, R) :-
  append([ "--read0", "--with-nth=2.." ], OPTS, FZF_OPTS),
  setup_call_cleanup(
    process_create(path(fzf), FZF_OPTS, [ stdin(pipe(IN)), stdout(pipe(OUT)), detached(true) ]),
    ( forall(member([OPT, DESC], CHOICES), format(IN, "~w ~w\0", [ OPT, DESC ])),
      close(IN),
      read_string(OUT, " ", "", _, R)
    ),
    close(OUT)).

%! select(+CHOICES, -CHOICE)
%  Given a list of choices, run fzf to get a choice
select(CHOICES, CHOICE) :-
  run(CHOICES, [], CHOICE).

%! select_text(+CHOICES, -CHOICE)
%  Same as select, but assume the values are path to text files, enabling
%  a previewer in fzf
select_text(CHOICES, CHOICE) :-
  run(CHOICES, [ "--preview", "bat --color=always {+1}" ], CHOICE).
