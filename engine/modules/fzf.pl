
:- module(fzf,
         [ run/3
         , select/2
         , select_text/2
         ]).
:- use_module(korrvigs(popup)).

handler(CHOICES, IN, OUT, RES) :-
  forall(member([OPT, DESC], CHOICES), format(IN, "~w ~w\0", [ OPT, DESC ])),
  close(IN),
  read_string(OUT, " ", "", _, RES).

%! run(+CHOICES, +OPTS, -R)
%  Given a list CHOICES of pair of values and description, open a fuzzy finder
%  on the descriptions and return the selected associated value. Additional
%  options can be given to fzf with OPTS. Values may not include spaces.
run(CHOICES, OPTS, R) :-
  append([ "--read0", "--with-nth=2.." ], OPTS, FZF_OPTS),
  absolute_file_name(path(fzf), FZF),
  popup:with(FZF, FZF_OPTS, fzf:handler(CHOICES), R).

%! select(+CHOICES, -CHOICE)
%  Given a list of choices, run fzf to get a choice
select(CHOICES, CHOICE) :-
  run(CHOICES, [], CHOICE).

%! select_text(+CHOICES, -CHOICE)
%  Same as select, but assume the values are path to text files, enabling
%  a previewer in fzf
select_text(CHOICES, CHOICE) :-
  run(CHOICES, [ "--preview", "bat --color=always {+1}" ], CHOICE).
