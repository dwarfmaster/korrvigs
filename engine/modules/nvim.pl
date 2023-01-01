
:- module(nvim, []).
:- use_module(korrvigs(ctx)).

ctx:declare(nvim, path, "NVIM instance socket").

%! view(+SOCK, +FILE)
%  Open a file in neovim as readonly file
view(SOCK, PATH) :-
  process_create(path(nvim), [ "--server", SOCK, "--remote", "-R", PATH ], [ ]).

%! edit(+SOCK, +FILE)
%  Open a file in neovim
edit(SOCK, PATH) :-
  process_create(path(nvim), [ "--server", SOCK, "--remote", PATH ], [ ]).
