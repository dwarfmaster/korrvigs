
:- module(nvim, []).
:- dynamic sock/1 as local.

%! view(+SOCK, +FILE)
%  Open a file in neovim as readonly file
view(SOCK, PATH) :-
  process_create(path(nvim), [ "--server", SOCK, "--remote", "-R", PATH ], [ ]).

%! edit(+SOCK, +FILE)
%  Open a file in neovim
edit(SOCK, PATH) :-
  process_create(path(nvim), [ "--server", SOCK, "--remote", PATH ], [ ]).

%! sock(SOCK)
%  A socket SOCK connected to a neovim instance
