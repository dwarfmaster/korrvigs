
:- module(popup, []).
:- use_module(korrvigs(xdg)).
:- use_module(library(socket)).
:- use_module(korrvigs(posix)).

temp_sock(PATH) :-
  tmp_file_stream(PATH, SIN, [ extension(sock) ]),
  close(SIN), delete_file(PATH).

server_is_ready(_).

%! with(+CMD, +ARGS, +PRED, -OUT)
%  Open a popup running cmd and args, and run PRED/3 on the input and output
%  stream of the running process, unifying OUT with the second argument of
%  PRED.
with(CMD, ARGS, PRED, RES) :-
  piper(PIPER), current_prolog_flag(pid, CPID),
  append([ "-c", "popup", PIPER, SPATH, CPID, CMD ], ARGS, NARGS),
  unix_domain_socket(SOCK),
  setup_call_cleanup(
    ( temp_sock(SPATH) ),
    ( % Wait for server to be ready
      on_signal(usr1, _, server_is_ready),
      process_create(path(st), NARGS, [ process(PID) ]),
      pause(),
      on_signal(usr1, _, default),

      % Connect to server
      tcp_connect(SOCK, SPATH),
      tcp_open_socket(SOCK, SPAIR),
      stream_pair(SPAIR, OUT, IN),
      !, call(PRED, IN, OUT, RES) ),
    ( on_signal(usr1, _, default),
      process_wait(PID, _), 
      close(SPAIR), 
      delete_file(SPATH) )).
