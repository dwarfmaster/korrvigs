
:- module(popup, []).
         % [ make/3
         % , run/3
         % , clean/1
         % , with/4
         % ]).
:- use_module(korrvigs(xdg)).
:- use_module(library(socket)).

temp_sock(PATH) :-
  tmp_file_stream(PATH, SIN, [ extension(sock) ]),
  close(SIN), delete_file(PATH).

%! with(+CMD, +ARGS, +PRED, -OUT)
%  Open a popup running cmd and args, and run PRED/3 on the input and output
%  stream of the running process, unifying OUT with the second argument of
%  PRED.
with(CMD, ARGS, PRED, RES) :-
  piper(PIPER),
  append([ "-c", "popup", PIPER, SPATH, CMD ], ARGS, NARGS),
  unix_domain_socket(SOCK),
  setup_call_cleanup(
    ( temp_sock(SPATH) ),
    ( process_create(path(st), NARGS, [ process(PID) ]),
      sleep(1),
      tcp_connect(SOCK, SPATH),
      tcp_open_socket(SOCK, SPAIR),
      stream_pair(SPAIR, OUT, IN),
      !, call(PRED, IN, OUT, RES) ),
    ( process_wait(PID, _), close(SPAIR), delete_file(SPATH) )).
