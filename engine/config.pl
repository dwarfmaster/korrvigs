
user:file_search_path(korrvigs, "/home/luc/repos/korrvigs/engine/modules").
user:file_search_path(foreign, "/home/luc/repos/korrvigs/tree-sitter").
user:file_search_path(library, "/home/luc/repos/korrvigs/tree-sitter").
user:file_search_path(foreign, "/home/luc/repos/korrvigs/posix").
user:file_search_path(library, "/home/luc/repos/korrvigs/posix").

%! data_dir(+DIR) is det
%  data_dir indicates the directory in which the wiki files must be looked up
data_dir("/home/luc/downloads/wiki").
%! piper(+PATH) is det
%  Path to the piper script
piper("/home/luc/repos/korrvigs/piper/piper.out").
%! spawn_terminal(-CMD)
%  run a terminal executing command
spawn_terminal(CMD) :-
  absolute_file_name(path(bash), BASH),
  process_create(path(st), [ "-e", BASH, "-c", CMD ], [ detached(true) ]).
