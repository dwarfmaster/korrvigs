

%  __  __           _       _           
% |  \/  | ___   __| |_   _| | ___  ___ 
% | |\/| |/ _ \ / _` | | | | |/ _ \/ __|
% | |  | | (_) | (_| | |_| | |  __/\__ \
% |_|  |_|\___/ \__,_|\__,_|_|\___||___/
%                                       
user:file_search_path(korrvigs, "/home/luc/repos/korrvigs/engine/modules").
user:file_search_path(library, "/home/luc/repos/korrvigs/tree-sitter").
user:file_search_path(library, "/home/luc/repos/korrvigs/engine/posix").

%! find_module(+PATH)
%  find a module
find_module(PATH) :-
  absolute_file_name(korrvigs("."), DIR, [access(exist), file_type(directory), solutions(all)]),
  directory_files(DIR, FILES),
  member(FILE, FILES),
  concat(_, ".pl", FILE),
  directory_file_path(DIR, FILE, PATH),
  access_file(PATH, read).

%! load_modules()
%  Load all modules
load_modules() :- forall(find_module(PATH), use_module(PATH)).


%   ____             __ _       
%  / ___|___  _ __  / _(_) __ _ 
% | |   / _ \| '_ \| |_| |/ _` |
% | |__| (_) | | | |  _| | (_| |
%  \____\___/|_| |_|_| |_|\__, |
%                         |___/ 

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



%  ____       _               
% / ___|  ___| |_ _   _ _ __  
% \___ \ / _ \ __| | | | '_ \ 
%  ___) |  __/ |_| |_| | |_) |
% |____/ \___|\__|\__,_| .__/ 
%                      |_|    

:- load_modules().
:- xdg:runtime(RT), set_prolog_flag(tmp_dir, RT).
:- wiki:reload_all().
