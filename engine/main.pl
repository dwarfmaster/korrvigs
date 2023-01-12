

% Assumes config has been loaded first

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

%  ____       _               
% / ___|  ___| |_ _   _ _ __  
% \___ \ / _ \ __| | | | '_ \ 
%  ___) |  __/ |_| |_| | |_) |
% |____/ \___|\__|\__,_| .__/ 
%                      |_|    

main :- 
  load_modules(),
  xdg:runtime(RT), set_prolog_flag(tmp_dir, RT),
  wiki:reload_all(),
  server:run().
