
user:file_search_path(korrvigs, "/home/luc/repos/korrvigs/engine/modules").

:- reexport(korrvigs(plugins),
           [ run_action/2
           , select_action/2
           ]).

%! data_dir(+DIR) is det
%  data_dir indicates the directory in which the wiki files must be looked up
data_dir("/home/luc/downloads/wiki").
%! extract_script(+SCRIPT)
%  extract_script indicates the absolute path to the extractor script
extract_script("/home/luc/repos/korrvigs/extractor/extract.rb").
%! spawn_terminal(-CMD)
%  run a terminal executing command
spawn_terminal(CMD) :-
  absolute_file_name(path(bash), BASH),
  process_create(path(st), [ "-e", BASH, "-c", CMD ], [ detached(true) ]).
%! open_url(URL)
%  Open an url
open_url(URL) :-
  process_create(path("firefox-launcher"), [ URL ], [ detached(true) ]).
%! plugin_dir(+DIR)
%  The path to the plugin directory
plugin_dir("/home/luc/repos/korrvigs/engine/plugins").

:- plugins:load_all().
