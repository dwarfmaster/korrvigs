
:- module(xdg,
         [ runtime/1
         , config/1
         , cache/1
         , data/1
         , state/1
         ]).

%! dir_get(-ENV, +PATH)
%  Creates $ENV/korrvigs if it doesn't exists and unify PATH with it
dir_get(ENV, PATH) :-
  getenv(ENV, DIR),
  directory_file_path(DIR, "korrvigs", PATH),
  (exists_directory(PATH) -> true; make_directory(PATH)).

runtime(PATH) :- dir_get("XDG_RUNTIME_DIR", PATH).
config(PATH) :- dir_get("XDG_CONFIG_HOME", PATH).
cache(PATH) :- dir_get("XDG_CACHE_HOME", PATH).
data(PATH) :- dir_get("XDG_DATA_HOME", PATH).
state(PATH) :- dir_get("XDG_STATE_HOME", PATH).
