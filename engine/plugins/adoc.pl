
:- module(adoc, []).
:- use_module(korrvigs(plugins)).
:- use_module(korrvigs(wiki)).

% First argument of adoc_macro is either follow or edit
plugins:run_action_impl(1, adoc_macro(_, wiki, UUID, _), CTX) :-
  wiki:wiki_file(PATH, UUID),
  plugins:run_action(open_text(PATH), CTX).
% TODO macros: link, image

% TODO should also remove source file
plugins:run_action_impl(1, adoc_store_file(PATH), CTX) :-
  bagof(W, UUID^wiki:wiki_file(W, UUID), FILES),
  plugins:run_action(select_file(FILES, FILE), CTX), !,
  directory_file_path(DIR, _, FILE),
  % TODO check files does not exists and ask for confirmation if it does
  copy_file(PATH, DIR).

% TODO check if file is not text
plugins:is_available(CTX, adoc_store_file(PATH), DESC, 90) :-
  member(pointing(file(PATH)), CTX),
  concat("/home/luc/downloads", _, PATH),
  directory_file_path(_, FILE, PATH),
  concat("Store file ", FILE, DESC).
plugins:is_available(CTX, adoc_store_file(PATH), DESC, 30) :-
  member(pointing(file(PATH)), CTX),
  not(concat("/home/luc/downloads", _, PATH)),
  directory_file_path(_, FILE, PATH),
  concat("Store file ", FILE, DESC).

% Helpers actions that can be used by others
plugins:run_action_impl(1, adoc_set_attr(PATH, ATTR, VALUE), _) :-
  wiki:set_attribute(PATH, ATTR, VALUE).
