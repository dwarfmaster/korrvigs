
:- module('wiki',
         [ handler/2
         , uuid_dir/2
         , file/2
         , reload_all/0
         , title/2
         , attr/3
         ]).
:- use_module(korrvigs(actions)).
:- use_module(korrvigs(ctx)).
:- use_module(korrvigs(norg)).

:- use_module(library(process)).
:- use_module(library(http/json)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

%  _____ _ _           
% |  ___(_) | ___  ___ 
% | |_  | | |/ _ \/ __|
% |  _| | | |  __/\__ \
% |_|   |_|_|\___||___/
%                      

%! wiki_file_store(-UUID, -PATH, -TIMESTAMP) is nondet
%  Cache containing all the entries in the wiki, with the UUID,
%  the PATH, and the TIMESTAMP of the last update.
:- dynamic wiki_file_store/3.

%! handler(++LOADER, ++UNLOADER) is nondet
%  Allows declaring loaders and unloaders that will be called
%  when loading or unloading an entry. LOADER and UNLOADER must be
%  either a predicate with two arguments, the uuid and path of the
%  file or none.
:- multifile handler/2.

%! call_handler(++HANDLER, ++UUID, ++PATH) is det
%  Call HANDLER on UUID and PATH or do nothing if HANDLER is none.
call_handler(none, _, _) :- !.
call_handler(H, UUID, PATH) :- call(H, UUID, PATH).

%! unload_file(++UUID, ++PATH) is det
unload_file(UUID, PATH) :-
  retractall(wiki_file_store(UUID, _, _)),
  retractall(wiki_file_store(_, PATH, _)),
  forall(handler(_, UNLOADER), call_handler(UNLOADER, UUID, PATH)).

%! reload_file(++UUID, ++PATH) is semidet
%  Reprocess the file and update the TIMESTAMP in the wiki_store_file
reload_file(UUID, PATH) :-
  unload_file(UUID, PATH),
  exists_file(PATH),
  time_file(PATH, TIMESTAMP),
  assertz(wiki_file_store(UUID, PATH, TIMESTAMP)),
  forall(handler(LOADER, _), call_handler(LOADER, UUID, PATH)).

%! reload_if_new(++UUID, -PATH) is semidet
%  Lookup UUID and PATH from wiki_file_store, checking if the file
%  still exists and reloading it if necessary. If the file doesn't exists,
%  its entry is cleared and reload_if_new fails.
reload_if_new(UUID, PATH) :-
  (atom(UUID) -> true; instantiation_error(UUID)),
  (exists_file(PATH) -> true; unload_file(UUID, PATH), fail),
  ( ( wiki_file_store(UUID, PATH, TIMESTAMP),
      time_file(PATH, NEW_TIME),
      NEW_TIME > TIMESTAMP
    ; not(wiki_file_store(UUID, PATH, _))
    ) -> reload_file(UUID, PATH)
  ; true
  ).

%! uuid_dir(++UUID, -DIR) is det
%  uuid_dir(-UUID, ++DIR) is det
%  Given the UUID of an entry, return its directory
uuid_dir(UUID_IN, DIR) :-
  to_atom(UUID_IN, UUID),
  sub_atom(UUID, 0, 2, _, ID), data_dir(ROOT),
  directory_file_path(ROOT, ID, PATH_ID),
  directory_file_path(PATH_ID, UUID, DIR).
uuid_dir(UUID, DIR_IN) :-
  to_string(DIR_IN, DIR),
  file_base_name(DIR, UUID),
  parse_uuid(UUID).

%! files(-UUID, -PATH) is nondet
%  files(++UUID, -PATH) is semidet
%  files(-UUID, ++PATH) is semidet
file(UUID_IN, PATH) :-
  to_atom(UUID_IN, UUID),
  reload_if_new(UUID, PATH), !.
file(UUID_IN, PATH) :-
  to_atom(UUID_IN, UUID), !,
  uuid_dir(UUID, DIR),
  file_in_dir(DIR, PATH),
  reload_if_new(UUID, PATH).
file(UUID, PATH_IN) :-
  to_string(PATH_IN, PATH), !,
  directory_file_path(DIR, _, PATH),
  uuid_dir(UUID, DIR),
  reload_if_new(UUID, PATH).
file(UUID, PATH) :-
  find_file(UUID, PATH),
  reload_if_new(UUID, PATH).

%! reload_all() is det
%  Reload all wiki files
reload_all() :-
  forall(
    find_file(UUID, PATH),
    ( format("Reloading ~w~n", [ UUID ]),
      reload_file(UUID, PATH))).

%! file_in_dir(++DIR, -PATH) is semidet
%  Find the wiki file in DIR
file_in_dir(DIR, PATH) :-
  subs_of(DIR, FILE, PATH),
  file_name_extension(_, "norg", FILE), !.

%! find_file(-UUID, -PATH) is nondet
%  Find all wiki files without using cached data
find_file(UUID, PATH) :-
  data_dir(ROOT),
  subs_of(ROOT, ID, PATH_ID),
  subs_of(PATH_ID, UUID, DIR),
  parse_uuid(UUID),
  sub_string(UUID, 0, 2, _, ID),
  file_in_dir(DIR, PATH).

actions:register(5, "Reload wiki", wiki:reload_all()) :-
  ctx:desktop().
actions:register(30, DESC, wiki:reload_file(UUID, PATH)) :-
  ctx:desktop(),
  ctx:wiki(UUID),
  file(UUID, PATH),
  file_base_name(PATH, NAME),
  concat("Reload ", NAME, DESC).


%  __  __      _            _       _        
% |  \/  | ___| |_ __ _  __| | __ _| |_ __ _ 
% | |\/| |/ _ \ __/ _` |/ _` |/ _` | __/ _` |
% | |  | |  __/ || (_| | (_| | (_| | || (_| |
% |_|  |_|\___|\__\__,_|\__,_|\__,_|\__\__,_|
%                                            

%! wiki_title_stored(-UUID, -TITLE) is nondet
%  Store the title of a wiki entry
:- dynamic wiki_title_stored/2.

%! wiki_attr_stored(-UUID, -ATTR, -VALUE) is nondet
%  Store the attributes of a wiki entry
:- dynamic wiki_attr_stored/3.

%! unload_metadata(++UUID, ++PATH) is det
%  Clear the metadata of an entry
unload_metadata(UUID, _) :-
  retractall(wiki_title_stored(UUID, _)),
  retractall(wiki_attr_stored(UUID, _, _)).

%! load_metadata(++UUID, ++PATH) is semidet
%  Load the metadata of an entry
load_metadata(UUID, PATH) :-
  norg:parse(PATH, NORG),
  norg:get_attributes(NORG, ATTRS),
  forall(
    get_dict(NAME, ATTRS, VALUE),
    assertz(wiki_attr_stored(UUID, NAME, VALUE))),
  norg:get_title(NORG, TITLE),
  assertz(wiki_title_stored(UUID, TITLE)).

handler(load_metadata, unload_metadata).

%! title(-UUID, -TITLE) is nondet
%  Get the title of an entry
title(UUID, TITLE) :-
  file(UUID, _),
  wiki_title_stored(UUID, TITLE).

%! attr(-UUID, -ATTR, -VALUE) is nondet
%  Get the attribute or value of an entry. If ATTR is a string or
%  an atom, it will use the cached values to answer more efficiently,
%  but may miss some updated data. ATTR may be a list of attributes,
%  in which case VALUE will be the corresponding list of values.
attr(UUID, ATTRS, VALUES) :-
  is_list(ATTRS), !,
  maplist(attr(UUID), ATTRS, VALUES).
attr(UUID, atom(ATTR), VALUE) :-
  !, attr(UUID, ATTR, VALUE_STR),
  atom_string(VALUE, VALUE_STR).
attr(UUID, ATTR_IN, VALUE) :-
  to_atom(ATTR_IN, ATTR), !,
  wiki_attr_stored(UUID, ATTR, VALUE).
attr(UUID, ATTR, VALUE) :-
  file(UUID, _),
  wiki_attr_stored(UUID, ATTR, VALUE).

%! set_attr(++UUID, ++ATTR, ++VALUE) is semidet
%  Set an attribute (or a list of) to a value (or a list of).
set_attr(UUID, ATTRS, VALS) :-
  is_list(ATTRS), !,
  zip(ATTRS, VALS, SETS),
  file(UUID, PATH),
  norg:parse(PATH, NORG),
  norg:set_attributes(NORG, SETTED, SETS),
  norg:commit(SETTED).
set_attr(UUID, ATTR, VAL) :-
  file(UUID, PATH),
  norg:parse(PATH, NORG),
  norg:set_attribute(NORG, SETTED, ATTR, VAL),
  norg:commut(SETTED).


%  _____      _               ____            _             
% | ____|_  _| |_ _ __ __ _  |  _ \ _ __ ___ | | ___   __ _ 
% |  _| \ \/ / __| '__/ _` | | |_) | '__/ _ \| |/ _ \ / _` |
% | |___ >  <| |_| | | (_| | |  __/| | | (_) | | (_) | (_| |
% |_____/_/\_\\__|_|  \__,_| |_|   |_|  \___/|_|\___/ \__, |
%                                                     |___/ 

%! wiki_extra_pl(-UUID, -PATH) is nondet
%  Store the wiki files that have some extra prolog code
:- dynamic wiki_extra_pl/2.

%! unload_extra(++UUID, ++PATH) is det
%  Unload any loaded extra and remove it from wiki_extra_pl
unload_extra(UUID, _) :-
  wiki_extra_pl(UUID, EXTRA), !,
  unload_file(EXTRA),
  retractall(wiki_extra_pl(UUID, _)).
unload_extra(_, _).

%! load_extra(++UUID, ++PATH) is semidet
%  If PATH has an 'extra-prolog' attribute, load it and add the
%  information to wiki_extra_pl
load_extra(UUID, PATH) :-
  attr(UUID, 'extra-prolog', EXTRA), !,
  file_directory_name(PATH, DIR),
  expand_path(DIR, EXTRA, EXTRA_PATH),
  load_files(EXTRA_PATH, []).
load_extra(_,_).

handler(load_extra, unload_extra).


%  _   _ _   _ _     
% | | | | |_(_) |___ 
% | | | | __| | / __|
% | |_| | |_| | \__ \
%  \___/ \__|_|_|___/
%                    

%! gen_uuid(--UUID) is det
%  Generate a new UUID
gen_uuid(UUID) :-
  setup_call_cleanup(
    process_create(path(uuidgen), [], [ stdout(pipe(OUT)) ]),
    read_string(OUT, 36, UUID),
    close(OUT)).

%! to_atom(+STR, -ATOM) is semidet
%  If STR is a string, convert it to an atom. If it is an atom,
%  return it as itself. Otherwise fails.
to_atom(ATOM, ATOM) :- atom(ATOM).
to_atom(STR, ATOM) :- string(STR), atom_string(ATOM, STR).

%! to_string(+ATOM, -STR) is semidet
%  If ATOM is an atom, convert it to a string. If it is a string,
%  return it as itself. Otherwise fails.
to_string(STR, STR) :- string(STR).
to_string(ATOM, STR) :- atom(ATOM), atom_string(ATOM, STR).

uuid_dcg() -->
  sequence(xdigit, PART1),
  "-", sequence(xdigit, PART2),
  "-", sequence(xdigit, PART3),
  "-", sequence(xdigit, PART4),
  "-", sequence(xdigit, PART5),
  { length(PART1, 8),
    length(PART2, 4),
    length(PART3, 4),
    length(PART4, 4),
    length(PART5, 12) }.

%! parse_uuid(++UUID) is semidet
%  Succeeds if UUID is a valid UUID
parse_uuid(UUID) :-
  string_codes(UUID, CODES),
  phrase(uuid_dcg(), CODES, []).

%! subs_of(-DIR, +NAME, +PATH) is nondet
%  Given DIR a path to a directory, subs_of succeeds exactly once for each file in DIR,
%  giving its NAME and its PATH.
subs_of(DIR, FILE, PATH) :-
  directory_files(DIR, ALL),
  append([_,_], FILES, ALL),
  member(FILE, FILES),
  directory_file_path(DIR, FILE, PATH).

%! expand_path(++ROOT, ++REL, -ABS) is det
%  Given a root and a potentially relative path, get the absolute
%  path from this root.
expand_path(_, REL, ABS) :-
  is_absolute_file_name(REL), !,
  absolute_file_name(REL, ABS).
expand_path(ROOT, REL, ABS) :-
  directory_file_path(ROOT, REL, TMP),
  absolute_file_name(TMP, ABS).

%! zip(?A, ?B, ?C) is semidet
zip([], [], []).
zip([A|As], [B|Bs], [[A,B]|Cs]) :- zip(As, Bs, Cs).
