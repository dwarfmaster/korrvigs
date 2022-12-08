
:- use_module(library(process)).
:- use_module(library(http/json)).

data_dir("/home/luc/downloads/wiki").
extract_script("/home/luc/repos/korrvigs/extract.rb").

make_path([ PATH ], PATH) :- !.
make_path(COMPONENTS, PATH) :-
  append([ HD ], TL, COMPONENTS),
  make_path(TL, TLPATH),
  format(string(PATH), "~w/~w", [ HD, TLPATH ]).

subs_of(DIR, FILE, PATH) :-
  directory_files(DIR, ALL),
  append([_,_], FILES, ALL),
  member(FILE, FILES),
  make_path([ DIR, FILE ], PATH).

wiki_file(PATH, UUID) :-
  data_dir(ROOT),
  subs_of(ROOT, _, SUBDIR),
  subs_of(SUBDIR, UUID, WIKIDIR),
  adoc_file(WIKIDIR, PATH).

adoc_file(WIKIDIR, PATH) :-
  subs_of(WIKIDIR, NAME, PATH),
  concat(_, ".adoc", NAME), !.

valid_char(CHAR) :-
  char_type(CHAR, ascii),
  char_type(CHAR, csym).
valid_char('.').

glob_filename_character(CHAR, GLOBBED) :-
  valid_char(CHAR), !,
  downcase_atom(CHAR, GLOBBED).
glob_filename_character(CHAR, '_') :-
  char_type(CHAR, white), !.
glob_filename_character(_, 'X').

glob_filename(FILENAME, GLOBBED) :-
  atom_string(FILENAME, STR),
  string_chars(STR, CHRS),
  maplist(glob_filename_character, CHRS, GLOBBED_CHRS),
  string_chars(GLOBBED, GLOBBED_CHRS).

% TODO Better handle errors in process
get_attributes(PATH, ATTRS) :-
  extract_script(EXTRACT),
  setup_call_cleanup(
    process_create(path(ruby), [ EXTRACT, "attributes", PATH ], [ stdout(pipe(OUT)) ]),
    json_read_dict(OUT, ATTRS),
    close(OUT)).

gen_uuid(UUID) :-
  setup_call_cleanup(
    process_create(path(uuidgen), [], [ stdout(pipe(OUT)) ]),
    read_string(OUT, 36, UUID),
    close(OUT)).

write_adoc(TITLE, AUTHOR, ATTRS, STREAM) :-
  format(STREAM, "= ~w~n~w~n", [ TITLE, AUTHOR ]),
  forall(
    get_dict(NAME, ATTRS, VALUE),
    format(STREAM, ":~w: ~w~n", [ NAME, VALUE ])).

register_adoc(TITLE, AUTHOR, ATTRS, UUID) :-
  gen_uuid(UUID), !,
  sub_atom(UUID, 0, 2, _, ID), data_dir(ROOT),
  make_path([ ROOT, ID ], PATH_ID), make_directory(PATH_ID),
  make_path([ PATH_ID, UUID ], PATH_UUID), make_directory(PATH_UUID),
  glob_filename(TITLE, GLOB), concat(GLOB, ".adoc", FILENAME),
  make_path([ PATH_UUID, FILENAME ], PATH),
  setup_call_cleanup(
    open(PATH, write, FILE, []),
    write_adoc(TITLE, AUTHOR, ATTRS, FILE),
    close(FILE)).

