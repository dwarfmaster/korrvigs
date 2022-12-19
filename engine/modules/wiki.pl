
:- module(wiki,
         [ wiki_file/2
         , get_attributes/2
         , set_attribute/3
         , register_adoc/4
         ]).
:- use_module(library(process)).
:- use_module(library(http/json)).

%! make_path(-COMPONENTS, +PATH) is det
%  make_path concatenates components into path, inserting '/' between them
make_path([ PATH ], PATH) :- !.
make_path(COMPONENTS, PATH) :-
  append([ HD ], TL, COMPONENTS),
  make_path(TL, TLPATH),
  format(string(PATH), "~w/~w", [ HD, TLPATH ]).

%! subs_of(-DIR, +NAME, +PATH) is nondet
%  Given DIR a path to a directory, subs_of succeeds exactly once for each file in DIR,
%  giving its NAME and its PATH.
subs_of(DIR, FILE, PATH) :-
  directory_files(DIR, ALL),
  append([_,_], FILES, ALL),
  member(FILE, FILES),
  make_path([ DIR, FILE ], PATH).

%! wiki_file(+PATH, ~UUID) is nondet
%  Succeed once for each wiki file, giving its complete path and UUID. If UUID is specified,
%  succeeds at most once, for the wiki file that has this UUID.
wiki_file(PATH, UUID) :-
  data_dir(ROOT),
  subs_of(ROOT, _, SUBDIR),
  subs_of(SUBDIR, UUID, WIKIDIR),
  adoc_file(WIKIDIR, PATH).

%! adoc_file(-WIKIDIR, +PATH) is nondet
%  Find one asciidoc file in WIKIDIR
adoc_file(WIKIDIR, PATH) :-
  subs_of(WIKIDIR, NAME, PATH),
  concat(_, ".adoc", NAME), !.

%! valid_char(-CHAR) is det
%  Succeeds on caracters that are valid in a filename
valid_char(CHAR) :-
  char_type(CHAR, ascii),
  char_type(CHAR, csym).
valid_char('.').

%! glob_filename_character(-CHAR, +GLOBBED) is det
%  Given a character CHAR in the title of an entry, transform it into the character that must
%  go into the file name of that entry.
glob_filename_character(CHAR, GLOBBED) :-
  valid_char(CHAR), !,
  downcase_atom(CHAR, GLOBBED).
glob_filename_character(CHAR, '_') :-
  char_type(CHAR, white), !.
glob_filename_character(_, 'X').

%! glob_filename(-TITLE, +GLOBBED) is det
%  Given the title of an entry, compute the filename it must be stored under.
glob_filename(FILENAME, GLOBBED) :-
  atom_string(FILENAME, STR),
  string_chars(STR, CHRS),
  maplist(glob_filename_character, CHRS, GLOBBED_CHRS),
  string_chars(GLOBBED, GLOBBED_CHRS).

%! get_attributes(-PATH, +ATTRS) is det
%, get_attributes
%  Given the path to an asciidoc file, get its attributes as a dictionary
% TODO Better handle errors in process
get_attributes(PATH, ATTRS) :-
  extract_script(EXTRACT),
  setup_call_cleanup(
    process_create(path(ruby), [ EXTRACT, "attributes", PATH ], [ stdout(pipe(OUT)) ]),
    json_read_dict(OUT, ATTRS),
    close(OUT)).

%! set_attribute(-PATH, -ATTR, -VALUE) is det
%  Set and attribute in an asciidoc file
set_attribute(PATH, ATTR, VALUE) :-
  extract_script(EXTRACT),
  process_create(path(ruby), [ EXTRACT, "set-attr", PATH, ATTR, VALUE ], []).

%! gen_uuid(+UUID) is nondet
%  Generate a new UUID
gen_uuid(UUID) :-
  setup_call_cleanup(
    process_create(path(uuidgen), [], [ stdout(pipe(OUT)) ]),
    read_string(OUT, 36, UUID),
    close(OUT)).

%! write_adoc(-TITLE, -AUTHOR, -ATTRS, -STREAM) is det
%  Generates a minimal asciidoc file from its title, initial attributes and author, and write it
%  to STREAM
write_adoc(TITLE, AUTHOR, ATTRS, STREAM) :-
  format(STREAM, "= ~w~n~w~n", [ TITLE, AUTHOR ]),
  forall(
    get_dict(NAME, ATTRS, VALUE),
    format(STREAM, ":~w: ~w~n", [ NAME, VALUE ])).

%! register_adoc(-TITLE, -AUTHOR, -ATTRS, +UUID) is det
%  Given the title, author and initial attributes of a new entry, add this entry to the wiki and
%  return its UUID
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
