
:- module(adoc,
         [ get_attributes/2
         , set_attribute/3
         , init/4
         ]).
:- use_module(library(process)).
:- use_module(library(http/json)).

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

%! init(-TITLE, -AUTHOR, -ATTRS, -STREAM) is det
%  Generates a minimal asciidoc file from its title, initial attributes and author, and write it
%  to STREAM
init(TITLE, AUTHOR, ATTRS, STREAM) :-
  format(STREAM, "= ~w~n~w~n", [ TITLE, AUTHOR ]),
  forall(
    get_dict(NAME, ATTRS, VALUE),
    format(STREAM, ":~w: ~w~n", [ NAME, VALUE ])).
