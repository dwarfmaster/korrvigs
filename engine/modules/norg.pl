
:- module(norg,
         [ parse/2
         , get_attributes/2
         , get_title/2
         , get_authors/2
         , set_attribute/4
         , set_attributes/3
         , commit/1
         , init/4
         ]).
:- use_module(library('norg-parser')).
:- use_module(library('xpath')).
:- use_module(library('yaml')).

parse(PATH, norg{ 'title': TITLE, 'authors': AUTHORS
                , 'path': PATH, 'dom': DOM
                , 'attrs': ATTRS, 'attrs-range': [ START, END ] }) :-
  'norg-parser':parse_norg(file(PATH), DOM),
  parse_meta(DOM, TITLE, AUTHORS),
  parse_attrs(DOM, ATTRS, START, END).

parse_meta(DOM, TITLE, AUTHORS) :-
  xpath(DOM, //ranged_verbatim_tag, TAG),
  xpath(TAG, /'*'/tag_name(text), 'document.meta'), !,
  xpath(TAG, /'*'/ranged_verbatim_tag_content, CONTENT), !,
  xpath(CONTENT, /'*'(text), META),
  yaml_read(string(META), ATTRS),
  _{ 'title': TITLE, 'authors': AUTHORS } :< ATTRS.

parse_attrs(DOM, ATTRS, START, END) :-
  xpath(DOM, //ranged_verbatim_tag, TAG),
  xpath(TAG, /'*'/tag_name(text), 'korrvigs.attrs'), !,
  parse_attrs_tag(TAG, ATTRS, START, END).
parse_attrs_tag(TAG, ATTRS, START, END) :-
  xpath(TAG, /'*'/ranged_verbatim_tag_content, CONTENT), !,
  xpath(CONTENT, /'*'(@start), START), xpath(CONTENT, /'*'(@end), END),
  xpath(CONTENT, /'*'(text), META),
  yaml_read(string(META), ATTRS).
% Handle empty tag
parse_attrs_tag(TAG, yaml{}, START, START) :-
  xpath(TAG, /'*'/ranged_verbatim_tag_end(@start), START).

get_attributes(NORG, ATTRS) :- _{ 'attrs': ATTRS } :< NORG.
get_title(NORG, TITLE) :- _{ 'title': TITLE } :< NORG.
get_authors(NORG, AUTHORS) :- _{ 'authors': AUTHORS } :< NORG.

set_attribute(IN, OUT, KEY, VALUE) :-
  OUT = IN.put('attrs', IN.attrs.put(KEY, VALUE)).
set_attributes(IN, OUT, [[KEY,VALUE]|ATTRS]) :-
  atom_string(RKEY, KEY),
  set_attribute(IN, NIN, RKEY, VALUE), !,
  set_attributes(NIN, OUT, ATTRS).
set_attributes(IN, IN, []).

generate(NORG, OUT) :-
  _{ 'attrs': ATTRS, 'attrs-range': [ START, END ], 'path': PATH } :< NORG,
  setup_call_cleanup(
    ( open(PATH, read, IN, [ type(binary) ]),
      open_null_stream(NULL) ),
    ( copy_stream_data(IN, OUT, START),
      yaml_write(OUT, ATTRS, [ unicode(true), canonical(false) ]),
      LEN is END - START, copy_stream_data(IN, NULL, LEN),
      copy_stream_data(IN, OUT)
    ),
    ( close(IN), close(NULL) )).

copy(FROM, TO) :-
  setup_call_cleanup(
    ( open(FROM, read, IN, [ type(binary) ]),
      open(TO, write, OUT, [ type(binary) ]) ),
    copy_stream_data(IN, OUT),
    ( close(IN), close(OUT) )).

commit(NORG) :-
  tmp_file_stream(TMP, OUT, [ encoding(binary), extension(norg) ]),
  generate(NORG, OUT),
  close(OUT),
  copy(TMP, NORG.path),
  delete_file(TMP).

format_value(OUT, VALUE) :-
  is_list(VALUE), !,
  format(OUT, "~n", []),
  forall(member(VAL, VALUE),
    format(OUT, "- ~w~n", [VAL])).
format_value(OUT, VALUE) :-
  format(OUT, " ~w~n", [VALUE]).

init(TITLE, AUTHORS, ATTRS, OUT) :-
  format(OUT, "@document.meta~ntitle: ~w~nauthors:", [TITLE]),
  format_value(OUT, AUTHORS),
  format(OUT, "@end~n~n@korrvigs.attrs~n", []),
  forall(get_dict(KEY, ATTRS, VALUE),
    ( format(OUT, "~w:", [KEY]), format_value(OUT, VALUE) )),
  format(OUT, "@end~n", []).


