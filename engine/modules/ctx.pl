
:- module(ctx, []).
:- use_module(library(uri)).

parse_uri(URI, _{ 'str': URI, 'scheme': SCHEME,
                  'authority': AUTH, 'path': PATH,
                  'search': SRH, 'fragment': FRG }) :-
  uri_components(URI, COMPS),
  uri_data(scheme, COMPS, SCHEME),
  uri_data(authority, COMPS, AUTH),
  uri_data(path, COMPS, PATH),
  uri_data(search, COMPS, SRH),
  uri_data(fragment, COMPS, FRG).
parse_url(URL, URL) :-
  parse_uri(URL, COMPS),
  member(COMPS.scheme, [ 'http', 'https' ]).
parse_path(PATH, ABS) :-
  absolute_file_name(PATH, ABS).
parse_uuid(UUID, UUID) :-
  wiki:parse_uuid(UUID).
parse_bool(STR, true) :- atom_string(true, STR).
parse_bool(STR, false) :- atom_string(false, STR).

parse_attr(KEYS, INPUT, ATTR) :-
  maplist(KEYS, [[KEY, TYPE], R]>> V^(get_dict(KEY, INPUT, V), parse_type(TYPE, V, R)), RES), 
  ATTR = _{}.put(RES).
parse_sum(CASES, _{ 'kind': KIND_STR, 'value': VAL }, RES) :-
  atom_string(KIND, KIND_STR),
  member([KIND, TYPE], CASES), !,
  parse_type(TYPE, VAL, PARSED),
  RES =.. [KIND, PARSED].

%! type(+NAME, +PARSER, +DESC)
%  Register a context type, with a parser to get a
%  from a json object
:- multifile type/3.
type(str, atom_strings, "a string").
type(uri, ctx:parse_uri, "an URI").
type(url, ctx:parse_url, "an URL").
type(path, ctx:parse_path, "a filesystem path").
type(uuid, ctx:parse_uuid, "a unique identifier").
type(bool, ctx:parse_bool, "a boolean").
% TODO better descriptions
type(attr(KEYS), ctx:parse_attr(KEYS), "a attribute set").
type(sum(CASES), ctx:parse_sum(CASES), "an alternative").

%! doc_kind(+KIND, +TYPE)
%  The accepted kinds of document references
:- multifile doc_kind/2.
doc_kind(url, url).
doc_kind(path, path).
doc_kind(wiki, uuid).
type(doc, ctx:parse_sum(CASES), "a document") :-
  bagof([NAME, TYPE], doc_kind(NAME, TYPE), CASES).

%! parse_type(+TYPE, +VAL, -RES)
%  Parse VAL using the parser associated with TYPE
parse_type(TYPE, VAL, R) :-
  type(TYPE, PARSER, _),
  call(PARSER, VAL, R).

%! declare(+NAME, +TYPE, +DESC)
%  Declare a new context key, along with its type and
%  a description.
:- multifile declare/3.
declare(pointing, doc, "Cursor pointing to document").
declare(editing, doc, "Current focused document").
declare(wiki, uuid, "Pointing or editing a wiki file").
declare(web, uuid, "Pointing or viewing a web file").
declare(desktop, bool, "On a desktop").

%! context_stored(+NAME, +VALUE)
%  A dynamic predicate storing the context
:- dynamic context_stored/2 as local.
:- multifile context_stored/2.
context_stored(wiki, UUID) :- get(pointing, wiki(UUID)), !.
context_stored(wiki, UUID) :- get(editing, wiki(UUID)).
context_stored(web, URL) :- get(pointing, url(URL)), !.
context_stored(web, URL) :- get(editing, url(URL)).

%! set(+NAME, +VALUE)
%  Set context NAME to VALUE, parsing VALUE according to the
%  type of NAME.
set(NAME, VALUE) :-
  declare(NAME, TYPE, _),
  parse_type(TYPE, VALUE, PARSED), !,
  asserta(context_stored(NAME, PARSED)).

%! get(+NAME, -VALUE)
%  Get the value of an option
get(NAME, VALUE) :- context_stored(NAME, VALUE).

%! clear(+NAME, +VALUE)
%  Undo the action of set.
clear(NAME, VALUE) :-
  retractall(context_stored(NAME, VALUE)).
