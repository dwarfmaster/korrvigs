
:- module('norg-parser',
         [ parse_norg/2
         , norg_link/2
         ]).
:- use_foreign_library('norg_parser.so').
:- use_module(library(xpath)).

parse_norg(stream(STREAM), DOM) :-
  read_string(STREAM, "", "", _, STR),
  parse_norg_impl(STR, DOM).
parse_norg(file(PATH), DOM) :-
  open(PATH, read, STREAM, []),
  parse_norg(stream(STREAM), DOM).
parse_norg(STR, DOM) :- parse_norg_impl(STR, DOM).

% Some query examples
norg_link(DOM, [TYPE, TARGET, NAME]) :-
  norg_link_kind(DOM, LINK),
  xpath(LINK, //link_location/'*'(@field="type"), element(TYPE, _, _)),
  xpath(LINK, //link_location/'*'(@field="text",text), TARGET),
  ( xpath(LINK, //link_description/'*'(@field="text",text), NAME) -> true ; NAME = TARGET ).

norg_link_kind(DOM, LINK) :-
  xpath(DOM, //link(content), LINK).
norg_link_kind(DOM, LINK) :-
  xpath(DOM, //anchor_definition(content), LINK).



