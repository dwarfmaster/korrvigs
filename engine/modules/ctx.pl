
:- module(ctx,
         [ pointing/1
         , viewing/2
         , editing/2
         , wiki/1
         , url/1
         , path/1
         ]).
:- dynamic pointing/1 as local.
:- dynamic viewing/2 as local.
:- dynamic editing/2 as local.
:- dynamic desktop/0 as local.

viewing(SOFT, DOC) :- editing(SOFT, DOC).

wiki(UUID) :- pointing(wiki(UUID)), !.
wiki(UUID) :- viewing(_, wiki(UUID)).

url(URL) :- pointing(url(URL)), !.
url(URL) :- viewing(_, url(URL)).

path(PATH) :- pointing(path(PATH)), !.
path(PATH) :- viewing(_, path(PATH)).
