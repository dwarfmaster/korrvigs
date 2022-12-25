
:- module(ctx,
         [ pointing/2
         , viewing/2
         , editing/2
         ]).
:- dynamic pointing/2 as local.
:- dynamic viewing/2 as local.
:- dynamic editing/2 as local.
:- dynamic desktop/0 as local.

viewing(SOFT, DOC) :- editing(SOFT, DOC).
