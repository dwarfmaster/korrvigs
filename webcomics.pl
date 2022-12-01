:- module(webcomics, [ webcomic_archive/2, webcomic_pages/4, webcomic_pages_post/3
                     , webcomic_chapters/4, webcomic_chapters_post/3 ]).
:- use_module(library(xpath)).

:- discontiguous webcomic_archive/2.
:- discontiguous webcomic_pages/4.
:- discontiguous webcomic_pages_post/3.
:- discontiguous webcomic_chapters/4.
:- discontiguous webcomic_chapters_post/3.

% Sandra And Woo
webcomic_archive(sandraandwoo, "http://www.sandraandwoo.com/archive/").
webcomic_pages(sandraandwoo, DOM, TITLE, URL) :-
  xpath(DOM, //td(@class='archive-title')/a, A),
  xpath(A, /a(@href), URL_ATOM), atom_string(URL_ATOM, URL),
  xpath(A, /a(text), TITLE_ATOM), atom_string(TITLE_ATOM, TITLE).
webcomic_pages_post(sandraandwoo, IN, OUT) :-
  reverse(IN, OUT).
webcomic_chapters_post(sandraandwoo, IN, IN).

% Empowered
empowered_url(REL, URL) :-
  string_concat("https://www.empoweredcomic.com/", REL, URL).
webcomic_archive(empowered, "https://www.empoweredcomic.com/comic/archive").
webcomic_pages(empowered, DOM, TITLE, URL) :-
  xpath(DOM, //select(@name='comic')/option, OPT),
  xpath(OPT, /self(@value), RELATIVE),
  string_length(RELATIVE, REL_LEN),
  REL_LEN > 0,
  empowered_url(RELATIVE, URL),
  xpath(OPT, /self(content), [TITLE_ATOM]), atom_string(TITLE_ATOM, TITLE).
webcomic_pages_post(empowered, IN, IN).
webcomic_chapters(empowered, DOM, TITLE, URL) :-
  xpath(DOM, //div(@class='cc-storyline-contain')/div/div(@class='cc-storyline-header')/a, A),
  xpath(A, /self(@href), URL_ATOM), atom_string(URL_ATOM, URL),
  xpath(A, /self(content), [TITLE_ATOM]), atom_string(TITLE_ATOM, TITLE).
webcomic_chapters_post(empowered, IN, IN).
