:- module(webcomics, [webcomic_archive/2, webcomic_pages/4, webcomic_pages_post/3]).
:- use_module(library(xpath)).

:- discontiguous webcomic_archive/2.
:- discontiguous webcomic_pages/4.
:- discontiguous webcomic_pages_post/3.

% Sandra And Woo
webcomic_archive(sandraandwoo, 'http://www.sandraandwoo.com/archive/').
webcomic_pages(sandraandwoo, DOM, TITLE, URL) :-
  xpath(DOM, //td(@class='archive-title')/a, A),
  xpath(A, /a(@href), URL),
  xpath(A, /a(text), TITLE).
webcomic_pages_post(sandraandwoo, IN, OUT) :-
  reverse(IN, OUT).

% Empowered
webcomic_archive(empowered, 'https://www.empoweredcomic.com/comic/archive').
webcomic_pages(empowered, DOM, TITLE, URL) :-
  xpath(DOM, //select(@name='comic')/option, OPT),
  xpath(OPT, /self(@value), RELATIVE),
  string_length(RELATIVE, REL_LEN),
  REL_LEN > 0,
  string_concat('https://www.empoweredcomic.com/', RELATIVE, URL),
  xpath(OPT, /self(content), TITLE).
webcomic_pages_post(empowered, IN, IN).
