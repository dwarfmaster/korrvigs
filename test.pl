:- use_module(library(http/http_open), [ http_open/3 ]).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_stream)).
:- use_module(library(sgml)).
:- use_module(webcomics).
:- use_module(library(xpath)).

file_load_html(PATH, DOM) :-
  setup_call_cleanup(open(PATH, read, In),
                     ( dtd(html, DTD),
                       load_structure(stream(In),
                                      DOM,
                                      [ dtd(DTD),
                                        dialect(sgml),
                                        shorttag(false),
                                        max_errors(-1),
                                        syntax_errors(quiet)
                                      ])
                     ),
                     close(In)).
http_load_html(URL, DOM) :-
  tmp_file_stream(text, TMP, WRT),
  process_create(path(curl), [ URL ], [ stdout(stream(WRT)), stderr(null) ]),
  close(WRT),
  file_load_html(TMP, DOM).

webcomic_dom(WEBCOMIC, DOM) :-
  webcomic_archive(WEBCOMIC, URL),
  http_load_html(URL, DOM).
webcomic_get_pair(WEBCOMIC, DOM, [ TITLE, URL ]) :-
  webcomic_pages(WEBCOMIC, DOM, TITLE, URL).
webcomic_get_pages(WEBCOMIC, PAGES) :-
  webcomic_dom(WEBCOMIC, DOM),
  bagof(PAIR, webcomic_get_pair(WEBCOMIC, DOM, PAIR), TMP),
  webcomic_pages_post(WEBCOMIC, TMP, PAGES).

sandra_pages(PAGES) :- webcomic_get_pages(sandraandwoo, PAGES).
empowered_pages(PAGES) :- webcomic_get_pages(empowered, PAGES).
