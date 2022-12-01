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
webcomic_pages_pair(WEBCOMIC, DOM, [ TITLE, URL ]) :-
  webcomic_pages(WEBCOMIC, DOM, TITLE, URL).
webcomic_get_pages(WEBCOMIC, DOM, PAGES) :-
  bagof(PAIR, webcomic_pages_pair(WEBCOMIC, DOM, PAIR), TMP),
  webcomic_pages_post(WEBCOMIC, TMP, PAGES).
webcomic_chapters_pair(WEBCOMIC, DOM, [ TITLE, URL ]) :-
  webcomic_chapters(WEBCOMIC, DOM, TITLE, URL).
webcomic_get_chapters(WEBCOMIC, DOM, CHAPTERS) :-
  bagof(PAIR, webcomic_chapters_pair(WEBCOMIC, DOM, PAIR), TMP),
  webcomic_chapters_post(WEBCOMIC, TMP, CHAPTERS).

page_url([ _, URL ], URL).
page_title([ TITLE, _ ], TITLE).

% FROM is the first page of a chapter
chapters_metrics_find(PAGES, CHAPTERS, FROM, _, CHAP, LEFT, NEW, NEW_CHAPTERS) :-
  append([ PAGE ], TL, PAGES),
  append([ CHAP ], TLC, CHAPTERS),
  page_url(PAGE, FROM), page_url(CHAP, FROM), !,
  length(TL, NEW),
  length(TLC, NEW_CHAPTERS),
  chapters_metrics_compute(TL, TLC, 0, LEFT).
% New chapter
chapters_metrics_find(PAGES, CHAPTERS, FROM, _, CURRENT, LEFT, NEW, NEW_CHAPTERS) :-
  append([ PAGE ], TL, PAGES),
  append([ CHAP ], TLC, CHAPTERS),
  page_url(PAGE, URL), page_url(CHAP, URL), !,
  chapters_metrics_find(TL, TLC, FROM, CHAP, CURRENT, LEFT, NEW, NEW_CHAPTERS).
% Find from page
chapters_metrics_find(PAGES, CHAPTERS, FROM, ACTUAL, ACTUAL, LEFT, NEW, NEW_CHAPTERS) :-
  append([ PAGE ], TL, PAGES),
  page_url(PAGE, FROM), !,
  length(TL, NEW),
  length(CHAPTERS, NEW_CHAPTERS),
  chapters_metrics_compute(TL, CHAPTERS, 0, LEFT).
% Pass page
chapters_metrics_find(PAGES, CHAPTERS, FROM, ACTUAL, CURRENT, LEFT, NEW, NEW_CHAPTERS) :-
  append([ _ ], TL, PAGES), !,
  chapters_metrics_find(TL, CHAPTERS, FROM, ACTUAL, CURRENT, LEFT, NEW, NEW_CHAPTERS).
% Count number of pages until next chapter start
chapters_metrics_compute(PAGES, CHAPTERS, LEFT, LEFT) :-
  append([ PAGE ], _, PAGES),
  append([ CHAP ], _, CHAPTERS),
  page_url(PAGE, URL), page_url(CHAP, URL), !.
chapters_metrics_compute(PAGES, [], N, LEFT) :-
  length(PAGES, LEN),
  LEFT = N + LEN.
chapters_metrics_compute([], _, LEFT, LEFT).
chapters_metrics_compute(PAGES, CHAPTERS, N, LEFT) :-
  append([ _ ], TL, PAGES),
  NN is N+1,
  chapters_metrics_compute(TL, CHAPTERS, NN, LEFT).
% CURRENT: current chapter
% LEFT: number of pages left in current chapter
% NEW: new comics in total
% NEW_CHAPTERS: new chapters after current one
chapters_metrics(WEBCOMIC, DOM, FROM, CURRENT, LEFT, NEW, NEW_CHAPTERS) :-
  webcomic_get_pages(WEBCOMIC, DOM, PAGES),
  webcomic_archive(WEBCOMIC, URL),
  webcomic_get_chapters(WEBCOMIC, DOM, CHAPTERS),
  chapters_metrics_find(PAGES, CHAPTERS, FROM, [ "Archive", URL ], CURRENT, LEFT, NEW, NEW_CHAPTERS).

empowered_pages(PAGES) :-
  file_load_html("empowered.html", DOM),
  webcomic_get_pages(empowered, DOM, PAGES).
empowered_chapters(CHAPTERS) :-
  file_load_html("empowered.html", DOM),
  webcomic_get_chapters(empowered, DOM, CHAPTERS).
empowered_metrics(CHAP, L, N, NC) :-
  file_load_html("empowered.html", DOM),
  chapters_metrics(empowered, DOM, "https://www.empoweredcomic.com/comic/volume-2-page-80", CHAP, L, N, NC).
