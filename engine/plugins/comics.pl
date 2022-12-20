
:- module(comics,
         [ webcomic_archive/2
         , webcomic_pages/4
         , webcomic_pages_post/3
         , webcomic_chapters/4
         , webcomic_chapters_post/3
         , webcomic_identity/2
         ]).
:- multifile webcomic_archive/2.
:- multifile webcomic_pages/4.
:- multifile webcomic_pages_post/3.
:- multifile webcomic_chapters/4.
:- multifile webcomic_chapters_post/3.
:- multifile webcomic_identity/2.
:- use_module(korrvigs(plugins)).
:- use_module(korrvigs(wiki)).

:- use_module(library(http/http_open), [ http_open/3 ]).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_stream)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).


%     _        _   _                 
%    / \   ___| |_(_) ___  _ __  ___ 
%   / _ \ / __| __| |/ _ \| '_ \/ __|
%  / ___ \ (__| |_| | (_) | | | \__ \
% /_/   \_\___|\__|_|\___/|_| |_|___/
%                                    
% Actions

wiki(CTX, UUID) :- member(pointing(wiki(UUID)), CTX), !.
wiki(CTX, UUID) :- member(reading(wiki(UUID)), CTX).
url(CTX, URL) :- member(pointing(url(URL)), CTX), !.
url(CTX, URL) :- member(reading(url(URL)), CTX).

webcomic_continue_uuid(UUID_STR,URL,TITLE) :-
  atom_string(UUID, UUID_STR),
  wiki_file(PATH, UUID),
  wiki:get_attributes(PATH, ATTRS),
  _{ webcomic: _, 'last-read': URL, 'doctitle': TITLE } :< ATTRS.

plugins:run_action_impl(1, webcomic_continue, CTX) :-
  wiki(CTX,UUID),
  webcomic_continue_uuid(UUID,URL,_),
  plugins:run_action(open_url(URL), CTX).
plugins:is_available(CTX, open_url(URL), DESC, 100) :-
  wiki(CTX, UUID),
  webcomic_continue_uuid(UUID, URL, TITLE),
  concat("Continue ", TITLE, DESC).

plugins:run_action_impl(1, webcomic_update, CTX) :-
  wiki(CTX, UUID_STR),
  atom_string(UUID, UUID_STR),
  wiki:wiki_file(PATH,UUID),
  wiki:get_attributes(PATH, ATTRS),
  _{ webcomic: COMIC_STR, 'last-read': LAST } :< ATTRS,
  atom_string(COMIC, COMIC_STR),
  wiki:include_extra(UUID, ATTRS),
  webcomic_archive(COMIC, URL),
  load_html_file(URL, DOM),
  webcomic_metrics(COMIC, DOM, LAST, [_, CHAP], LEFT, NEW, NEW_CHAPTERS),
  wiki:set_attribute(PATH, 'chapter', CHAP),
  wiki:set_attribute(PATH, 'new-in-chapter', LEFT),
  wiki:set_attribute(PATH, 'new-pages', NEW),
  wiki:set_attribute(PATH, 'new-chapters', NEW_CHAPTERS).
plugins:is_available(CTX, webcomic_update, DESC, 90) :-
  wiki(CTX, UUID_STR),
  atom_string(UUID, UUID_STR),
  wiki_file(PATH,UUID),
  wiki:get_attributes(PATH, ATTRS),
  _{ webcomic: _, 'last-read': _, 'doctitle': TITLE } :< ATTRS,
  concat("Update metric on ", TITLE, DESC).

% TODO run webcomic_update afterwards
plugins:run_action_impl(1, webcomic_save, CTX) :-
  url(CTX, URL), !,
  wiki:wiki_file(PATH, UUID),
  wiki:get_attributes(PATH, ATTRS),
  _{ 'webcomic': COMIC_STR } :< ATTRS, !,
  atom_string(COMIC, COMIC_STR),
  wiki:include_extra(UUID, ATTRS),
  webcomic_identify(COMIC, URL), !,
  wiki:set_attribute(PATH, 'last-read', URL).
plugins:is_available(CTX, adoc_set_attr(PATH, 'last-read', URL), DESC, 100) :-
  url(CTX, URL), !,
  wiki:wiki_file(PATH, UUID),
  wiki:get_attributes(PATH, ATTRS),
  _{ 'webcomic': COMIC_STR, 'doctitle': TITLE } :< ATTRS,
  atom_string(COMIC, COMIC_STR),
  wiki:include_extra(UUID, ATTRS),
  webcomic_identify(COMIC, URL), !,
  concat("Remember reading status for ", TITLE, DESC).


%  _   _ _   _ _ _ _   _           
% | | | | |_(_) (_) |_(_) ___  ___ 
% | | | | __| | | | __| |/ _ \/ __|
% | |_| | |_| | | | |_| |  __/\__ \
%  \___/ \__|_|_|_|\__|_|\___||___/
%                                  
% Utilities

%! file_load_html(-PATH, +DOM) is det
%  Load and parse an html file at PATH, giving its DOM
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
%! http_load_html(-URL, +DOM) is det
%  Load and parse an html file online at URL, giving its DOM
http_load_html(URL, DOM) :-
  tmp_file_stream(text, TMP, WRT),
  process_create(path(curl), [ URL ], [ stdout(stream(WRT)), stderr(null) ]),
  close(WRT),
  file_load_html(TMP, DOM).

%! webcomic_dom(-WEBCOMIC, +DOM) is det
%  Given a webcomic id, parse the relevant page
webcomic_dom(WEBCOMIC, DOM) :-
  webcomic_archive(WEBCOMIC, URL),
  http_load_html(URL, DOM).
%! webcomic_pages_pair(-WEBCOMIC, -DOM, +PAIR) is nondet
%  Get a pair of a title and an url of a page of a webcomic, found
%  by scrapping the DOM of its archive page. No guarantee is made on
%  the order
webcomic_pages_pair(WEBCOMIC, DOM, [ TITLE, URL ]) :-
  webcomic_pages(WEBCOMIC, DOM, TITLE, URL).
%! webcomic_get_pages(-WEBCOMIC, -DOM, +PAGES) is det
%  Get all PAGES as pair of a webcomic, in the chronological order
webcomic_get_pages(WEBCOMIC, DOM, PAGES) :-
  bagof(PAIR, webcomic_pages_pair(WEBCOMIC, DOM, PAIR), TMP),
  webcomic_pages_post(WEBCOMIC, TMP, PAGES).
%! webcomic_chapters_pair(-WEBCOMIC, -DOM, +PAIR) is nondet
%  Same as webcomic_pages_pair but for chapters
webcomic_chapters_pair(WEBCOMIC, DOM, [ TITLE, URL ]) :-
  webcomic_chapters(WEBCOMIC, DOM, TITLE, URL).
%! webcomic_get_chapters(-WEBCOMIC, -DOM, +CHAPTERS) is det
%  Same as webcomic_get_pages but for chapters
webcomic_get_chapters(WEBCOMIC, DOM, CHAPTERS) :-
  bagof(PAIR, webcomic_chapters_pair(WEBCOMIC, DOM, PAIR), TMP),
  webcomic_chapters_post(WEBCOMIC, TMP, CHAPTERS).

page_url([ _, URL ], URL).
page_title([ TITLE, _ ], TITLE).

%! chapters_metrics_find(-PAGES, -CHAPTERS, -FROM, -ACTUAL, +CHAP, +LEFT, +NEW, +NEW_CHAPTERS) is det
%  Given the PAGES and CHAPTERS of a webcomic, compute the current chapter CHAP,
%  the number of pages to the end of the chapter +LEFT, the number of pages left to
%  the end of the webcomic +NEW, and the number of new chapters +NEW_CHAPTERS from
%  a page FROM. ACTUAL is the default chapter, used if FROM is not in any chapters.

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

%! chapters_metrics_compute(-PAGES, -CHAPTERS, -N, +LEFT)
%  Given a list of PAGES and CHAPTERS, compute the number of pages
%  before the beggining of the new chapters LEFT, using N as an accumulator
%  (so should be set to 0 to get the actual value).

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

%! webcomic_metrics(-WEBCOMIC, -DOM, -FROM, +CURRENT, +LEFT, +NEW, +NEW_CHAPTERS) is det
%  Given a WEBCOMIC and the DOM of its archive, compute the current chapter CURRENT,
%  the number of pages to the end of the chapter +LEFT, the number of pages left to
%  the end of the webcomic +NEW, and the number of new chapters +NEW_CHAPTERS from
%  a page FROM. If there are no chapters, CURRENT is the archive page.
webcomic_metrics(WEBCOMIC, DOM, FROM, CURRENT, LEFT, NEW, NEW_CHAPTERS) :-
  webcomic_get_pages(WEBCOMIC, DOM, PAGES),
  webcomic_archive(WEBCOMIC, URL),
  webcomic_get_chapters(WEBCOMIC, DOM, CHAPTERS),
  chapters_metrics_find(PAGES, CHAPTERS, FROM, [ "Archive", URL ], CURRENT, LEFT, NEW, NEW_CHAPTERS).

