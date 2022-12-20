
:- module(plugins,
         [ run_action/2
         , run_action_impl/3
         , is_available/4
         , select_action/2 
         , load_all/0
         ]).
:- multifile run_action_impl/3.
:- multifile is_available/4.

user:file_search_path(plugins, D) :-
  plugin_dir(D).

%! run_action(-ACTION, -CONTEXT)
%  ACTION is the specification of the action to run, it can be extended by plugins to
%  support more and more actions. CONTEXT is a list of more specific information about
%  the context in which the action must be run. It can be extended by plugins. Some
%  actions have a result, it must be bound in ACTION
run_action(ACT, CTX) :-
  between(0, 100, N),
  PRIO is 100 - N,
  run_action_impl(PRIO, ACT, CTX), !.

%! select_action(-CTX, +ACTIONS)
%  Given a specific context, return a list of list of actions, sorted by
%  pertinence
select_action_int(CTX, ACTIONS) :-
  between(0, 100, SC),
  SCORE is 100 - SC,
  bagof(P, ACT^NAME^(is_available(CTX, ACT, NAME, SCORE), P = [ NAME, ACT ]), ACTIONS).
select_action(CTX, ACTIONS) :-
  bagof(ACTS, select_action_int(CTX, ACTS), ACTIONS).

%! run_action_impl(-PRIORITY, -ACTION, -CONTEXT)
%  Same as run_action, but with an explicit PRIORITY between 0 and 100 that make the ordering
%  of the rules less important. Higher priorities are given precedence.
%  Priority 0 is reserved for a generic handler that displays a notification that the action
%  couldn't be executed.
%  Priorities [1-9] are for more context specific handlers
run_action_impl(0, ACT, CTX) :-
  format(string(MSG), "Unknown action ~w ctx:~w", [ ACT, CTX ]),
  process_create(path("notify-send"), [ "Korrvigs", MSG ], []).

%! is_available(-CTX, +ACTION, +NAME, ?SCORE)
%  In a given context, indicates if an action is available, and gives a score between 1 and 100
%  of how relevant this action is, and the name of the action.

%! find_plugin(+PATH)
%  find a plugin
find_plugin(PATH) :-
  absolute_file_name(plugins("."), DIR, [access(exist), file_type(directory), solutions(all)]),
  directory_files(DIR, FILES),
  member(FILE, FILES),
  concat(_, ".pl", FILE),
  directory_file_path(DIR, FILE, PATH),
  access_file(PATH, read).

%! load_all()
%  Load all plugins in plugin directory
load_all() :- forall(find_plugin(PATH), load_files(PATH, [])).

