%%
%% @doc Sanitize user input. Not an easy task, see inline comments
%%
-module(hint_query_parser).
-compile([debug_info, export_all]).
%% Main export
-export([sanitize/1]).
%% Utilities
-export([ arity/1
        , function/1
        , guards/1
        , module/1
        , params/1
        , return/1
        , spec_string/1
        , string/1]).

%%
%% Defines
%%
-record(req, { string      = ""
             , spec_string = ""
             , module      = ""
             , function    = ""
             , arity       = 0
             , params      = ""
             , guards      = ""
             , return      = ""
             }).

-type req() :: #req{}.

%%
%% Helper defines
%%

-define(lrf(L), lists:reverse(lists:flatten(L))).
-define(is_alphanum(C), C >= $a, C =< $z; C >= 0, C =< 9).
-define(strip(S), string:strip(string:strip(S, both, $\t), both, 32)).
-define(is_open_pbb(C),  C =:= $( orelse C =:= ${ orelse C =:= $[).
-define(is_close_pbb(C), C =:= $) orelse C =:= $} orelse C =:= $]).
-define(is_pbb(C), (?is_open_pbb(C)) orelse (?is_close_pbb(C))).
-define(is_space(C), C =:= 32; C =:= $\t).


%%
%% API
%%
-spec sanitize(string()) -> req().
sanitize(Query) ->
  {M, F, P, G, R} = parse_parts(Query),
  Module   = sanitize_module(M),
  Function = sanitize_function(F),
  Params   = sanitize_params(P),
  Guards   = sanitize_guards(G),
  Return   = sanitize_return(R),
  Arity    = parse_arity(Params),

  SpecString  = build_spec_string(Params, Guards, Return),
  SanitizedString = build_sanitized_string(Module, Function, SpecString),
  #req{ string      = SanitizedString
      , spec_string = SpecString
      , module      = Module
      , function    = Function
      , arity       = Arity
      , params      = Params
      , guards      = Guards
      , return      = Return
      }.

-spec arity(req()) -> integer().
arity(#req{arity = X}) -> X.

-spec function(req()) -> string().
function(#req{function = X}) -> X.

-spec guards(req()) -> string().
guards(#req{guards = X}) -> X.

-spec module(req()) -> string().
module(#req{module = X}) -> X.

-spec params(req()) -> string().
params(#req{params = X}) -> X.

-spec return(req()) -> string().
return(#req{return = X}) -> X.

-spec spec_string(req()) -> string().
spec_string(#req{spec_string = X}) -> X.

-spec string(req()) -> string().
string(#req{string = X}) -> X.

%%
%% Internal
%%

%%
%% @doc Will roughly split a string into 5 parts:
%%      Module, Function, Params, Guards, Return
%%
-spec parse_parts(string()) ->
                     {string(), string(), string(), string(), string()}.
parse_parts(S) ->
  parse_module(S).

-spec sanitize_module(string()) -> string().
sanitize_module(S) ->
  sanitize_module_or_fun(S).

-spec sanitize_function(string()) -> string().
sanitize_function(S) ->
  sanitize_module_or_fun(S).

-spec sanitize_params(string()) -> string().
sanitize_params(S) ->
  sanitize_specs(S).

-spec sanitize_guards(string()) -> string().
sanitize_guards(S) ->
  S.

-spec sanitize_return(string()) -> string().
sanitize_return(S) ->
  S.

%% since
-spec parse_arity(string()) -> integer().
parse_arity(S) ->
  S.

-spec build_spec_string(string(), string(), string()) -> string().
build_spec_string(_Params, _Guards, _Return) ->
  ok.

-spec build_sanitized_string(string(), string(), string()) -> string().
build_sanitized_string(_Module, _Function, _Specs) ->
  ok.


%%
%% ** Parsing parts **
%%

parse_module(S) -> parse_module(S, []).
%% Reached end of line, no other parts found
parse_module([], Acc) -> {?lrf(Acc), "", "", "", ""};
%% Suddenly reached return
parse_module([$-, $>|T], Acc) ->
  Return = parse_return(T),
  {?lrf(Acc), "?", "", "", Return};
%% Suddenly reached guards
parse_module([$w, $h, $e, $n|T], Acc) ->
  {Guards, Return} = parse_guards(T),
  {?lrf(Acc), "?", "", Guards, Return};
%% Reached beginning of parameter definition
%% TODO: decide whether this is module name or function name
parse_module([$(|T], Acc) ->
  {Params, Guards, Return} = parse_params(T),
  {?lrf(Acc), "?", Params, Guards, Return};
%% reached end of module definition
parse_module([$:|T], Acc) ->
  {Function, Params, Guards, Return} = parse_function(T),
  {?lrf(Acc), Function,  Params, Guards, Return};
parse_module([H|T], Acc)  -> parse_module(T, [H|Acc]).


parse_function(S) -> parse_function(S, []).
%% Reached end of line, no other parts found
parse_function([], Acc) -> {?lrf(Acc), "", "", ""};
%% Suddenly reached return
parse_function([$-, $>|T], Acc) ->
  Return = parse_return(T),
  {?lrf(Acc), "", "", Return};
%% Suddenly reached guards
parse_function([$w, $h, $e, $n|T], Acc) ->
  {Guards, Return} = parse_guards(T),
  {?lrf(Acc), "", Guards, Return};
%% Reached end of function definition
parse_function([$(|T], Acc) ->
  {Params, Guards, Return} = parse_params(T),
  {?lrf(Acc), Params, Guards, Return};
parse_function([H|T], Acc)  -> parse_function(T, [H|Acc]).

%% params is anything until we hit guards or return or eol
parse_params(S) -> parse_params(S, []).
%% Reached end of line, no other parts found
parse_params([], Acc) -> {?lrf(Acc), "", ""};
%% Suddenly reached return
parse_params([$-, $>|T], Acc) ->
  Return = parse_return(T),
  {?lrf(Acc), "", Return};
%% Suddenly reached guards
parse_params([$w, $h, $e, $n|T], Acc) ->
  {Guards, Return} = parse_guards(T),
  {?lrf(Acc), Guards, Return};
parse_params([H|T], Acc)  -> parse_params(T, [H|Acc]).

%% guards until we hit return or eol
parse_guards(S) -> parse_guards(S, []).
%% Reached end of line, no other parts found
parse_guards([], Acc) -> {?lrf(Acc), ""};
%% Suddenly reached return
parse_guards([$-, $>|T], Acc) ->
  Return = parse_return(T),
  {?lrf(Acc), Return};
parse_guards([H|T], Acc)  -> parse_guards(T, [H|Acc]).

%% return is anything that's left'
parse_return(T) -> T.



%%
%% ** Sanitizing **
%%

%% Module is:
%%   _
%%   ?
%%   alphanumeric, can contain _ anywhere but first symbol
sanitize_module_or_fun([])     -> "?";
sanitize_module_or_fun([$_|_]) -> "_";
sanitize_module_or_fun([$?|_]) -> "?";
sanitize_module_or_fun(S)      -> sanitize_module_or_fun(S, []).

sanitize_module_or_fun([], [])  -> "?";
sanitize_module_or_fun([], Acc) -> ?lrf(Acc);
sanitize_module_or_fun([H|T], Acc) when ?is_alphanum(H); H =:= $_ ->
  sanitize_module_or_fun(T, [H|Acc]);
sanitize_module_or_fun([_|T], Acc) ->
  sanitize_module_or_fun(T, Acc).


%%
%% Specs are fun and PITA
%%
%% ** Specs are tricky: they can have several nested brackets/braces/parens
%% ** They can have ":" and "::" in definitions
%% ** They end when we encounter ")" on level 0 of nested parens
%% ** If brackets/braces/parens are unmatched, we skip until we match or
%% **    until we hit the end of string, at wich point we balance them out
%% ** Spaces can occur only once after ",", ":", "::", brackets/braces/parens
%% ** Spaces inside param names are skiped
%% ** Params can only be alphanumeric (uppercase and lowercase are ok)
%% ** Cannot start with a number
%% ** Can contain a single "_" at the beginning of the word
%% ** Can contain an unlimited number of "_" anywehere else in the name
%% ** A single "_" is also a valid parameter name
%%
%% First balance out parentheses, brackets and braces. We call them pbb
sanitize_specs(S) ->
  Balanced = balance_pbb(S),
  do_sanitize_specs(Balanced).


balance_pbb(S) ->
  balance_pbb(S, [], []).

-spec balance_pbb(string(), Pbb::list(), string()) ->string().
%% skip all whitespace, makes job easier below
balance_pbb([C|T], Pbb, Acc) when ?is_space(C) ->
  balance_pbb(T, Pbb, Acc);
%% end of story, simplest case
balance_pbb([], [], Acc) -> ?lrf(Acc);
%% end of story, unbalanced pbb
balance_pbb([], [_|_]=Pbb, Acc0) ->
  Acc = unwind_pbb(Pbb, Acc0),
  ?lrf(Acc);
%% pbb, possibly rebalance pbbs
%% we skip unbalanced closing pbbs
balance_pbb([C|T], Pbb0, Acc0) when ?is_pbb(C) ->
  {Acc, Pbb} = maybe_unwind_pbb(Pbb0, C),
  balance_pbb(T, Pbb, [Acc | Acc0]);
%% anything else just goes in
balance_pbb([H|T], Pbb, Acc) ->
  balance_pbb(T, Pbb, [H|Acc]).


unwind_pbb([], Acc)     -> Acc;
unwind_pbb([$(|T], Acc) -> unwind_pbb(T, [$) | Acc]);
unwind_pbb([$[|T], Acc) -> unwind_pbb(T, [$] | Acc]);
unwind_pbb([${|T], Acc) -> unwind_pbb(T, [$} | Acc]).

maybe_unwind_pbb([], C) when ?is_open_pbb(C)  ->
  {[C], [C]};
maybe_unwind_pbb([], C) when ?is_close_pbb(C) ->
  {[], []};
maybe_unwind_pbb(Pbbs0, C) when ?is_close_pbb(C) ->
  {Acc, Pbb} = find_matching_pbb(Pbbs0, C),
  {lists:reverse(Acc), Pbb};
maybe_unwind_pbb(Pbb, C) ->
  {[C], [C | Pbb]}.

find_matching_pbb(Pbbs0, C) ->
  find_matching_pbb(Pbbs0, C, []).

%% found no matching Pbb
find_matching_pbb([], _, Acc) ->
  {[], lists:reverse(Acc)};
find_matching_pbb([$(=C1|T], $) = C, Acc) ->
  {unwind_pbb([C1|Acc], []), T};
find_matching_pbb([$[=C1|T], $] = C, Acc) ->
  {unwind_pbb([C1|Acc], []), T};
find_matching_pbb([${=C1|T], $} = C, Acc) ->
  {unwind_pbb([C1|Acc], []), T};
find_matching_pbb([C1|T], C, Acc)         ->
  find_matching_pbb(T, C, [C1 | Acc]).

do_sanitize_specs(S) ->
  do_sanitize_specs(S, start, []).

%% see priv/sanitize.png
-define(is_alpha(C),        (C >= $a andalso C =< $z)
                     orelse (C >= $A andalso C =< $Z)).
-define(is_num(C),   (C >= $0 andalso C =< $9)).
-define(is_under(C), C =:= $_).
-define(is_comma(C), C =:= $,).
-define(is_alt(C),   C =:= $|).
-define(is_colon(C), C =:= $:).
-define(not_any(C),   not (?is_alpha(C)), not (?is_num(C)), not (?is_under(C))
                    , not (?is_comma(C)), not (?is_alt(C)), not (?is_colon(C))).


-spec do_sanitize_specs(string(), State::atom(), string()) -> string().
do_sanitize_specs([], start, Acc) ->
  ?lrf(Acc);
do_sanitize_specs([], state_1, Acc) ->
  ?lrf(Acc);
do_sanitize_specs([], state_3, Acc) ->
  ?lrf(Acc);
do_sanitize_specs([], state_4, Acc) ->
  ?lrf(Acc);
do_sanitize_specs([], state_5, Acc) ->
  ?lrf([$_|Acc]);
do_sanitize_specs([], state_6, Acc) ->
  ?lrf([$_|Acc]);
do_sanitize_specs([], state_7, Acc) ->
  ?lrf(Acc);
%% start
do_sanitize_specs(S, start, Acc) ->
  do_sanitize_specs(S, state_1, Acc);
%% state 1
do_sanitize_specs([C|T], state_1, Acc) when ?is_alpha(C); ?is_under(C) ->
  do_sanitize_specs(T, state_3, [C|Acc]);
do_sanitize_specs([C|T], state_1, Acc) when ?is_open_pbb(C) ->
  do_sanitize_specs(T, state_2, [C|Acc]);
do_sanitize_specs([C|T], state_1, Acc) when ?not_any(C) ->
  do_sanitize_specs(T, state_2, [$_|Acc]);
do_sanitize_specs(S, state_1, Acc) ->
  do_sanitize_specs(S, state_3, [$_|Acc]);
%% state 2
do_sanitize_specs([C|T], state_2, Acc) when ?is_alpha(C); ?is_under(C) ->
  do_sanitize_specs(T, state_3, [C|Acc]);
do_sanitize_specs([C|T], state_2, Acc) when ?is_open_pbb(C) ->
  do_sanitize_specs(T, state_2, [C|Acc]);
do_sanitize_specs([C|T], state_2, Acc) when ?is_close_pbb(C) ->
  do_sanitize_specs(T, state_4, [C|Acc]);
do_sanitize_specs([C|T], state_2, Acc) when ?not_any(C) ->
  do_sanitize_specs(T, state_3, [$_|Acc]);
do_sanitize_specs(S, state_2, Acc) ->
  do_sanitize_specs(S, state_3, [$_|Acc]);
%% state 3
do_sanitize_specs([C|T], state_3, Acc) when   ?is_alpha(C); ?is_num(C)
                                            ; ?is_under(C)->
  do_sanitize_specs(T, state_3, [C|Acc]);
do_sanitize_specs([C|T], state_3, Acc) when ?is_comma(C); ?is_alt(C) ->
  do_sanitize_specs(T, state_1, [C|Acc]);
do_sanitize_specs([C|T], state_3, Acc) when ?is_colon(C) ->
  do_sanitize_specs(T, state_5, [C|Acc]);
do_sanitize_specs([C|T], state_3, Acc) when ?is_open_pbb(C) ->
  do_sanitize_specs(T, state_2, [C|Acc]);
do_sanitize_specs([C|T], state_3, Acc) when ?is_close_pbb(C) ->
  do_sanitize_specs(T, state_4, [C|Acc]);
do_sanitize_specs([C|T], state_3, Acc) when ?not_any(C) ->
  do_sanitize_specs(T, state_3, [$_|Acc]);
do_sanitize_specs(S, state_3, Acc) ->
  do_sanitize_specs(S, state_3, [$_|Acc]);
%% state 4
do_sanitize_specs([C|T], state_4, Acc) when ?is_close_pbb(C) ->
  do_sanitize_specs(T, state_4, [C|Acc]);
do_sanitize_specs([C|T], state_4, Acc) when ?is_comma(C) ->
  do_sanitize_specs(T, state_1, [C|Acc]);
do_sanitize_specs([C|T], state_4, Acc) when ?not_any(C) ->
  do_sanitize_specs(T, state_3, [$_, $, |Acc]);
do_sanitize_specs(S, state_4, Acc) ->
  do_sanitize_specs(S, state_3, [$,|Acc]);
%% state 5
do_sanitize_specs([C|T], state_5, Acc) when ?is_alpha(C); ?is_under(C) ->
  do_sanitize_specs(T, state_3, [C|Acc]);
do_sanitize_specs([C|T], state_5, Acc) when ?is_colon(C) ->
  do_sanitize_specs(T, state_6, [C|Acc]);
do_sanitize_specs([C|T], state_5, Acc) when ?not_any(C) ->
  do_sanitize_specs(T, state_3, [$_|Acc]);
do_sanitize_specs(S, state_5, Acc) ->
  do_sanitize_specs(S, state_7, [$_|Acc]);
%% state 6
do_sanitize_specs([C|T], state_6, Acc) when ?is_alpha(C); ?is_under(C) ->
  do_sanitize_specs(T, state_3, [C|Acc]);
do_sanitize_specs([C|T], state_6, Acc) when ?not_any(C) ->
  do_sanitize_specs(T, state_7, [$_ |Acc]);
do_sanitize_specs(S, state_5, Acc) ->
  do_sanitize_specs(S, state_7, [$_|Acc]);
%% state 7
do_sanitize_specs([C|T], state_6, Acc) when   ?is_alpha(C); ?is_under(C)
                                            ; ?is_num(C) ->
  do_sanitize_specs(T, state_3, [C|Acc]);
do_sanitize_specs([C|T], state_6, Acc) when ?is_close_pbb(C) ->
  do_sanitize_specs(T, state_4, [C|Acc]);
do_sanitize_specs([C|T], state_6, Acc) when ?is_alt(C); ?is_comma(C) ->
  do_sanitize_specs(T, state_1, [C|Acc]);
do_sanitize_specs([C|T], state_6, Acc) when ?not_any(C) ->
  do_sanitize_specs(T, state_7, [$_ |Acc]);
do_sanitize_specs(S, state_5, Acc) ->
  do_sanitize_specs(S, state_4, Acc).
