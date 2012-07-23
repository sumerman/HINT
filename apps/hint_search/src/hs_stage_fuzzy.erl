%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 HINT Spawnfest Team
%%%-------------------------------------------------------------------

-module(hs_stage_fuzzy).

-behaviour(hs_stage).

-export([apply/2, prepare/3]).

-export_type([state/0]).

-record(state, {
    rex :: re:mp()
    }).

-opaque state() :: #state{}.

apply(#state{ rex=Re }, {Rank, {M,F,_A}=MFA, Ex}) ->
  case re:run(mf_to_s(M,F), Re) of
    {match, MList} ->
      Delta = length([Idx || {S,_}=Idx <- MList, S >= 0]),
      {Rank+Delta, MFA, Ex};
    nomatch -> 
      skip
  end.

prepare(_Opts, _PLT, Req) ->
  M = hint_search_req:module(Req),
  F = hint_search_req:func(Req),
  S = mf_to_s(M, F),
  R = str_to_rex(S),
  {ok, C} = compile(R),
  {ok, #state{
      rex = C
      }}.

mf_to_s(M, F) ->
  [to_l(M), " ", to_l(F)].

str_to_rex(S) ->
  S1 = unicode:characters_to_list(iolist_to_binary(S)),
  S2 = re:split(S1, "\\s", [{return, list}]),
  R1 = [[quote(Ch) || Ch <- Si] || Si <- S2],
  R2 = string:join(R1, quote(" ")),
  [R2, "$"].

quote(L) ->
  ["(", L, ").*"].

compile(RexStr) ->
  io:format("~nREEEX ~s~n", [RexStr]),
  re:compile(RexStr, [unicode]).

to_l(L) when is_list(L) -> L;
to_l(L) when is_atom(L) ->
  atom_to_list(L);
to_l(L) when is_integer(L) ->
  integer_to_list(L);
to_l(L) when is_binary(L) ->
  binary_to_list(L).
