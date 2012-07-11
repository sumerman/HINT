%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 HINT Spawnfest Team
%%%-------------------------------------------------------------------

-module(hs_engine_types).

-export([q/1]).
-export([exec_stage/3]).

q(Request) ->
	Req  = hint_search_req:new(Request),
	Module = hint_search_req:module(Req),
	Arity = hint_search_req:arity(Req),
	Sigs = plt_cache_server:lookup(Module, Arity),
	plt_cache_server:apply({?MODULE, exec_stage, [Req, Sigs]}).

exec_stage(PLT, Req, Sigs) ->
	{ok, S} = hs_stage_types:prepare([], PLT, Req),
	Module = hint_search_req:module(Req),
	rank(S, Module, Sigs).

rank(S, Module, MFAs) -> 
	F = fun({Mod, Fun, _} = MFA) ->
			RankMod = rank_module(Module, Mod),	
			RankFun = rank_fun(Fun),
			{MFA, Rank, _} = hs_stage_types:apply(S, {MFA, 1, []}),
			{MFA, lists:sum([RankMod, RankFun, Rank, 1])}
	end,
	Result = lists:map(F, MFAs),
	lists:reverse(lists:keysort(2, Result)).
	
rank_module(Original, Mod) when is_list(Original)->
	rank_module(binary_to_atom(iolist_to_binary(Original), latin1), Mod);
rank_module(Mod, Mod) ->
	0.5;
rank_module(_, Mod) ->
	Length    = length(atom_to_list(Mod)),
	if 
		Length > 2, Length < 7 -> 0;
		true -> 
			Deviation = math:pow(erlang:abs(10 - Length), 2),
			-1*Deviation*0.1
	end.

rank_fun(Fun) ->
	Length = length(atom_to_list(Fun)),
	if
		Length > 10 -> -1*0.005;
		true        -> 0
	end.


