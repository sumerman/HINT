%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 HINT Spawnfest Team
%%%-------------------------------------------------------------------

-module(hint_no_dial).

-compile(export_all).

temp_mod_name() -> 
	atom_to_list(?MODULE) ++ "_temp_mod".

temp_file() ->
	Ext = ".erl",
	Dir = mochitemp:mkdtemp(),
	filename:join(Dir, [temp_mod_name(), Ext]).

rm_temp_file(File) ->
	mochitemp:rmtempdir(filename:dirname(File)).

extend_req(Req) ->
	F = hint_search_req:func(Req), 
	S = hint_search_req:string(Req),
	["-module('",temp_mod_name(),"').\n"
		"-spec '", to_s(F), "'", S, "."].

compile_request(PLT, Req) ->
	File = temp_file(),
	Data = extend_req(Req),
	ok = file:write_file(File, Data),
	{ok, AbstractCode} = epp:parse_file(File, [], []),
	rm_temp_file(File),
	CS0 = dialyzer_codeserver:new(),
	{ok, RecDict} = dialyzer_utils:get_record_and_type_info(AbstractCode),
	Mod = list_to_atom(temp_mod_name()),
	{ok, SpecDict, CbDict} = dialyzer_utils:get_spec_info(Mod, AbstractCode, RecDict),
	CS1 = dialyzer_codeserver:store_temp_records(Mod, RecDict, CS0),
	CS2 = dialyzer_codeserver:store_temp_contracts(Mod, SpecDict, CbDict, CS1),
	CS3 = process_remote_types(CS2, PLT), 
	F = hint_search_req:func(Req), 
	A = hint_search_req:arity(Req),
	{ok, {_, C}} = dialyzer_codeserver:lookup_mfa_contract({Mod, F, A}, CS3),
	dialyzer_codeserver:delete(CS3),
	{ok, {dialyzer_contracts:get_contract_return(C),
			dialyzer_contracts:get_contract_args(C)}}.

test_ranks(String) ->
	PLT = dialyzer_plt:from_file(dialyzer_plt:get_default_plt()),
	Req = hint_search_req:new(String),
	{ok, RTS} = compile_request(PLT, Req),
	Modules = sets:to_list(dialyzer_plt:all_modules(PLT)),
	Fun = fun(Mod) ->
			{value, Sigs} = dialyzer_plt:lookup_module(PLT, Mod),
			[{MFA, test_rank(RTS, {FTR,FTA}), erl_types:t_to_string(erl_types:t_fun(FTA,FTR))} ||
				{{_,_,A}=MFA, FTR, FTA} <- Sigs,
				A =:= hint_search_req:arity(Req)]
	end,
	lists:reverse(lists:keysort(2,lists:flatmap(Fun,Modules))).

test_rank({RTR, RTA}, FTS) ->
	FTSL = ts_to_list(FTS),
	lists:max([
		lists:sum([rank(T1, T2) || 
					{T1, T2} <- lists:zip([RTR|RTAR], FTSL)]) ||
			RTAR <- rotations(RTA)]).

ts_to_list({Ret, L}) -> 
	[Ret | L].

rank(T1, T2) ->
	try {erl_types:t_to_tlist(T1), 
			erl_types:t_to_tlist(T2)} of
		{[_T],TL} ->
			lists:max([rank_(T1, T2i) || T2i <- TL]);
		_  -> 
			rank_(T1, T2)
	catch
		_:_ -> 
			rank_(T1, T2)
	end.

rank_(T1, T2) ->
	Eql = erl_types:t_is_equal(T1, T2),
	Ins = erl_types:t_is_instance(T1, T2),
	Sub = erl_types:t_is_subtype(T1, T2),
	Sup = erl_types:t_is_subtype(T2, T1),
	Any = (T2 == erl_types:t_any()),
	if
		Eql  -> 0.9;
		Sup  -> 0;
		Sub  -> 
			if
				Any  -> -0.1;
				true ->  0.2
			end;
		Ins  -> 0.3;
		true -> -0.5
	end.

rotate([H|L]) ->
	L ++ [H].

rotations(L) ->
	tl(lists:foldl(
		fun(_,R) ->
			R1 = rotate(hd(R)),
			[R1|R]
			end, [L], L)).

process_remote_types(CodeServer1, PLT) ->
	NewRecords = dialyzer_codeserver:get_temp_records(CodeServer1),
	OldRecords = dialyzer_plt:get_types(PLT), 
	OldExTypes = dialyzer_plt:get_exported_types(PLT),
	MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
	CodeServer2 = dialyzer_codeserver:set_temp_records(MergedRecords, CodeServer1),
	CodeServer3 = dialyzer_codeserver:insert_temp_exported_types(OldExTypes, CodeServer2),
	CodeServer4 = dialyzer_utils:process_record_remote_types(CodeServer3),
	dialyzer_contracts:process_contract_remote_types(CodeServer4).

to_s(A) when is_atom(A) ->
	atom_to_binary(A, latin1);
to_s(I) when is_integer(I) ->
	integer_to_list(I);
to_s(S) -> S.
