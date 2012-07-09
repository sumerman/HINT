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

compile_request(PLT, String) ->
	Req = hint_search_req:new(String),
	File = temp_file(),
	Data = extend_req(Req),
	ok = file:write_file(File, Data),
	{ok, AbstractCode} = epp:parse_file(File, [], []),
	rm_temp_file(File),
	CS0 = dialyzer_codeserver:new(),
	{ok, RecDict} = dialyzer_utils:get_record_and_type_info(AbstractCode),
	Mod = list_to_atom(filename:basename(File, ".erl")),
	{ok, SpecDict, CbDict} = dialyzer_utils:get_spec_info(Mod, AbstractCode, RecDict),
	CS1 = dialyzer_codeserver:store_temp_records(Mod, RecDict, CS0),
	CS2 = dialyzer_codeserver:store_temp_contracts(Mod, SpecDict, CbDict, CS1),
	CS3 = process_remote_types(CS2, PLT), % XXX change to the PLT
	F = hint_search_req:func(Req), 
	A = hint_search_req:arity(Req),
	{ok, {_, C}} = dialyzer_codeserver:lookup_mfa_contract({Mod, F, A}, CS3),
	{ok, {dialyzer_contracts:get_contract_return(C),
			dialyzer_contracts:get_contract_args(C)}}.

test_rank(Req, MFA) ->
	PLT = dialyzer_plt:from_file(dialyzer_plt:get_default_plt()),
	{ok, RTS} = compile_request(PLT, Req),
	{value, FTS} = dialyzer_plt:lookup(PLT, MFA),
	RTSL = ts_to_list(RTS),
	FTSL = ts_to_list(FTS),
	lists:max([
		lists:sum([rank(T1, T2) || 
				{T1, T2} <- lists:zip(RTSLR, FTSL)]) ||
			RTSLR <- rotations(RTSL)]).

ts_to_list({Ret, L}) -> 
	[Ret | L].

rank(T1, T2) ->
	Eql = erl_types:t_is_equal(T1, T2),
	Sub = erl_types:t_is_subtype(T1, T2),
	Sup = erl_types:t_is_subtype(T2, T1),
	if
		Eql  -> 0.5;
		Sub  -> 0.3;
		Sup  -> 0.1;
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
	MergedRecords = dialyzer_utils:merge_records(NewRecords, OldRecords),
	CodeServer2 = dialyzer_codeserver:set_temp_records(MergedRecords, CodeServer1),
	CodeServer3 = dialyzer_utils:process_record_remote_types(CodeServer2),
	dialyzer_contracts:process_contract_remote_types(CodeServer3).

to_s(A) when is_atom(A) ->
	atom_to_binary(A, latin1);
to_s(I) when is_integer(I) ->
	integer_to_list(I);
to_s(S) -> S.
