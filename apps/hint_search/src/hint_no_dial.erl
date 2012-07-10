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
	{ok, C}.

test_ranks(PLT, String) ->
	Req = hint_search_req:new(String),
	{ok, RTS} = compile_request(PLT, Req),
	Modules = sets:to_list(dialyzer_plt:all_modules(PLT)),
	Fun = fun(Mod) ->
			{value, Sigs} = dialyzer_plt:lookup_module(PLT, Mod),
			[{MFA, test_rank(PLT, RTS, {FTR,FTA}), 
					erl_types:t_to_string(erl_types:t_fun(FTA,FTR))} ||
				{{_,_,A}=MFA, FTR, FTA} <- Sigs,
				A =:= hint_search_req:arity(Req)]
	end,
	lists:reverse(lists:keysort(2,lists:flatmap(Fun,Modules))).

test_rank(PLT, Cont, {Ret,Args}) ->
	lists:max([rank(PLT, Cont, {Ret, ArgsR}) 
			|| ArgsR <- rotations(Args)]).

rank(PLT, Cont, RA) ->
	%Sign = erl_types:t_fun(Args, Ret),
	%Warn = dialyzer_contracts:check_contract(Cont, Sign),
	Warn = get_invalid_contract_warnings_fun(Cont, RA, PLT),
	rank_warning(Warn).

rank_warning(ok) -> 
	1;
rank_warning(contract_diff) ->
	-0.5;
rank_warning(contract_subtype) ->
	-0.2;
rank_warning(contract_supertype) ->
	-0.09;
rank_warning(contract_range) ->
	-0.2;
rank_warning(invalid_contract) ->
	-0.7;
rank_warning(extra_range) ->
	-0.2;
% rank_warning({_Perm, overlapping_contract, _}) ->
% 	0;
% rank_warning({_Perm, spec_missing_fun, _}) ->
% 	0;
rank_warning(_) ->
	-0.9.
	

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


%% Code based (mostly copy-paste) on 'dialyzer_contracts.erl'

-include_lib("dialyzer/src/dialyzer.hrl").

get_invalid_contract_warnings_fun(Contract, {Ret, Args}, Plt) ->
	Rec = dialyzer_plt:get_types(Plt),
	Sig = erl_types:t_fun(Args, Ret),
	case dialyzer_contracts:check_contract(Contract, Sig) of
		{error, invalid_contract} ->
			invalid_contract;
		{error, {extra_range, ExtraRanges, _STRange}} ->
			Warn = case t_from_forms_without_remote(Contract#contract.forms, Rec) of
				{ok, NoRemoteType} ->
					CRet = erl_types:t_fun_range(NoRemoteType),
					erl_types:t_is_subtype(ExtraRanges, CRet);
				unsupported ->
					true
			end,
			case Warn of
				true ->
					extra_range;
				false ->
					ok
			end;
		{error, _Msg} ->
			contract_syntax;
		ok ->
			Domain = dialyzer_contracts:get_contract_args(Contract),
			Range  = dialyzer_contracts:get_contract_return(Contract),
			CSig0 = erl_types:t_fun(Domain, Range),
			CSig  = erl_types:subst_all_vars_to_any(CSig0),
			picky_contract_check(CSig, Sig, Contract, Rec)
	end.

picky_contract_check(CSig0, Sig0, Contract, RecDict) ->
	CSig = erl_types:t_abstract_records(CSig0, RecDict),
	Sig  = erl_types:t_abstract_records(Sig0, RecDict),
	case erl_types:t_is_equal(CSig, Sig) of
		true  -> ok;
		false ->
			case (erl_types:t_is_none(erl_types:t_fun_range(Sig)) andalso
					erl_types:t_is_unit(erl_types:t_fun_range(CSig))) of
				true  -> ok;
				false ->
					extra_contract_warning(Contract, CSig, Sig, RecDict)
			end
	end.

extra_contract_warning(Contract, CSig, Sig, RecDict) ->
	SigString = lists:flatten(dialyzer_utils:format_sig(Sig, RecDict)),
	ContractString0 = lists:flatten(dialyzer_utils:format_sig(CSig, RecDict)),
	%% The only difference is in record fields containing 'undefined' or not.
	IsUndefRecordFieldsRelated = SigString =:= ContractString0,
	{IsRemoteTypesRelated, SubtypeRelation} =
		is_remote_types_related(Contract, CSig, Sig, RecDict),
	case IsUndefRecordFieldsRelated orelse IsRemoteTypesRelated of
		true ->
			ok;
		false ->
			case SubtypeRelation of
				contract_is_subtype ->
					contract_subtype;
				contract_is_supertype ->
					contract_supertype;
				neither ->
					contract_diff
			end
	end.

is_remote_types_related(Contract, CSig, Sig, RecDict) ->
	case erl_types:t_is_subtype(CSig, Sig) of
		true ->
			{false, contract_is_subtype};
		false ->
			case erl_types:t_is_subtype(Sig, CSig) of
				true ->
					case t_from_forms_without_remote(Contract#contract.forms, RecDict) of
						{ok, NoRemoteTypeSig} ->
							case blame_remote(CSig, NoRemoteTypeSig, Sig) of
								true ->
									{true, neither};
								false ->
									{false, contract_is_supertype}
							end;
						unsupported ->
							{false, contract_is_supertype}
					end;
				false ->
					{false, neither}
			end
	end.

t_from_forms_without_remote([{FType, []}], RecDict) ->
	Type0 = erl_types:t_from_form(FType, RecDict),
	Map =
		fun(Type) ->
			case erl_types:t_is_remote(Type) of
				true -> erl_types:t_none();
				false -> Type
			end
	end,
	{ok, erl_types:t_map(Map, Type0)};
t_from_forms_without_remote([{_FType, _Constrs}], _RecDict) ->
	%% 'When' constraints
	unsupported;
t_from_forms_without_remote(_Forms, _RecDict) ->
	%% Lots of forms
	unsupported.

blame_remote(ContractSig, NoRemoteContractSig, Sig) ->
	CArgs  = erl_types:t_fun_args(ContractSig),
	CRange = erl_types:t_fun_range(ContractSig),
	NRArgs = erl_types:t_fun_args(NoRemoteContractSig),
	NRRange = erl_types:t_fun_range(NoRemoteContractSig),
	SArgs = erl_types:t_fun_args(Sig),
	SRange = erl_types:t_fun_range(Sig),
	blame_remote_list([CRange|CArgs], [NRRange|NRArgs], [SRange|SArgs]).

blame_remote_list([], [], []) ->
	true;
blame_remote_list([CArg|CArgs], [NRArg|NRArgs], [SArg|SArgs]) ->
	case erl_types:t_is_equal(CArg, NRArg) of
		true ->
			case not erl_types:t_is_equal(CArg, SArg) of
				true  -> false;
				false -> blame_remote_list(CArgs, NRArgs, SArgs)
			end;
		false ->
			case erl_types:t_is_subtype(SArg, NRArg)
				andalso not erl_types:t_is_subtype(NRArg, SArg) of
				true  -> false;
				false -> blame_remote_list(CArgs, NRArgs, SArgs)
			end
	end.

