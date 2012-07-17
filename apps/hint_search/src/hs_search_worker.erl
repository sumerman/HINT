%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 HINT Spawnfest Team
%%%-------------------------------------------------------------------

-module(hs_search_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, start_link/2, stop/1, q/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-type stage_desc() :: {module(), proplists:proplist(), term()}.
-record(state, { 
    stages = [] :: [stage_desc()], 
    plt    = dialyzer_plt:new() :: dialyzer_plt:plt() 
    }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Stages) ->
  DefPath = dialyzer_plt:get_default_plt(),
  PLT = dialyzer_plt:from_file(DefPath),
  start_link(PLT, Stages).

start_link(PLT, Stages) ->
  gen_server:start_link(?MODULE, [{stages, Stages}, 
                                  {plt, PLT}], []).

stop(Wrk) ->
  gen_server:call(Wrk, stop).

-spec q(pid()|atom(), hint_search_req:req()) -> 
  {ok, [hs_stage:entry()]}.
q(Wrk, Req) ->
  gen_server:call(Wrk, {q, Req}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  RawStages = proplists:get_value(stages, Args, []),
  Stages = uniform_stages(RawStages),
  PLT = proplists:get_value(plt, Args),
  {ok, #state{
      stages = Stages,
      plt = PLT
      }}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call({q, Req}, _From, State) ->
  State1 = prepare_for_req(State, Req),
  R = process(State1),
  {reply, {ok, R}, State1}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

uniform_stages(Stages) ->
  UF = fun
    ({S, O})             -> {S, O, undefined};
    (S) when is_atom(S)  -> {S, [], undefined}; 
    (T) when is_tuple(T) -> T
  end,
  lists:map(UF, Stages).

entry_from_sig({MFA, _Ret, _Args}) ->
  hs_stage:empty_entry_for_mfa(MFA).

process(#state{ plt=PLT } = State) ->
  Modules = sets:to_list(dialyzer_plt:all_modules(PLT)),
  Fun = fun(Mod) ->
      {value, Sigs} = dialyzer_plt:lookup_module(PLT, Mod),
      process_entries(State, Sigs) 
  end,
  lists:reverse(lists:keysort(2, lists:flatmap(Fun, Modules))).

process_entries(State, Entries) ->
  lists:foldl(fun(Entry, Res) ->
        case process_entry_sig(State, Entry) of
          % When Entry is 'ignore', 'skip', etc.
          Ent when is_atom(Ent) ->
            Res;
          Ent when is_tuple(Ent) -> 
            [Ent|Res]
        end
    end, [], Entries).

process_entry_sig(#state{ stages=SL }, Sig) ->
  Entry = entry_from_sig(Sig),
  lists:foldl(fun apply_stage/2, Entry, SL).

-spec apply_stage(stage_desc(), hs_stage:entry()) -> hs_stage:entry().
apply_stage(_Stage, Entry) when is_atom(Entry) -> 
  % When Entry is 'ignore', 'skip', etc.
  Entry;
apply_stage({Mod, _Opt, State}, Entry) ->
  try Mod:apply(State, Entry)
  catch 
    _:Reason -> 
      error_logger:warning_msg(
        "Stage ~p failed to process entry ~p. Reason: ~p",
        [Mod, Entry, Reason]),
      Entry 
  end.

prepare_for_req(#state{ stages=SL, plt=PLT } = State, Req) ->
  NewSL = lists:map(fun(StageDesc) ->
        prepare_stage(StageDesc, PLT, Req)
    end, SL),
  State#state{ stages=NewSL }.

-spec prepare_stage(stage_desc(), dialyzer_plt:plt(), 
                    hint_search_req:req()) -> stage_desc().
prepare_stage({Mod, Opt, _OldState}, PLT, Req) ->
  try 
    {ok, NewState} = Mod:prepare(Opt, PLT, Req),
    {Mod, Opt, NewState}
  catch 
    _:Reason -> 
      error_logger:warning_msg(
        "Stage ~p failed to prepare for request ~p. Reason: ~p",
        [Mod, Req, Reason]),
      {undefined, [], undefined}
  end.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

request() ->
  "foo:bar([A], fun((A)->B)) -> [B]".

stages() ->
  [hs_stage_arity, {hs_stage_types, []}].

setup() ->
  %% TODO don't use default PLT for this test
  {ok, Wrk} = start_link(stages()),
  Wrk.

cleanup(Wrk) ->
  stop(Wrk).

get_first(0, _)  -> [];
get_first(_, []) -> [];
get_first(N, [E | List]) -> 
  [E | get_first(N-1, List)].

do_search(Wrk) ->
  Req = hint_search_req:new(request()),
  {ok, Res} = q(Wrk, Req),
  ?debugFmt("Search result is: ~p~n", [get_first(5, Res)]),
  Res.

is_valid_search_reuslt([]) -> true;
is_valid_search_reuslt([{MFA, Score1, _Ex}, 
                        {_, Score2, _} | Rest]) 
    when (Score1 >= Score2) and (tuple_size(MFA) == 3) -> 
  is_valid_search_reuslt(Rest);
is_valid_search_reuslt([{MFA, _Score, _Ex} | Rest]) 
    when (tuple_size(MFA) == 3) -> 
  is_valid_search_reuslt(Rest);
is_valid_search_reuslt(_) -> false.

smth_found_and_well_formated(Wrk) ->
  [?_assert(is_process_alive(Wrk)),
   ?_assert(is_valid_search_reuslt(do_search(Wrk)))].

well_formatted_result_test_() ->
  {setup,
   fun setup/0, fun cleanup/1,
   fun smth_found_and_well_formated/1}.

-endif.

