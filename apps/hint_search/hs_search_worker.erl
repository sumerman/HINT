%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(hs_search_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, q/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-type entry()      :: {mfa(), number(), proplists:proplist()}.
-type stage_desc() :: {module(), proplists:proplist(), term()}.
-record(state, { 
    stages = [] :: [stage_desc()], 
    plt    = dialyzer_plt:new() :: dialyzer_plt:plt() 
    }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

q(Wrk, Req) ->
  gen_server:call(Wrk, {q, Req}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Args}.

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

empy_entry_for_mfa(MFA) ->
  {MFA, 0, []}.

process(#state{ plt=PLT } = State) ->
  Modules = sets:to_list(dialyzer_plt:all_modules(PLT)),
  Fun = fun(Mod) ->
      {value, Sigs} = dialyzer_plt:lookup_module(PLT, Mod),
      process_entries(State, [empy_entry_for_mfa(MFA)  
                              || {MFA, _, _} <- Sigs])
  end,
  lists:reverse(lists:keysort(2, lists:flatmap(Fun, Modules))).

process_entries(State, Entries) ->
  lists:foldl(fun(Entry, Res) ->
        case process_entry(State, Entry) of
          % When Entry is 'ignore', 'skip', etc.
          Ent when is_atom(Ent) ->
            Res;
          Ent when is_tuple(Ent) -> 
            [Ent|Res]
        end
    end, [], Entries).

process_entry(#state{ stages=SL }, Entry) ->
  lists:foldl(fun apply_stage/2, Entry, SL).

-spec apply_stage(stage_desc(), entry()) -> entry().
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

prepare_for_req(#state{ stages=SL, plt=PLT }, Req) ->
  NewSL = lists:map(fun(StageDesc) ->
        prepare_stage(StageDesc, PLT, Req)
    end, SL),
  #state{ stages=NewSL }.

-spec prepare_stage(stage_desc(), dialyzer_plt:plt(), 
                    hint_search_req:req()) -> stage_desc().
prepare_stage({Mod, Opt, _OldState}, PLT, Req) ->
  try 
    NewState = Mod:prepare(Opt, PLT, Req),
    {Mod, Opt, NewState}
  catch 
    _:Reason -> 
      error_logger:warning_msg(
        "Stage ~p failed to prepare for request ~p. Reason: ~p",
        [Mod, Req, Reason]),
      undefined 
  end.

