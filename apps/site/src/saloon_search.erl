-module(saloon_search).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-include_lib("eunit/include/eunit.hrl").

%%
%% API Callbacks
%%

init(_, Req, _) ->
  {ok, Req, 0}.

handle(Req, State) ->
  case cowboy_http_req:method(Req) of
    {'POST', _} ->
      process_post(Req, State);
    {'GET', _} ->
      reply(301, "~[,,_,,]:3", Req, State)
  end.

terminate(_R, _S) ->
  ok.


%%
%% Internal
%%
process_post(Req, State) ->
  Rendered = case cowboy_http_req:body_qs(Req) of
               {[{<<"search">>, QB}], _} -> process_search(QB);
               _                         -> render(empty)
             end,
  reply(200, Rendered, Req, State).

process_search(QB) when is_binary(QB) ->
  process_search(binary_to_list(QB));
process_search(Q)                     ->
  case hint_search:q(Q) of
    {error, _Reason} -> render(error);
    {ok, Data} ->
      Data1 = lists:sublist(Data, 30),
      render(Data1)
  end.

%%
%% Request/response utilities
%%
reply(Status, Data, Req, State) ->
  {ok, Rep} = cowboy_http_req:reply(Status, [], Data, Req),
  {ok, Rep, State}.

render(error) ->
  {ok, Rendered} =
    search_view:render([{data,["Error occured. ",
                                    "Try rephrasing your search<br />"]}]),
  Rendered;
render(empty) ->
  {ok, Rendered} =
    search_view:render([{data,["No data matching your criteria. ",
                                    "Try rephrasing your search<br />"]}]),
  Rendered;
render(Data) ->
  D = [{ W, M, F, A
       , function_type(E)} || {W, {M, F, A}, E} <- Data],
  {ok, Rendered} = search_view:render([{data, D}]),
  Rendered.

%%
%% Misc. helpers
%%
function_type(Extra) ->
  proplists:get_value(type_string, Extra, "").
