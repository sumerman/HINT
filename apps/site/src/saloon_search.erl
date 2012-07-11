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
      Data1 = lists:sublist(Data, 10),
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
  D = [to_href(M) || M <- Data],
  {ok, Rendered} = search_view:render([{data, D}]),
  Rendered.

%%
%% Misc. helpers
%%
to_href({MFA, Weight}) ->
  FunctionName = function_name(MFA),
  W = io_lib:format("~.1f",[Weight]),
  lists:append([ "<a href=\"http://erlang.org/doc/man/"
               , function_name_link(MFA), "\">", FunctionName
               , "</a>&nbsp;<small>Weight: ", W, "</small><br />"]).

%% M:F/A
function_name({M, F, A}) ->
  lists:flatten(lists:append([ atom_to_list(M), ":", atom_to_list(F)
                             , "/", integer_to_list(A)])).
%% M.html#F-A
function_name_link({M, F, A}) ->
  lists:flatten(lists:append([atom_to_list(M), ".html#", atom_to_list(F)
                             , "-", integer_to_list(A)])).
