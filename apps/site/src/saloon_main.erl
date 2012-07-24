-module(saloon_main).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-include_lib("eunit/include/eunit.hrl").

init({_Any, http}, Req, []) ->
  {ok, Req, 0}.

handle(Req, State) ->
  saloon_init:prepare(Req),
  {ok, Rendered} =
    landing_view:render([ {me, [ {category, <<"">>}
                               , {controller, <<"saloon_main">>}]}
                         , {categories, []}
                         , {scripts, []}
                         , {stylesheets, [[{name, <<"landing">>}]]}
                        ]),
  {ok, Rep} = cowboy_http_req:reply(
                200, [], Rendered, Req
               ),
  {ok, Rep, State+1}.

terminate(_R, _S) ->
  ok.
