-module(ragno_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, Workers} = application:get_env(ragno, pool_workers),
    wpool:start_sup_pool(crawler_pool, [{workers, Workers}]).
    % {ok, HttpcOptions} = application:get_env(httpc_options),
    %httpc:set_options(HttpcOptions),
   %  {ok, self()}.

stop(_State) ->
	ok.
