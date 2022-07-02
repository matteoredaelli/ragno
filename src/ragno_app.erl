-module(ragno_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) -> 
    {ok, Workers} = application:get_env(pool_workers),
    wpool:start_sup_pool(crawler_pool, [{workers, Workers}]).

stop(_State) ->
	ok.
