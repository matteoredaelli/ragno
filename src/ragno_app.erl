-module(ragno_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([crawl_domains_string/1]).
-export([crawl_domains_string/2]).

-include_lib("crawler.hrl").

start(_Type, _Args) ->
    {ok, Workers} = application:get_env(ragno, pool_workers),
    wpool:start_sup_pool(crawler_pool, [{workers, Workers}]),
    %% {ok, HttpcOptions} = application:get_env(httpc_options),
    %%httpc:set_options(HttpcOptions),
    {ok, self()}.

stop(_State) ->
	ok.


ragno_get_options() ->
    case application:get_env(ragno, crawler_default_options) of
	{ok, Options} ->
	    Options;
	_Else ->
	    ?RAGNO_OPTS
    end.

any_running_workers(Pool) ->
    Stats = wpool:stats(Pool),
    Tasks = lists:usort([proplists:get_value(task, WS)
			 || {_, WS} <- proplists:get_value(workers, Stats)]),
    case length(lists:delete(undefined, Tasks)) of
	0 ->
	    false;
	_ ->
	    true
    end.

wait_for_and_halt(Pool) ->
    timer:sleep(5000),
    case any_running_workers(Pool) of
	false ->
	    halt();
	true ->
	    wait_for_and_halt(Pool)
    end.

crawl_domains(Domains, Options, Halt) ->
    Pool = crawler_pool,
    [wpool:cast(Pool,
		{crawler, crawl_domain, [Domain, Options]})
     || Domain <- Domains],
    case Halt of
	true ->
	    wait_for_and_halt(Pool);
	false ->
	    ok
    end.

crawl_domains_string([DomainsString]) ->
    Options = ragno_get_options(),
    crawl_domains_string([DomainsString], Options).

crawl_domains_string([DomainsString], Options) ->
    Domains = re:split(DomainsString, ","),
    crawl_domains(Domains, Options, true).
