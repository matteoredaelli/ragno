-module(ragno_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([crawl_domains_string/1]).
-export([crawl_domains_string/4]).

-include_lib("crawler.hrl").

start(_Type, _Args) ->
    {ok, Workers} = application:get_env(ragno, pool_workers),
    wpool:start_sup_pool(crawler_pool, [{workers, Workers}]),
    {ok, HttpcOptions} = application:get_env(httpc_options),
    httpc:set_options(HttpcOptions),
    {ok, self()}.

stop(_State) ->
	ok.

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

crawl_domains(Domains, HttpOptions, HttpcRequestOptions, RagnoOptions, Halt) ->
    Pool = crawler_pool,
    [wpool:cast(Pool,
		{crawler, crawl_domain, [Domain, HttpOptions, HttpcRequestOptions, RagnoOptions]})
     || Domain <- Domains],
    case Halt of
	true ->
	    wait_for_and_halt(Pool);
	false ->
	    ok
    end.

crawl_domains_string([DomainsString]) ->
    {ok, HttpOptions} = application:get_env(ragno, http_options),
    {ok, HttpcRequestOptions} = application:get_env(ragno, httpc_request_options),
    {ok, RagnoOptions} = application:get_env(ragno, crawler_default_options),
    crawl_domains_string([DomainsString], HttpOptions, HttpcRequestOptions, RagnoOptions).

crawl_domains_string([DomainsString], HttpOptions, HttpcRequestOptions, RagnoOptions) ->
    Domains = re:split(DomainsString, ","),
    crawl_domains(Domains, HttpOptions, HttpcRequestOptions, RagnoOptions, true).
