-module(ragno_crawler_server).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([crawl_domain/1]).

%-record(state, {}).

%% Local

-define(SKIPPED_URLS, "data/skipped_urls.txt").
-define(VISITED_URLS, "data/visited_urls.txt").


%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec crawl_domain(string()) -> {ok, list()}.
crawl_domain(Url) ->
    gen_server:cast(?MODULE, {crawl_domain, Url}).

%%find_domains(Url) ->
%%    gen_server:cast(?MODULE, {find_domains, Url}).

%% gen_server.

init([]) ->
	{ok,[]}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({crawl_domain, Url}, State) ->
    case crawler:crawl_domain(Url) of
	{ok, Data} ->
	    logger:debug("DEBUG: crawling ~p\n\n~p", [Url, Data]),
	    crawler:save_url_data(Url, Data),
	    file:write_file(?VISITED_URLS, io_lib:format("~s\n", [Url]), [append])
      ;
	{error, Error} ->
	    %% something went wrong
	    logger:debug("ERROR: crawling ~p\n\n~p", [Url, Error]),
	    file:write_file(?SKIPPED_URLS, io_lib:format("~s\n", [Url]), [append])
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
