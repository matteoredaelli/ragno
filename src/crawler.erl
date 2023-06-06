% Ragno - web crawler
%% Copyright (C) 2022  Matteo Redaelli

%% This program is free software: you can redistribute it and/or modify it under the  terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(crawler).

-export([crawl_domain/1,
	 crawl_domain/2,
	 crawl_domains/1,
	 crawl_domains/2,
	 crawl_domains_string/1,
	 crawl_domains_string/2
	]).

-include_lib("kernel/include/logger.hrl").
-include_lib("crawler.hrl").

-define(HTTP_DEFAULT_USER_AGENT, <<"ragno.erl/1.0-SNAPSHOT">>).

-define(HTTP_DEFAULT_REQUEST_TIMEOUT, 5).

ragno_get_options() ->
    case application:get_env(ragno, crawler_default_options) of
	{ok, Options} ->
	    Options;
	_Else ->
	    ?RAGNO_OPTS
    end.

-spec remove_headers(list(), list()) -> list().
remove_headers(HeadersToBeRemoved, Headers) ->
    lists:foldl(fun proplists:delete/2, Headers, HeadersToBeRemoved).

-spec convert_headers_to_binary(list()) -> list().
convert_headers_to_binary(List) ->
    lists:map(fun({Key, Val}) -> [list_to_binary(Key), list_to_binary(Val)] end,
	      List).

fetch_page(Url, Method, Timeout) ->
    httpc:request(Method, 
		  {Url, [{"User-Agent", ?HTTP_DEFAULT_USER_AGENT}]}, 
		  [{ssl, [{verify, verify_none}]},
		   {timeout, timer:seconds(Timeout)},
		   {autoredirect, false}
		  ], 
		  [{body_format, binary}]).

fetch_page_with_manual_redirect(Url, Method, Timeout) when is_list(Url) -> 
    fetch_page_with_manual_redirect(list_to_binary(Url), Method, Timeout);
fetch_page_with_manual_redirect(URL, Method, Timeout) when is_binary(URL) ->
    case fetch_page(URL, Method, Timeout) of
	{ok, {{HttpVersion, Code, Reason}, Headers, Body}}  when Code >= 200, Code < 299  ->
	    {ok, URL, {{HttpVersion, Code, Reason}, Headers, Body}};
	{ok, {{_, Code, _}, Headers, _}}  when Code < 310 , Code >= 300 ->
	    NewURL=proplists:get_value("location", Headers),
	    %% the url  in Location can be relative (ex. mozilla.org)
	    NewAbsURL = uri_string:resolve(NewURL, URL),
	    fetch_page_with_manual_redirect(list_to_binary(NewAbsURL), Method, Timeout);
	{ok, {{HttpVersion, Code, Reason}, Headers, _}}  when Code >= 400 ->
	    {ok, URL, {{HttpVersion, Code, Reason}, Headers, ""}};
	Error -> Error
    end.

get_final_url(Url, Timeout) ->
    case fetch_page_with_manual_redirect(Url, head, Timeout) of
	       {ok, FinalUrl, _} ->
		   FinalUrl;
	       {error, Error} ->
		   %% something went wrong
		   logger:error("Skipping crawling url ~p due to '~p'", [Url, Error]),
		   Url
    end.

analyze_domain(Domain, Options, Url, {ok, FinalUrl, {_Resp, Headers, Body}}) ->
    UrlMap = uri_string:parse(Url),
    Domain = maps:get(host, UrlMap),
    FinalUrlMap = uri_string:parse(FinalUrl),
    FinalDomain = maps:get(host, FinalUrlMap),
    FilteredHeaders =  case proplists:lookup(remove_headers, Options) of
			   {remove_headers, HeadersToBeDeleted} ->
			       remove_headers(HeadersToBeDeleted, Headers);
			   _ ->
			       Headers
		       end,
    RegexData =  case proplists:get_value(extract_regex_data, Options, false) of
		     false -> [];
		     RegexList ->
			 links_ext:re_extract_all_regex_data(Body, RegexList)
		 end,
    OrigLinks = links_ext:extract_links(Body, FinalUrl),
    Links = case proplists:get_value(final_links, Options, false) of
		true ->
		    Timeout = proplists:get_value(http_request_timeout, Options, ?HTTP_DEFAULT_REQUEST_TIMEOUT),
		    lists:map(fun(U) -> get_final_url(U, Timeout) end, 
			      OrigLinks);
		_ ->
		    OrigLinks
	    end,    
    ExternalLinks =  case proplists:get_value(extract_external_links, Options, false) orelse 
			 proplists:get_value(extract_social, Options, false) of
			 true ->
			     links_ext:filter_external_links(Links, Url);
			 _ ->
			     []
		     end,
    InternalLinks =  case proplists:get_value(extract_samedomain_links, Options, false) of
			 true ->
			     links_ext:filter_same_domain_links(Links, Url);
			 _ ->
			     []
		     end,
    UniqDomains = case proplists:get_value(extract_external_domains, Options, false) orelse 
		      proplists:get_value(extract_subdomains, Options, false) of
		      true ->
			  links_ext:extract_domains(Links);
		      _ ->
			  []
		  end,
    
    ExternalDomains = case proplists:get_value(extract_external_domains, Options, false) of
			  true ->
			      links_ext:filter_external_domains(UniqDomains, Domain);
			  _ ->
			      []
		      end,
    SubDomains = case proplists:get_value(extract_subdomains, Options, false) of
		     true ->
			 links_ext:filter_sub_domains(UniqDomains, Domain);
		     _ ->
			 []
		 end,
    %% TODO: www. domains should be skipped in order to avoid duplicates
    %% or adding a cache for traking visited domaibs
	       
    Tags = case proplists:get_value(extract_tags, Options, false) of
	       true ->
		   tagger:find_tags(FilteredHeaders);
	       _ ->
		   []
	   end,
    Social = case proplists:get_value(extract_social, Options, false) of
		 true ->
		     social:find_identities(ExternalLinks);
		 _ ->
		     []
	     end,
    logger:debug("Successfully crawled url ~p", [Url]),
    {ok, [{url, Url}, 
	  {final_url, FinalUrl},
	  {domain, Domain},
	  {final_domain, FinalDomain},
	  {regex_data, RegexData},
	  {headers, convert_headers_to_binary(FilteredHeaders)}, 
	  {external_links, ExternalLinks}, 
	  {internal_links, InternalLinks}, 
	  {external_domains, ExternalDomains},
	  {system_time, erlang:system_time(microsecond)},
	  {social, Social},
	  {sub_domains, SubDomains},
	  {tags, Tags}]}.

crawl_domain(Domain) when is_list(Domain) -> 
    Options = ragno_get_options(),
    crawl_domain(list_to_binary(Domain), Options);
crawl_domain(Domain) when is_binary(Domain) -> 
    Options = ragno_get_options(),
    crawl_domain(Domain, Options).

crawl_domain(Domain, Options) when is_list(Domain) -> 
   crawl_domain(list_to_binary(Domain), Options);
crawl_domain(Domain, Options) when is_binary(Domain) ->
    logger:debug("DEBUG: crawling ~p\n", [Domain]),
    Url = erlang:list_to_binary([<<"https://">>, Domain, <<"/">>]),
    Timeout = proplists:get_value(http_request_timeout, Options, ?HTTP_DEFAULT_REQUEST_TIMEOUT),
    case fetch_page_with_manual_redirect(Url, get, Timeout) of
	{ok, FinalUrl, {_Resp, Headers, Body}} ->
	    {ok, Data} = analyze_domain(Domain, Options, Url, {ok, FinalUrl, {_Resp, Headers, Body}}),
	    io:fwrite(jsone:encode(Data)),
	    {ok, Data};
	{error, Error} ->
	    %% something went wrong
	    logger:error("Skipping crawling url ~p due to '~p'", [Url, Error]),
	    {error, Domain}
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

wait_for(Pool) ->
    case any_running_workers(Pool) of
	false ->
	    ok;
	true ->
	    timer:sleep(5000),
	    wait_for(Pool)
    end.

crawl_domains(Domains) -> 
    Options = ragno_get_options(),
    crawl_domains(Domains, Options).

crawl_domains(Domains, Options) ->
    Pool = crawler_pool,
    [wpool:cast(Pool,
		{crawler, crawl_domain, [Domain, Options]})
     || Domain <- Domains],
    wait_for(Pool).

crawl_domains_string([DomainsString]) ->
    Options = ragno_get_options(),
    crawl_domains_string(DomainsString, Options).

crawl_domains_string(DomainsString, Options) ->
    Domains = re:split(DomainsString, ","),
    crawl_domains(Domains, Options).
