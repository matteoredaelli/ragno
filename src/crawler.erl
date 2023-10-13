% Ragno - web crawler
%% Copyright (C) 2022  Matteo Redaelli

%% This program is free software: you can redistribute it and/or modify it under the  terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(crawler).

-export([crawl_domain/4]).

-include_lib("kernel/include/logger.hrl").


-define(HTTP_DEFAULT_USER_AGENT, <<"ragno.erl/1.0-SNAPSHOT">>).

-define(HTTP_DEFAULT_REQUEST_TIMEOUT, 5).

-spec url_filename(string()) -> string().
url_filename(Url) ->
    UrlMap = uri_string:parse(Url),
    Host = maps:get(host, UrlMap),
    List = re:split(Host, "\\.", [{return, list}]),
    [Dir1, [C2|_Dir2]|_] = lists:reverse(List),
    Dir = io_lib:format("data/~s/~s/", [Dir1, [C2]]),
    ok = filelib:ensure_dir(Dir),
    Filename = re:replace(Url, "/", "", [{return, binary}, global]),
    io_lib:format("~s~s", [Dir, Filename]).

-spec save_url_data(string() | binary(), list(), atom()) -> ok | {error, atom()}.
save_url_data(Url, Data, Type) ->
    Filename = url_filename(Url),
    String = case Type of
		 json ->
		     jsone:encode(Data);
		 erl ->
		     io_lib:format("~p.\n", [Data]);
		 binary ->
		     erlang:term_to_binary(Data)
	     end,
    file:write_file(Filename, String).

-spec remove_headers(list(), list()) -> list().
remove_headers(HeadersToBeRemoved, Headers) ->
    lists:foldl(fun proplists:delete/2, Headers, HeadersToBeRemoved).

-spec convert_headers_to_binary(list()) -> list().
convert_headers_to_binary(List) ->
    lists:map(fun({Key, Val}) -> [list_to_binary(Key), list_to_binary(Val)] end,
	      List).

fetch_page(Url, Method, HttpOptions, HttpcRequestOptions) ->
    httpc:request(Method, 
		  {Url, [{"User-Agent", ?HTTP_DEFAULT_USER_AGENT}]},
		   HttpOptions,
		   %%[{ssl, [{verify, verify_none}]},
		   %%{timeout, timer:seconds(Timeout)},
		   %%{autoredirect, false}
		  %%], 
		  %% TODO
		  %% "head" request is often blocked: use get with the option {stream, {self, once}}
		  HttpcRequestOptions
		  %%[{body_format, binary}]
		 ).

fetch_page_with_manual_redirect(Url, Method, HttpOptions, HttpcRequestOptions) when is_list(Url) -> 
    fetch_page_with_manual_redirect(list_to_binary(Url), Method, HttpOptions, HttpcRequestOptions);
fetch_page_with_manual_redirect(URL, Method, HttpOptions, HttpcRequestOptions) when is_binary(URL) ->
    case fetch_page(URL, Method, HttpOptions, HttpcRequestOptions) of
	{ok, {{HttpVersion, Code, Reason}, Headers, Body}} when Code >= 200, Code < 299  ->
	    {ok, URL, {{HttpVersion, Code, Reason}, Headers, Body}};
	{ok, {{HttpVersion, Code, Reason}, Headers, Body}}  when Code < 310 , Code >= 300 ->
	    NewURL=proplists:get_value("location", Headers),

	    %% the url  in Location can be relative (ex. mozilla.org)
	    NewAbsURL = uri_string:resolve(NewURL, URL),
	    NewAbsURLBin = case is_list(NewAbsURL) of
			       true ->  list_to_binary(NewAbsURL);
			       false ->  NewAbsURL
			   end,
	    %% TODO: follow teh redirect only if the domain doesn't change
	    URLDom = links_ext:extract_domain(URL),
	    NewAbsURLDom = links_ext:extract_domain(NewAbsURL),
	    case domains_ext:is_same_domain(URLDom, NewAbsURLDom) of 
		true ->
		    fetch_page_with_manual_redirect(list_to_binary(NewAbsURL), Method, HttpOptions, HttpcRequestOptions);
		false ->
		    {ok, NewAbsURLBin, {{HttpVersion, Code, Reason}, Headers, Body}}
	    end;
	{ok, {{HttpVersion, Code, Reason}, Headers, _}}  when Code >= 400 ->
	    {ok, URL, {{HttpVersion, Code, Reason}, Headers, ""}};
	Error -> Error
    end.

get_final_url(Url, HttpOptions, HttpcRequestOptions) ->
    %% TODO using get & stream instead of head: how to manage stream?
    %%FinalHttpcRequestOptions = HttpcRequestOptions ++ [{stream, {self, once}}],
    case fetch_page_with_manual_redirect(Url, head, HttpOptions, HttpcRequestOptions) of
	       {ok, FinalUrl, _} ->
		   FinalUrl;
	       {error, Error} ->
		   %% something went wrong
		   logger:error("Skipping get_final_url ~p due to '~p'", [Url, Error]),
		   Url
    end.

analyze_domain(Domain, HttpOptions, HttpcRequestOptions, RagnoOptions, Url, {ok, FinalUrl, {{HttpVersion, Code, Reason}, Headers, Body}}) ->
    UrlMap = uri_string:parse(Url),
    Domain = maps:get(host, UrlMap),
    FinalUrlMap = uri_string:parse(FinalUrl),
    FinalDomain = maps:get(host, FinalUrlMap),
    FilteredHeaders =  case proplists:lookup(remove_headers, RagnoOptions) of
			   {remove_headers, HeadersToBeDeleted} ->
			       remove_headers(HeadersToBeDeleted, Headers);
			   _ ->
			       Headers
		       end,
    RegexData =  case proplists:get_value(extract_regex_data, RagnoOptions, false) of
		     false -> [];
		     RegexList ->
			 links_ext:re_extract_all_regex_data(Body, RegexList)
		 end,
    OrigLinks = links_ext:extract_links(Body, FinalUrl),
    Links = case proplists:get_value(final_links, RagnoOptions, false) of
		true ->
		    lists:map(fun(U) -> get_final_url(U, HttpOptions, HttpcRequestOptions) end, 
			      OrigLinks);
		_ ->
		    OrigLinks
	    end,  
    UniqDomains = case proplists:get_value(extract_external_domains, RagnoOptions, false) orelse 
		      proplists:get_value(extract_subdomains, RagnoOptions, false) of
		      true ->
			  links_ext:extract_domains(Links);
		      _ ->
			  []
		  end,
	       
    Tags = case proplists:get_value(extract_tags, RagnoOptions, false) of
	       true ->
		   tagger_header:find_tags(FilteredHeaders);
	       _ ->
		   []
	   end,
    Social = case proplists:get_value(extract_social, RagnoOptions, false) of
		 true ->
		     social:find_identities(Links);
		 _ ->
		     []
	     end,
    logger:debug("Successfully crawled url ~p", [Url]),
    Data = [{url, Url}, 
	    {http_version, list_to_binary(HttpVersion)},
	    {http_resp_code, Code},
	    {http_reason, list_to_binary(Reason)},
	    {final_url, FinalUrl},
	    {domain, Domain},
	    {final_domain, FinalDomain},
	    {regex_data, RegexData},
	    {headers, convert_headers_to_binary(FilteredHeaders)}, 
	    {links, OrigLinks}, 
	    {final_links, Links}, 
	    {domains, UniqDomains},
	    {system_time, erlang:system_time(microsecond)},
	    {social, Social},
	    {tags, Tags}],
    {ok , Data}.

crawl_domain(Domain, HttpOptions, HttpcRequestOptions, RagnoOptions) when is_list(Domain) -> 
   crawl_domain(list_to_binary(Domain), HttpOptions, HttpcRequestOptions, RagnoOptions);
crawl_domain(Domain, HttpOptions, HttpcRequestOptions, RagnoOptions) when is_binary(Domain) ->
    logger:debug("DEBUG: crawling ~p\n", [Domain]),
    Url = erlang:list_to_binary([<<"https://">>, Domain, <<"/">>]),
    {Ret, Data} = case fetch_page_with_manual_redirect(Url, get, HttpOptions, HttpcRequestOptions) of
		      {ok, FinalUrl, {Resp, Headers, Body}} ->
			  analyze_domain(Domain, HttpOptions, HttpcRequestOptions, RagnoOptions, Url, {ok, FinalUrl, {Resp, Headers, Body}});
		      {error, Error} ->
			  %% something went wrong
			  logger:error("Skipping crawling url ~p due to '~p'", [Url, Error]),
			  {error, [{url, Url},
				   {domain, Domain},
				   {http_resp_code, -1},
				   {error, Error}
				  ]}
		  end,
    case Type = proplists:get_value(save_to_file, RagnoOptions, none) of
	none ->
	    true;
	Type ->
	    save_url_data(Url, Data, Type)
    end,
    Ret.
