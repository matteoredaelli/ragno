% Ragno - web crawler
%% Copyright (C) 2022  Matteo Redaelli

%% This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(crawler).

-define(CRAWLER_DEFAULT_OPTIONS, [
				  {extract_domains, true},
				  {remove_headers, ["etag", "keep-alive"]},
				  {save_to_file, true}
				 ]).

-export([crawl_domain/1,
	 crawl_domains/1,
	 load_url_data/1,
	 save_url_data/2,
	 url_filename/1]).

-compile(export_all).

-include_lib("kernel/include/logger.hrl").

-spec remove_headers(list(), list()) -> list().
remove_headers(HeadersToBeRemoved, Headers) ->
    lists:foldl(fun proplists:delete/2, Headers, HeadersToBeRemoved).

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

-spec save_url_data(string() | binary(), list()) -> ok | {error, atom()}.
save_url_data(Url, Data) ->
    Filename = crawler:url_filename(Url),
%%    file:write_file(Filename, erlang:term_to_binary(Data)).
    file:write_file(Filename, io_lib:format("~p.\n", [Data])).

-spec load_url_data(string() | binary) -> {ok, list()}.
load_url_data(Url) ->
    Filename = crawler:url_filename(Url),
    file:read_file(Filename).

re_extract_links(Text) ->
    re:run(Text,
	   "<a href=\"(?P<A>[^\"]+)\"", 
	   [{capture,['A'],list}, global]).

%%re_extract_title(Text) ->
%%    re:run(Text,
%%	   "<title>(?P<A>[^\"]+)</title>", 
%%	   [{capture,['A'],list}, global]).

is_http_link(Url) ->
    string:prefix(Url, "http") =/= nomatch.

extract_links(Text, BaseUrl) ->
    case re_extract_links(Text) of
	{match, List} ->
	    BinUrls = lists:map(fun erlang:list_to_binary/1, 
				List),
	    Urls = lists:flatten(BinUrls),
	    HttpUrls = lists:filter(fun is_http_link/1, Urls),
	    AbsUrls = absolute_urls(HttpUrls, BaseUrl),
	    %%BinUrls = lists:filter(fun(X) -> is_binary(X) end, AbsUrls),
	    lists:usort(AbsUrls)	
      ;
	_ -> []
    end.
%%extract_domains(Links) when is_list(Links) ->  
extract_domains(Links) ->     
    BaseLinks = base_urls(Links),
    NormalizedLinks = lists:map(fun uri_string:normalize/1, 
				BaseLinks),
    lists:usort(NormalizedLinks).
    
absolute_urls(Urls, BaseUrl) ->
    lists:foldl(fun(X, Acc) -> case uri_string:resolve(X, BaseUrl) of
				   {error, _} -> Acc;
				   NewUrl -> [NewUrl|Acc]
			       end
		end,
		[],
		Urls).	 

base_url(Url) ->
    %%io:format("DEBUG: base_urll ~p\n", [Url]),
    UrlMap = uri_string:parse(Url),
    %% removing the fragment
    UrlMapNoQuery= maps:remove(query, UrlMap),
    UrlMapNoFragment = maps:remove(fragment, UrlMapNoQuery),
    %% removing the path
    UrlWithWWW = uri_string:recompose(maps:put(path, "/", UrlMapNoFragment)),
    %% removing www
    re:replace(UrlWithWWW, "//www\.", "//", [global, {return, binary}]).

base_urls(Urls) ->
    lists:map(fun base_url/1, 
	      Urls).
    
fetch_page(Url) ->
    httpc:request(get, 
		  {Url, [{"User-Agent", "ragno.erl/1.0"}]}, 
		  [{ssl, [{verify, verify_none}]},
		   {timeout, timer:seconds(8)},
		   {autoredirect, false}
		  ], 
		  [{body_format, binary}]).

fetch_page_with_manual_redirect(URL) ->
    case fetch_page(URL) of
	{ok, {{HttpVersion, Code, Reason}, Headers, Body}}  when Code >= 200, Code < 299  ->
	    {ok, URL, {{HttpVersion, Code, Reason}, Headers, Body}};
	{ok, {{_, Code, _}, Headers, _}}  when Code < 310 , Code >= 300 ->
	    NewURL=proplists:get_value("location", Headers),
	    fetch_page_with_manual_redirect(NewURL);
	{ok, {{HttpVersion, Code, Reason}, Headers, _}}  when Code >= 400 ->
	    {ok, URL, {{HttpVersion, Code, Reason}, Headers, ""}};
	Error -> Error
    end.


crawl_domain(Url) when is_list(Url) -> 
   crawl_domain(list_to_binary(Url), ?CRAWLER_DEFAULT_OPTIONS);
crawl_domain(Url) when is_binary(Url) ->
   crawl_domain(Url, ?CRAWLER_DEFAULT_OPTIONS).

crawl_domain(Url, Options) when is_list(Url) -> 
   crawl_domain(list_to_binary(Url), Options);
crawl_domain(Url, Options) when is_binary(Url) ->
    logger:debug("DEBUG: crawling ~p\n", [Url]),
    Data = case fetch_page_with_manual_redirect(Url) of
	       {ok, FinalUrl, {_Resp, Headers, Body}} ->
		   FilteredHeaders =  case proplists:lookup(remove_headers, Options) of
					  {remove_headers, HeadersToBeDeleted} ->
					      remove_headers(HeadersToBeDeleted, Headers);
					  _ ->
					      Headers
				      end,
		   Links = extract_links(Body, FinalUrl),
		   UniqDomains = case proplists:lookup(extract_domains, Options) of
				     {extract_domains, true} ->
					 extract_domains(Links);
				     _ ->
					 []
				 end,
		   {ok, [{url, Url}, 
			 {final_url, FinalUrl}, 
			 {headers, FilteredHeaders}, 
			 {links, Links}, 
			 {domains, UniqDomains}]}
	     ;
	       {error, Error} ->
		   %% something went wrong
		   io:format("ERROR: crawling ~p\n\n~p", [Url, Error]),
		   {error, Error}
	   end,
    case proplists:lookup(save_to_file, Options) of
	{save_to_file, true} ->
	    save_url_data(Url, Data);
	_ ->
	    true
    end,
    Data.

crawl_domains(Urls) ->
    lists:foreach(fun(Url) ->  wpool:cast(crawler_pool, 
					  {crawler, crawl_domain , [Url]}) 
		  end, 
		  Urls).

