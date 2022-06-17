-module(crawler).

-export([crawl_domain/1,
	 load_url_data/1,
	 save_url_data/2,
	 url_filename/1]).

-compile(export_all).

-include_lib("kernel/include/logger.hrl").

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
    file:write_file(Filename, erlang:term_to_binary(Data)).

-spec load_url_data(string() | binary) -> {ok, list()}.
load_url_data(Url) ->
    Filename = crawler:url_filename(Url),
    case file:read_file(Filename) of
	{ok, BinData} ->
	    {ok, erlang:binary_to_term(BinData)}
      ;
	Err ->
	    Err
    end.

re_extract_links(Text) ->
    re:run(Text,
	   "<a href=\"(?P<A>[^\"]+)\"", 
	   [{capture,['A'],list}, global]).

re_extract_title(Text) ->
    re:run(Text,
	   "<title>(?P<A>[^\"]+)</title>", 
	   [{capture,['A'],list}, global]).

is_http_link(Url) ->
    string:prefix(Url, "http") =/= nomatch.

extract_links(Text, BaseUrl) ->
    case re_extract_links(Text) of
	{match, List} ->
	    BinUrls = lists:map(fun(X) -> list_to_binary(X) end, 
				List),
	    Urls = lists:flatten(BinUrls),
	    HttpUrls = lists:filter(fun is_http_link/1, Urls),
	    AbsUrls = absolute_urls(HttpUrls, BaseUrl),
	    lists:filter(fun(X) -> is_binary(X) end, AbsUrls)
      ;
	_ -> []
    end.
     
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
    crawl_domain(list_to_binary(Url));
crawl_domain(Url) when is_binary(Url) ->
    logger:debug("DEBUG: crawling ~p\n", [Url]),
    %%io:format("DEBUG: crawling ~p\n", [Url]),
    case fetch_page_with_manual_redirect(Url) of
	{ok, FinalUrl, {_Resp, Headers, Body}} ->
	    Links = extract_links(Body, FinalUrl),
	    %%io:format("DEBUG: Links ~p\n", [Links]),
	    BaseLinks = base_urls(Links),
	    NormalizedLinks = lists:map(fun uri_string:normalize/1, 
					BaseLinks),
	    %% BinDomains = lists:map(fun(X) -> list_to_binary(X) end, 
	    %% 			   NormalizedLinks),
	    UniqLinks = lists:usort(NormalizedLinks),
	    {ok, [{url, Url}, 
		  {final_url, FinalUrl}, 
		  {headers, Headers}, 
		  {links, Links}, 
		  {domains, UniqLinks}]}
      ;
	{error, Error} ->
	    %% something went wrong
	    io:format("ERROR: crawling ~p\n\n~p", [Url, Error]),
	    {error, Error}
    end.
