%% Ragno - links_ext
%% Copyright (C) 2022  Matteo Redaelli

%% This program is free software: you can redistribute it and/or modify it under the  terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(links_ext).

-export([base_url/1,
	 base_urls/1,
	 extract_domains/1,
	 extract_links/2,
	 filter_external_links/2,
	 filter_samedomain_links/2,
	 filter_subdomain_links/2,
	 is_external_link/2,
	 is_samedomain_link/2,
	 is_subdomain_link/2,
	 is_http_link/1,
	 re_extract_links/1
	]).

-compile(export_all).

-include_lib("kernel/include/logger.hrl").

%%-spec re_extract_links(string() | binary()) -> list().
re_extract_links(Text) ->
    re:run(Text,
	   "<a href=\"(?P<A>[^\"]+)\"", 
	   [{capture,['A'],list}, global]).

-spec is_http_link(string() | binary()) -> boolean().
is_http_link(Url) ->
    string:prefix(Url, "http") =/= nomatch orelse
    string:prefix(Url, "#") =/= nomatch.

-spec extract_links(string() | binary(), string() | binary()) -> list().
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

-spec is_external_link(string() | binary(), string() | binary()) -> boolean().
is_external_link(Url, BaseHost) ->
    UrlMap = uri_string:parse(Url),
    Host = maps:get(host, UrlMap),
    %%removing www
    BaseHostNoWWW = binary:replace(BaseHost, <<"www.">>, <<"">>),
    %% adding . 
    BaseHostWithDotNoWWW = erlang:iolist_to_binary([".", BaseHostNoWWW]),
    Host =/= BaseHost andalso 
	Host =/= BaseHostNoWWW andalso
	nomatch ==  string:find(Host, BaseHostWithDotNoWWW).

-spec is_samedomain_link(string() | binary(), string() | binary()) -> boolean().
is_samedomain_link(Url, BaseUrl) ->
    nomatch =/= string:prefix(Url, BaseUrl).

-spec is_subdomain_link(string() | binary(), string() | binary()) -> boolean().
is_subdomain_link(Url, BaseHost) ->
    UrlMap = uri_string:parse(Url),
    Host = maps:get(host, UrlMap),
    %%removing www
    BaseHostNoWWW = binary:replace(BaseHost, <<"www.">>, <<"">>),
    %% adding . 
    BaseHostWithDotNoWWW = erlang:iolist_to_binary([".", BaseHostNoWWW]),
    Host == BaseHost orelse 
	Host == BaseHostNoWWW orelse
	nomatch =/=  string:find(Host, BaseHostWithDotNoWWW).

-spec filter_external_links(list(), string() | binary()) -> list().
filter_external_links(Urls, BaseUrl) ->
    BaseUrlMap = uri_string:parse(BaseUrl),
    BaseHost = maps:get(host, BaseUrlMap), 
    lists:filter(fun(Url) ->
			 is_external_link(Url, BaseHost)
		 end, 
		 Urls).

-spec filter_samedomain_links(list(), string() | binary()) -> list().
filter_samedomain_links(Urls, BaseUrl) ->	 	  
    lists:filter(fun(Url) ->	  	  
			 is_samedomain_link(Url, BaseUrl)
		 end, 
		 Urls).
-spec filter_subdomain_links(list(), string() | binary()) -> list().
filter_subdomain_links(Urls, BaseUrl) ->	
    BaseUrlMap = uri_string:parse(BaseUrl),
    BaseHost = maps:get(host, BaseUrlMap), 	  
    lists:filter(fun(Url) ->
			 is_subdomain_link(Url, BaseHost)
		 end, 
		 Urls).

%%extract_domains(Links) when is_list(Links) ->  
extract_domains(Links) ->     
    BaseLinks = base_urls(Links),
    NormalizedLinks = lists:map(fun uri_string:normalize/1, 
				BaseLinks),
    lists:usort(NormalizedLinks).
    
absolute_urls(Urls, BaseUrl) ->
    lists:foldl(fun(X, Acc) -> case uri_string:resolve(X, BaseUrl) of
				   {error, _, _} -> Acc;
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