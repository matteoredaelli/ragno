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
	 filter_external_domains/2,
	 filter_external_links/2,
	 filter_same_domains/2,
	 filter_same_domain_links/2,
	 filter_sub_domains/2,
	 filter_sub_domain_links/2,
	 is_external_domain/2,
	 is_external_link/2,
	 is_same_domain/2,
	 is_same_domain_link/2,
	 is_sub_domain/2,
	 is_sub_domain_link/2,
	 is_http_link/1,
	 re_extract_links/1
	]).

-compile(export_all).

-include_lib("kernel/include/logger.hrl").

-spec binary_join(Separator :: binary(), List :: [binary()]) -> binary().
binary_join(_Separator, []) ->
    <<>>;
binary_join(Separator, [H|T]) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary, Separator/binary, Value/binary>> end, H, T).

re_extract_links(Text) ->
    re:run(Text,
%%	   "<a href=\"(?P<A>[^\"]+)\"", 
%% blog.redaelli.org contains links like   href=https:/xxx
	   "href=\"?(?P<A>[^\">]+)\"?>", 
	   [{capture,['A'],list}, global]).

-spec re_extract_regex_data(string() | binary(), string() | binary()) -> list().
re_extract_regex_data(Text, Regex) ->
    case re:run(Text,
		Regex,
		[{capture,['A'],binary}, global]) of
	{match, [List|_]}
	->
	    List;
	nomatch -> 
	    []
    end.

-spec re_extract_all_regex_data(string() | binary(), list()) -> list().
re_extract_all_regex_data(Text, RegexList) ->
    lists:foldl(fun({Name, Regex}, Acc) -> 
			[{Name, re_extract_regex_data(Text, Regex)}|Acc]
		end,
		[],
		RegexList).

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

-spec is_external_domain(string() | binary(), string() | binary()) -> boolean().
is_external_domain(Domain1, Domain2) ->
    %%removing www
    Domain2NoWWW = binary:replace(Domain2, <<"www.">>, <<"">>),
    %% adding . 
    Domain2WithDotNoWWW = erlang:iolist_to_binary([".", Domain2NoWWW]),
    Domain1 =/= Domain2 andalso 
	Domain1 =/= Domain2NoWWW andalso
	nomatch ==  string:find(Domain1, Domain2WithDotNoWWW).

-spec is_external_link(string() | binary(), string() | binary()) -> boolean().
is_external_link(Url, BaseHost) ->
    UrlMap = uri_string:parse(Url),
    Host = maps:get(host, UrlMap),
    %%removing www
    BaseHostNoWWW = binary:replace(BaseHost, <<"www.">>, <<"">>),
    HostNoWWW = binary:replace(Host, <<"www.">>, <<"">>),
    %% adding . 
    BaseHostWithDotNoWWW = erlang:iolist_to_binary([".", BaseHostNoWWW]),
    Host =/= BaseHost andalso 
	HostNoWWW =/= BaseHostNoWWW andalso
	nomatch ==  string:find(Host, BaseHostWithDotNoWWW).

-spec is_same_domain(string() | binary(), string() | binary()) -> boolean().
is_same_domain(Domain1, Domain2) ->
   Domain1 == Domain2.

-spec is_same_domain_link(string() | binary(), string() | binary()) -> boolean().
is_same_domain_link(Url, BaseUrl) ->
    nomatch =/= string:prefix(Url, BaseUrl).

-spec is_sub_domain(string() | binary(), string() | binary()) -> boolean().
is_sub_domain(Domain1, Domain2) ->
    %%removing www
    Domain2NoWWW = binary:replace(Domain2, <<"www.">>, <<"">>),
    %% adding . 
    Domain2WithDotNoWWW = erlang:iolist_to_binary([".", Domain2NoWWW]),
    Domain1 == Domain2 orelse 
	Domain1 == Domain2NoWWW orelse
	nomatch =/=  string:find(Domain1, Domain2WithDotNoWWW).

-spec is_sub_domain_link(string() | binary(), string() | binary()) -> boolean().
is_sub_domain_link(Url, BaseHost) ->
    UrlMap = uri_string:parse(Url),
    Host = maps:get(host, UrlMap),
    %%removing www
    BaseHostNoWWW = binary:replace(BaseHost, <<"www.">>, <<"">>),
    %% adding . 
    BaseHostWithDotNoWWW = erlang:iolist_to_binary([".", BaseHostNoWWW]),
    Host == BaseHost orelse 
	Host == BaseHostNoWWW orelse
	nomatch =/=  string:find(Host, BaseHostWithDotNoWWW).

-spec filter_external_domains(list(), string() | binary()) -> list().
filter_external_domains(Domains, Domain) ->
    lists:filter(fun(D) ->
			 is_external_domain(D, Domain)
		 end, 
		 Domains).

-spec filter_external_links(list(), string() | binary()) -> list().
filter_external_links(Urls, BaseUrl) ->
    BaseUrlMap = uri_string:parse(BaseUrl),
    BaseHost = maps:get(host, BaseUrlMap), 
    lists:filter(fun(Url) ->
			 is_external_link(Url, BaseHost)
		 end, 
		 Urls).

-spec filter_same_domains(list(), string() | binary()) -> list().
filter_same_domains(Domains, Domain) ->
    lists:filter(fun(D) ->
			 is_same_domain(D, Domain)
		 end, 
		 Domains).

-spec filter_same_domain_links(list(), string() | binary()) -> list().
filter_same_domain_links(Urls, BaseUrl) ->	 	  
    lists:filter(fun(Url) ->	  	  
			 is_same_domain_link(Url, BaseUrl)
		 end, 
		 Urls).

-spec filter_sub_domains(list(), string() | binary()) -> list().
filter_sub_domains(Domains, Domain) ->
    lists:filter(fun(D) ->
			 is_sub_domain(D, Domain)
		 end, 
		 Domains).

-spec filter_sub_domain_links(list(), string() | binary()) -> list().
filter_sub_domain_links(Urls, BaseUrl) ->	
    BaseUrlMap = uri_string:parse(BaseUrl),
    BaseHost = maps:get(host, BaseUrlMap), 	  
    lists:filter(fun(Url) ->
			 is_sub_domain_link(Url, BaseHost)
		 end, 
		 Urls).

extract_domain(Url) ->
    %%io:format("DEBUG: base_urll ~p\n", [Url]),
    UrlMap = uri_string:parse(Url),
    maps:get(host, UrlMap).

%%extract_domains(Links) when is_list(Links) ->  
extract_domains(Links) ->     
    AllDomains = lists:map(fun extract_domain/1, Links),
    lists:usort(AllDomains).
    
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
