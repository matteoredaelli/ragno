% Ragno - web crawler
%% Copyright (C) 2022  Matteo Redaelli

%% This program is free software: you can redistribute it and/or modify it under the  terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(crawler).

-export([%%crawl_domain/1,
	 crawl_domain/3,
	 crawl_domains/1,
	 crawl_domains/3,
	 load_url_data/1,
	 save_url_data/3
	
]).

-compile(export_all).

-include_lib("kernel/include/logger.hrl").

-define(RAGNO_VER, <<"0.1.0-SNAPSHOT">>).

-spec remove_headers(list(), list()) -> list().
remove_headers(HeadersToBeRemoved, Headers) ->
    lists:foldl(fun proplists:delete/2, Headers, HeadersToBeRemoved).

-spec save_url_data(string() | binary(), list(), atom()) -> ok | {error, atom()}.
save_url_data(NoExtensionFilename, {Extension, Data}, Type) ->
    Filename = NoExtensionFilename ++ "." ++ Extension,
    ok = filelib:ensure_dir(Filename),
    logger:debug("Appending url data for ~p to file ~p'", [Data, Filename]),
    String = case Type of
		 json ->
		     jsone:encode(Data);
		 erl ->
		     io_lib:format("~p.\n", [Data]);
		 binary ->
		     erlang:term_to_binary(Data)
	     end,
    file:write_file(Filename, String, [append]),
    file:write_file(Filename, "\n", [append]).

-spec load_url_data(string() | binary()) -> {ok, list()}.
load_url_data(Url) ->
    Filename = crawler:url_filename(Url),
    {ok, Data} = file:consult(Filename),
    Data.

convert_headers_to_binary(List) ->
    lists:map(fun({Key, Val}) -> [list_to_binary(Key), list_to_binary(Val)] end,
	      List).
	       
fetch_page(Url) ->
    httpc:request(get, 
		  {Url, [{"User-Agent", "ragno.erl/" ++ ?RAGNO_VER}]}, 
		  [{ssl, [{verify, verify_none}]},
		   {timeout, timer:seconds(8)},
		   {autoredirect, false}
		  ], 
		  [{body_format, binary}]).

fetch_page_with_manual_redirect(Url) when is_list(Url) -> 
    fetch_page_with_manual_redirect(list_to_binary(Url));
fetch_page_with_manual_redirect(URL) when is_binary(URL) ->
    case fetch_page(URL) of
	{ok, {{HttpVersion, Code, Reason}, Headers, Body}}  when Code >= 200, Code < 299  ->
	    {ok, URL, {{HttpVersion, Code, Reason}, Headers, Body}};
	{ok, {{_, Code, _}, Headers, _}}  when Code < 310 , Code >= 300 ->
	    NewURL=proplists:get_value("location", Headers),
	    %% the url  in Location can be relative (ex. mozilla.org)
	    NewAbsURL = uri_string:resolve(NewURL, URL),
	    fetch_page_with_manual_redirect(list_to_binary(NewAbsURL));
	{ok, {{HttpVersion, Code, Reason}, Headers, _}}  when Code >= 400 ->
	    {ok, URL, {{HttpVersion, Code, Reason}, Headers, ""}};
	Error -> Error
    end.

get_final_url(Url) ->
    case fetch_page_with_manual_redirect(Url) of
	       {ok, FinalUrl, _} ->
		   FinalUrl;
	       {error, Error} ->
		   %% something went wrong
		   logger:error("Skipping crawling url ~p due to '~p'", [Url, Error]),
		   Url
    end.

crawl_domain(Domain, Options, Filename) when is_list(Domain) -> 
   crawl_domain(list_to_binary(Domain), Options, Filename);
crawl_domain(Domain, Options, Filename) when is_binary(Domain) ->
    logger:debug("DEBUG: crawling ~p\n", [Domain]),
    Url = erlang:list_to_binary([<<"https://">>, Domain, <<"/">>]),
    Data = case fetch_page_with_manual_redirect(Url) of
	       {ok, FinalUrl, {_Resp, Headers, Body}} ->
		   analyze_domain(Domain, Options, Filename, Url, {ok, FinalUrl, {_Resp, Headers, Body}});
	       {error, Error} ->
		   %% something went wrong
		   logger:error("Skipping crawling url ~p due to '~p'", [Url, Error]),
		   {error, Domain}
	   end,
    case Type = proplists:get_value(save_to_file, Options, none) of
	none ->
	    true;
	Type ->
	    save_url_data(Filename, Data, Type)
    end,
    Data.

analyze_domain(Domain, Options, Filename, Url, {ok, FinalUrl, {_Resp, Headers, Body}}) ->
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
		    lists:map(fun get_final_url/1, 
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

    case proplists:get_value(crawl_subdomains, Options, false) of
	true ->
	    crawl_domains(SubDomains, Options, Filename);
	_ ->
	    false
    end,	       
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
	  {ragno_ver, ?RAGNO_VER},
	  {system_time, erlang:system_time(microsecond)},
	  {social, Social},
	  {sub_domains, SubDomains},
	  {tags, Tags}]}.

crawl_domains(Domains) ->
    {ok, DataDir} = application:get_env(ragno, crawler_data_directory),
    Id = erlang:system_time(microsecond),
    Filename = filename:join([DataDir, integer_to_list(Id)]),
    logger:debug("DEBUG: crawling ~p and (maybe) saving to ~w\n", [Domains, Filename]),
    {ok, Options} = application:get_env(ragno, crawler_default_options),
    crawl_domains(Domains, Options, Filename).

crawl_domains(Domains, Options, Filename) ->
    lists:foreach(fun(Domain) -> wpool:cast(crawler_pool, 
					    {crawler, crawl_domain, [Domain, Options, Filename]}) 
		  end, 
		  Domains).

