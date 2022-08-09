 -module(crawler_SUITE).
 -compile(export_all).

 all() ->
     [filter_external_links,
      filter_samedomain_links,
      filter_subdomain_links,
      remove_existing_header,
      remove_missing_header].

-define(HEADERS, [{"connection","Keep-Alive"},
               {"date","Fri, 15 Jul 2022 21:38:19 GMT"},
               {"accept-ranges","bytes"},
               {"etag","\"a295912e-295e-5b98e996561e0\""},
               {"server","Apache"},
               {"content-length","10590"},
               {"content-type","text/html"},
               {"last-modified","Sat, 23 Jan 2021 10:16:33 GMT"},
               {"x-aruba-cache","NA"},
               {"keep-alive","timeout=15, max=100"}]).

-define(SAMEDOMAIN_LINKS, [
		<<"https://redaelli.org/b">>,
		<<"https://redaelli.org/a.txt">>,
		<<"https://redaelli.org/a/b.txt">>
	       ]).

-define(SUBDOMAIN_LINKS, [
		<<"https://redaelli.org/b">>,
		<<"https://redaelli.org/a.txt">>,
		<<"https://redaelli.org/a/b.txt">>,
		<<"https://domain1.redaelli.org/a">>,
		<<"https://domain2.redaelli.org/a">>,
		<<"https://www.redaelli.org/a">>
	       ]).

-define(EXTERNAL_LINKS, [
		<<"https://matteo.org/a">>
	       ]).

-define(LINKS, 
	%% ?SAMEDOMAIN_LINKS ++ 
	?SUBDOMAIN_LINKS ++ ?EXTERNAL_LINKS).

remove_existing_header(_) -> 
    L = crawler:remove_headers(["etag", "keep-alive"], ?HEADERS),
    L1 = proplists:delete("etag", ?HEADERS),
    L  = proplists:delete("keep-alive", L1).

remove_missing_header(_) -> 
    L1 = crawler:remove_headers(["missing"], ?HEADERS),
    L1 = ?HEADERS.

filter_external_links(_) ->
    BaseUrl = <<"https://redaelli.org/">>,
    ?EXTERNAL_LINKS = crawler:filter_external_links(?LINKS, BaseUrl).

filter_samedomain_links(_) ->
    BaseUrl = <<"https://redaelli.org/">>,
    ?SAMEDOMAIN_LINKS = crawler:filter_samedomain_links(?LINKS, BaseUrl).

filter_subdomain_links(_) ->
    BaseUrl = <<"https://redaelli.org/">>,
    L = crawler:filter_subdomain_links(?LINKS, BaseUrl),
    L = ?SUBDOMAIN_LINKS,
    BaseUrlWWW = <<"https://www.redaelli.org/">>,
    L = crawler:filter_subdomain_links(?LINKS, BaseUrlWWW).
    
	    
