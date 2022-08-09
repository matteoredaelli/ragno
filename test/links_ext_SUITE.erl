 -module(links_ext_SUITE).
 -compile(export_all).

 all() ->
     [filter_external_links,
      filter_samedomain_links,
      filter_subdomain_links].

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

filter_external_links(_) ->
    BaseUrl = <<"https://redaelli.org/">>,
    ?EXTERNAL_LINKS = links_ext:filter_external_links(?LINKS, BaseUrl).

filter_samedomain_links(_) ->
    BaseUrl = <<"https://redaelli.org/">>,
    ?SAMEDOMAIN_LINKS = links_ext:filter_samedomain_links(?LINKS, BaseUrl).

filter_subdomain_links(_) ->
    BaseUrl = <<"https://redaelli.org/">>,
    L = links_ext:filter_subdomain_links(?LINKS, BaseUrl),
    L = ?SUBDOMAIN_LINKS,
    BaseUrlWWW = <<"https://www.redaelli.org/">>,
    L = links_ext:filter_subdomain_links(?LINKS, BaseUrlWWW).
    
	    
