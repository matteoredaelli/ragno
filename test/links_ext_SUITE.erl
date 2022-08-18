 -module(links_ext_SUITE).
 -compile(export_all).

 all() ->
     [
      basic_checks,
      extract_domains,
      filter_external_domains,
      filter_external_links,
      filter_same_domain_links,
      filter_sub_domains,
      filter_sub_domain_links].

-define(SAME_DOMAIN, [
		      <<"redaelli.org">>
		     ]).

-define(SUB_DOMAINS, [
		<<"redaelli.org">>,
		<<"domain1.redaelli.org">>,
		<<"domain2.redaelli.org">>,
		<<"www.redaelli.org">>
	       ]).

-define(EXTERNAL_DOMAINS, [
		<<"redaelli.tech">>
	       ]).

-define(DOMAIN, <<"redaelli.org">>).

-define(DOMAINS, 
	?SUB_DOMAINS ++ ?EXTERNAL_DOMAINS).

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
		<<"https://redaelli.tech/a">>
	       ]).

-define(LINKS, 
	%% ?SAMEDOMAIN_LINKS ++ 
	?SUBDOMAIN_LINKS ++ ?EXTERNAL_LINKS).

basic_checks(_) ->
    true  = links_ext:is_same_domain(<<"apache.org">>, <<"apache.org">>),
    true  = links_ext:is_sub_domain(<<"camel.apache.org">>, <<"apache.org">>),
    true  = links_ext:is_sub_domain(<<"camel.apache.org">>, <<"www.apache.org">>),
    false = links_ext:is_sub_domain(<<"redaelli.tech">>, <<"redaelli.org">>),
    true  = links_ext:is_external_domain(<<"redaelli.tech">>, <<"apache.org">>).

extract_domains(_) ->
    Domains = links_ext:extract_domains(?LINKS),
    Domains = lists:usort(?DOMAINS).

filter_external_domains(_) ->
    ?EXTERNAL_DOMAINS = links_ext:filter_external_domains(?DOMAINS, ?DOMAIN).

filter_sub_domains(_) ->
    ?SUB_DOMAINS = links_ext:filter_sub_domains(?DOMAINS, ?DOMAIN).

filter_external_links(_) ->
    BaseUrl = <<"https://redaelli.org/">>,
    ?EXTERNAL_LINKS = links_ext:filter_external_links(?LINKS, BaseUrl).

filter_same_domain_links(_) ->
    BaseUrl = <<"https://redaelli.org/">>,
    ?SAMEDOMAIN_LINKS = links_ext:filter_same_domain_links(?LINKS, BaseUrl).

filter_sub_domain_links(_) ->
    BaseUrl = <<"https://redaelli.org/">>,
    L = links_ext:filter_sub_domain_links(?LINKS, BaseUrl),
    L = ?SUBDOMAIN_LINKS,
    BaseUrlWWW = <<"https://www.redaelli.org/">>,
    L = links_ext:filter_sub_domain_links(?LINKS, BaseUrlWWW).
