 -module(tagger_SUITE).
 -compile(export_all).

 all() ->
     [find_tag_from_keys,
      find_tags,
      find_tags_from_keys,
      find_tags_from_values].

-define(HEADERS, [{"connection","Keep-Alive"},
		  {"date","Fri, 15 Jul 2022 21:38:19 GMT"},
		  {"accept-ranges","bytes"},
		  {"etag","\"a295912e-295e-5b98e996561e0\""},
		  {"server","Apache"},
		  {"x-powered-by","modphp"},
		  {"content-length","10590"},
		  {"content-type","text/html"},
		  {"last-modified","Sat, 23 Jan 2021 10:16:33 GMT"},
		  {"x-aruba-cache","NA"},
		  {"keep-alive","timeout=15, max=100"}]).

find_tag_from_keys(_) -> 
    String = "content-security-policy cache-control vary referrer-policy x-amz-cf-pop x-frame-options x-cache expires x-amz-cf-id content-length content-type x-xss-protection x-clacks-overhead x-backend-server date connection via strict-transport-security x-content-type-options",
    true = tagger:find_tag_from_keys({{cloud, aws}, "-amz-"}, String).

find_tags_from_keys(_) -> 
    [{cloud, aruba}] = tagger:find_tags_from_keys(?HEADERS).

find_tags_from_values(_) -> 
    [{server,<<"Apache">>},{'x-powered-by',<<"modphp">>}] = tagger:find_tags_from_values(?HEADERS).

find_tags(_) -> 
    [{cloud, aruba}, {server,<<"Apache">>},{'x-powered-by',<<"modphp">>}] = tagger:find_tags(?HEADERS).
