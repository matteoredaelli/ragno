 -module(tagger_SUITE).
 -compile(export_all).

 all() ->
     [
      find_tags,
      find_tags_from_key_name,
      find_tags_from_key_regex,
      find_tags_from_key_value_regex
     ].

-define(HEADERS, [{"connection","Keep-Alive"},
		  {"date","Fri, 15 Jul 2022 21:38:19 GMT"},
		  {"accept-ranges","bytes"},
		  {"etag","\"a295912e-295e-5b98e996561e0\""},
		  {"server","Apache"},
		  {"via","1.1 varnish, 1.1 varnish"},
		  {"x-powered-by","modphp"},
		  {"content-length","10590"},
		  {"content-type","text/html"},
		  {"last-modified","Sat, 23 Jan 2021 10:16:33 GMT"},
		  {"x-aruba-cache","NA"},
		  {"keep-alive","timeout=15, max=100"}]).

find_tags_from_key_regex(_) -> 
    [{cloud, aruba}] = tagger:find_tags_from_key_regex(?HEADERS).

find_tags_from_key_name(_) -> 
    [{server,<<"Apache">>},{'x-powered-by',<<"modphp">>}] = tagger:find_tags_from_key_name(?HEADERS).

find_tags_from_key_value_regex(_) -> 
    [{sw, varnish}] = tagger:find_tags_from_key_value_regex(?HEADERS).

find_tags(_) -> 
    [{cloud, aruba}, 
     {server,<<"Apache">>},
     {'x-powered-by',<<"modphp">>},
     {sw, varnish}
    ] = tagger:find_tags(?HEADERS).
