 -module(crawler_SUITE).
 -compile(export_all).

 all() ->
     [remove_existing_header,
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

remove_existing_header(_) -> 
    L = crawler:remove_headers(["etag", "keep-alive"], ?HEADERS),
    L1 = proplists:delete("etag", ?HEADERS),
    L  = proplists:delete("keep-alive", L1).

remove_missing_header(_) -> 
    L1 = crawler:remove_headers(["missing"], ?HEADERS),
    L1 = ?HEADERS.
