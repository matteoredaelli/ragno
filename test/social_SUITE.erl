-module(social_SUITE).

-compile(export_all).

all() ->
    [
      find_identities
    ].

-define(LINKS, [
		<<"http://www.fondazionepirelli.org/">>,
		<<"http://www.pirellidesign.com/">>,
		<<"https://hub.pirelli.cn/">>,
		<<"https://pirellihangarbicocca.org/en">>,
		<<"https://www.facebook.com/Pirelli">>,
		<<"https://www.instagram.com/pirelli/">>,
		<<"https://www.linkedin.com/company/pirelli">>,
		<<"https://twitter.com/pirelli">>,
		<<"https://www.youtube.com/user/pirelli">>
	       ]).

find_identities(_) -> 
    [
     [facebook,  <<"Pirelli">>],
     [instagram, <<"pirelli">>],
     [linkedin,  <<"company/pirelli">>],
     [twitter,   <<"pirelli">>],
     [youtube,   <<"user/pirelli">>]
    ] = lists:sort(social:find_identities(?LINKS)).
