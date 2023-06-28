% Ragno - web crawler
%% Copyright (C) 2022  Matteo Redaelli

%% This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(social).

-export([find_identities/1]).

-define(SOCIAL_IDENTITIES, [ 
			     {youtube,   <<"^https://www.youtube.com/user/(?P<A>[^\?]+)">>},
			     {twitter,   <<"^https://twitter.com/(?P<A>.+)$">>},
			     {linkedin,  <<"^https://www.linkedin.com/(?P<A>.+)$">>},
			     {instagram, <<"^https://www.instagram.com/(?P<A>[^/]+)/?$">>},
			     {gitlab,    <<"^https://www.gitlab.com/(?P<A>[^/]+)$">>},
			     {github,    <<"^https://www.github.com/(?P<A>[^/]+)$">>},
			     {facebook,  <<"^https://www.facebook.com/(?P<A>[^/]+)$">>}
			   ]).

find_identity({_IdentityType, _Regex}, []) ->
    [];
find_identity({IdentityType, Regex}, [Link|Links]) ->
    ID = case re:run(Link, Regex,  [{capture,['A'],binary}, global]) of
	     {match,[[Identity]]} -> 
		 [[IdentityType, Identity]];
	     nomatch -> []
	 end,
    ID ++ find_identity({IdentityType, Regex}, Links).

find_identities(Links) ->
    Fun = fun({IdentityType,Regex}, Acc) -> 
		  New = find_identity({IdentityType, Regex}, Links),
		  New ++ Acc
	  end,
    Identities = lists:foldl(Fun, 
			     [], 
			     ?SOCIAL_IDENTITIES),
    lists:usort(Identities).

