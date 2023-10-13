% Ragno - web crawler
%% Copyright (C) 2022  Matteo Redaelli

%% This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(tagger_header).
-export([find_tags/1]).

%%-compile(export_all).

-define(TAGS_FROM_KEY_REGEX, [ 
			       { [cdn, akamai], "x-akamai-"},
			       { [cloud, aruba], "x-aruba-"},
			       { [cloud, aws], "x-amz-"},
			       { [cloud, azure], "x-azure"},
			       { [language, aspnet], "x-aspnet"},
			       { [language, php], "php"},
			       { [sw, apache], "apache|httpd"},
			       { [sw, drupal], "x-drupal"},
			       { [sw, litespeed], "x-litespeed"},
			       { [sw, nginx], "x-nginx"},
			       { [sw, nodejs], "x-node"},
			       { [sw, varnish], "x-varnish-"},
			       { [sw, wordpress], "x-wp-"}
			     ]).

-define(TAGS_FROM_KEY_NAME, [
			     "x-powered-by",
			     "server"
			     ]).

-define(TAGS_FROM_KEY_VALUE_REGEX, [ 
%%                                   { [cdn, cloudflare], "server", re:compile("akamai", [caseless])},
				     { [cdn, cloudflare], "server", "akamai"},
				     { [cdn, cloudfront], "x-cache", "cloudfront"},
				     { [cdn, cloudfront], "via", "cloudfront"},
				     { [cdn, cloudflare], "server", "cloudflare"},
				     { [sw, f5], "set-cookie", "bigip"},
				     { [sw, varnish], "via", "varnish"}
				   ]).
find_tags_from_key_name(Header) ->
    Fun = fun(Key, Acc) -> 
		  case Value = proplists:get_value(Key, Header) of
		      undefined -> Acc;
		      _Value ->
			  [[list_to_atom(Key), list_to_binary(string:to_lower(Value))]|Acc]
		  end
	  end,
    lists:foldl(Fun, 
		[], 
		?TAGS_FROM_KEY_NAME).

find_tags_from_key_regex(Header) ->
    Keys = proplists:get_keys(Header),
    String = string:join(Keys, " "),
    Fun = fun({Tag,Regex}, Acc) -> 
		  case re:run(String, Regex, [caseless]) of
		      {match, _} ->
			  [Tag|Acc];
		      nomatch ->
			  Acc
		  end
	  end,
    lists:foldl(Fun, 
		[], 
		?TAGS_FROM_KEY_REGEX).

find_tags_from_key_value_regex(Header) ->
    Fun = fun({Tag, Key, Regex}, Acc) -> 
		  %% search key / value
		  case Value = proplists:get_value(Key, Header) of
		      undefined -> Acc;
		      Value ->
			  %% search regex
			  case re:run(Value, Regex, [caseless]) of
			      {match, _} ->
				  [Tag|Acc];
			      nomatch ->
				  Acc
			  end
		  end
	  end,
    lists:foldl(Fun, 
		[], 
		?TAGS_FROM_KEY_VALUE_REGEX).

find_tags(Header) ->
    KR = find_tags_from_key_regex(Header),
    KN = find_tags_from_key_name(Header),
    KVR = find_tags_from_key_value_regex(Header),
    lists:usort(KR ++ KN ++ KVR).
