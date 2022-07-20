% Ragno - web crawler
%% Copyright (C) 2022  Matteo Redaelli

%% This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(tagger).
-export([find_tag_from_keys/2,
	 find_tags/1,
	 find_tags_from_keys/1,
	 find_tags_from_values/1]).

%%-compile(export_all).

-define(TAGS_FROM_KEY_NAMES, [ 
			  { {cloud, "aruba"}, "x-aruba-"},
			  { {cloud, "aws"}, "x-amz-"},
			  { {cloud, "azure"}, "x-azure"},
			  { {language, "aspnet"}, "x-aspnet"},
			  { {language, "php"}, "php"},
			  { {sw, "apache"}, "apache|httpd"},
			  { {sw, "drupal"}, "x-drupal"},
			  { {sw, "litespeed"}, "x-litespeed"},
			  { {sw, "nginx"}, "x-nginx"},
			  { {sw, "nodejs2"}, "x-node"},
			  { {sw, "varnish"}, "x-varnish-"},
			  { {sw, "wordpress"}, "x-wp-"}
		   ]).

-define(TAGS_FROM_KEYS_VALUES, [
			      "x-powered-by",
			      "server"
			     ]).

find_tag_from_keys({_Tag, Regex}, String) ->
    case re:run(String, Regex) of
	{match, _} ->
	    true;
	nomatch ->
	    false
    end.

find_tags_from_values(Header) ->
    Fun = fun(Key, Acc) -> 
		  case Value = proplists:get_value(Key, Header) of
		      undefined -> Acc;
		      _Value ->
			  [{list_to_atom(Key), Value}|Acc]
		  end
	  end,
    lists:foldl(Fun, 
		[], 
		?TAGS_FROM_KEYS_VALUES).

find_tags_from_keys(Header) ->
    Keys = proplists:get_keys(Header),
    String = string:join(Keys, " "),
    Fun = fun({Tag,Regex}, Acc) -> 
		  case tagger:find_tag_from_keys({Tag, Regex}, String) of 
		      false -> Acc ;
		      true -> [Tag|Acc]
		  end
	  end,
    lists:foldl(Fun, 
		[], 
		?TAGS_FROM_KEY_NAMES).


find_tags(Header) ->
    K = tagger:find_tags_from_keys(Header),
    V = tagger:find_tags_from_values(Header),
    K ++ V.
