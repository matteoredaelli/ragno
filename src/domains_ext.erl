%% Ragno - links_ext
%% Copyright (C) 2022  Matteo Redaelli

%% This program is free software: you can redistribute it and/or modify it under the  terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-module(domains_ext).

-export([filter_external_domains/2,
	 filter_same_domains/2,
	 filter_sub_domains/2,
	 is_external_domain/2,
	 is_same_domain/2,
	 is_sub_domain/2
	]).

-include_lib("kernel/include/logger.hrl").


-spec is_external_domain(string() | binary(), string() | binary()) -> boolean().
is_external_domain(Domain1, Domain2) ->
    %%removing www
    Domain2NoWWW = binary:replace(Domain2, <<"www.">>, <<"">>),
    %% adding . 
    Domain2WithDotNoWWW = erlang:iolist_to_binary([".", Domain2NoWWW]),
    Domain1 =/= Domain2 andalso 
	Domain1 =/= Domain2NoWWW andalso
	nomatch ==  string:find(Domain1, Domain2WithDotNoWWW).


-spec is_same_domain(string() | binary(), string() | binary()) -> boolean().
is_same_domain(Domain1, Domain2) ->
   Domain1 == Domain2.

-spec is_sub_domain(string() | binary(), string() | binary()) -> boolean().
is_sub_domain(Domain1, Domain2) ->
    %%removing www
    Domain2NoWWW = binary:replace(Domain2, <<"www.">>, <<"">>),
    %% adding . 
    Domain2WithDotNoWWW = erlang:iolist_to_binary([".", Domain2NoWWW]),
    Domain1 == Domain2 orelse 
	Domain1 == Domain2NoWWW orelse
	nomatch =/=  string:find(Domain1, Domain2WithDotNoWWW).


-spec filter_external_domains(list(), string() | binary()) -> list().
filter_external_domains(Domains, Domain) ->
    lists:filter(fun(D) ->
			 is_external_domain(D, Domain)
		 end, 
		 Domains).

-spec filter_same_domains(list(), string() | binary()) -> list().
filter_same_domains(Domains, Domain) ->
    lists:filter(fun(D) ->
			 is_same_domain(D, Domain)
		 end, 
		 Domains).


-spec filter_sub_domains(list(), string() | binary()) -> list().
filter_sub_domains(Domains, Domain) ->
    lists:filter(fun(D) ->
			 is_sub_domain(D, Domain)
		 end, 
		 Domains).
