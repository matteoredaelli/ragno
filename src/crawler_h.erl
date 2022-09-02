%% @doc POST echo handler.
-module(crawler_h).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = maybe_echo(Method, HasBody, Req0),
	{ok, Req, Opts}.

maybe_echo(<<"POST">>, true, Req0) ->
	{ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
	Echo = proplists:get_value(<<"domains">>, PostVals),
	echo(Echo, Req);
maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing domains parameter.">>, Req);
echo(Echo, Req) ->
    Domains = re:split(Echo, " "),
    crawler:crawl_domains(Domains),
    cowboy_req:reply(200, #{}, Echo, Req).
