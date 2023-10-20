-module(pkg_upd_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	% Parsing the JSON file
	update_location(jsx:decode(Data)),     
	Response = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, success, Req0),
	{ok, Response, Opts}.


update_location(JSON) ->
	% Send parsed JSON to back end
	delivered_server:update_location(JSON).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

% assert 200 = delivered_h.send({delivered, ...}, _where})

update_location_test() ->
	JSON = "{\"pkg_uuid\": \"550e8400-e29b-41d4-a716-446655440000\",\"loc_uuid\": \"550e8400-e29b-41d4-a716-446655440000\", \"time\": 1634578382}",
	?_assertEqual(ok, update_location(JSON)).

-endif.