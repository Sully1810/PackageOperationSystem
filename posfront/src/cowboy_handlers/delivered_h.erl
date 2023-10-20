-module(delivered_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	% Parsing the JSON file
	mark_delivered(jsx:decode(Data)),     
	Response = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, success, Req0),
	{ok, Response, Opts}.



mark_delivered(JSON) ->
	% Send parsed JSON to back end
	delivered_server:mark_delivered(JSON).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

% assert 200 = delivered_h.send({delivered, ...}, _where})

mark_delivered_test() ->
	JSON = "{\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\", \"lat\": 40.7128, \"long\": -74.0060, \"time\": 1634578382}",
	?_assertEqual(ok, mark_delivered(JSON)).

% Mocking the backend server


%

-endif.