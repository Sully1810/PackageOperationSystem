-module(pkg_loc_h).


-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	% Parsing the JSON file
	Package_data = package_locate(jsx:decode(Data)),
	        
	Response = cowboy_req:reply(Package_data, #{
		<<"content-type">> => <<"text/json">>
	}, Encoded_message, Req0),
	{ok, Response, Opts}.



package_locate(JSON) ->
	% Send parsed JSON to back end
	pkg_loc_server:package_locate(JSON).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

% assert 200 = delivered_h.send({delivered, ...}, _where})

package_locate_test() ->
	JSON = "{\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\"}",
	?_assertEqual("{\"lat\":40.7128,\"long\":-74.0060,\"time\":1634578382}", package_locate(JSON)).

-endif.