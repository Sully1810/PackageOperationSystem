-module(rpt_loc_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	% Parsing the JSON file
	mark_location(jsx:decode(Data)),     
	Response = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, success, Req0),
	{ok, Response, Opts}.


mark_location(JSON) ->
	% Send parsed JSON to back end
	delivered_server:mark_location(JSON).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

% assert 200 = delivered_h.send({delivered, ...}, _where})

%mark_location_test() ->
	%JSON = "{\"loc_uuid\": \"550e8400-e29b-41d4-a716-446655440000\",\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\" \"lat\": 40.7128, \"long\": -74.0060, \"time\": 1634578382}",
	%?_assertEqual(ok, mark_location(JSON)).

mark_locate_test_() ->
	{setup,
		fun() -> % This setup fun is ran once before the tests are run. Have to setup and teardown.
		% Call the delivered_server
		meck:new(delivered_server, [non_strict]),
		% Tells it to use the delivered_server to test mark_delivered if we receive a JSON, it will be successful. 
		meck:expect(delivered_server, mark_location, fun(JSON) -> "Successful" end)
		
		end,
		fun(_) ->%This is the teardown fun. Notice it takes one parameter.
		meck:unload(delivered_server)
		end,
		[%This is the list of tests to be generated and run.
		% It is successful if the JSON looks like that tuple below.
		?_assertEqual("Successful", mark_location("{\"loc_uuid\": \"550e8400-e29b-41d4-a716-446655440000\",\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\" \"lat\": 40.7128, \"long\": -74.0060, \"time\": 1634578382}"))
		]}.

-endif.