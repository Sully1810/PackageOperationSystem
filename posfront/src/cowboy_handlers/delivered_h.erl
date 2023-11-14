-module(delivered_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	% Parsing the JSON file
	cb_log_manager:log(jsx:decode(Data)), % send to logger
	call_mark_delivered(jsx:decode(Data)),  
	Response = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "Success", Req0),
	{ok, Response, Opts}.

call_mark_delivered(JSON) ->
	% Send parsed JSON to back end
	rpc:cast('backend@138.68.15.146',delivered_server, mark_delivered, [JSON]).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

% Mocking the backend server
mark_delivered_test_() ->
    {setup,
     fun() -> % This setup fun is ran once before the tests are run. Have to setup and teardown.
        % Call the delivered_server
	 	meck:new(delivered_server, [non_strict]),
		meck:new(cb_log_manager, [non_strict]),
		% Tells it to use the delivered_server to test mark_delivered if we receive a JSON, it will be successful. 
        meck:expect(delivered_server, mark_delivered, fun(JSON) -> "Successful" end),
		meck:expect(cb_log_manager, log, fun(JSON) -> log_success end)
     end,
     fun(_) ->%This is the teardown fun. Notice it takes one parameter.
        meck:unload(delivered_server),
		meck:unload(cb_log_manager)
     end,
	[%This is the list of tests to be generated and run.
		% It is successful if the JSON looks like that tuple below.
		?_assertEqual("Successful", call_mark_delivered("{\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\", \"lat\": 40.7128, \"long\": -74.0060, \"time\": 1634578382}")),
		?_assertEqual(log_success, cb_log_manager:log("{\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\", \"lat\": 40.7128, \"long\": -74.0060, \"time\": 1634578382}"))
	]}.

-endif.