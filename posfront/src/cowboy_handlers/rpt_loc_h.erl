-module(rpt_loc_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	% Parsing the JSON file
	cb_log_manager:log(jsx:decode(Data)), % send to logger
	call_mark_location(jsx:decode(Data)),   
	%io:format("successfully called mark_location in backend~n"),

	% Response = cowboy_req:reply(200, #{
	% 	<<"content-type">> => <<"text/json">>
	% }, success, Req0),

	Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
        }, "", Req0),
    {ok, Req, Opts}.



call_mark_location(JSON) ->
	% Send parsed JSON to back end
	
	rpc:cast('backend@138.68.15.146',rpt_loc_server, mark_location, [JSON]).
	%io:format("Sent to back end~n", []).
	% case global:whereis_name(rpt_loc_server) of
    %     undefined ->
    %         {error, global_not_found};
    %     PID ->
    %         Value = PID:mark_location(JSON),
    %         {ok, Value}
    % end.
	% rpt_loc_server:mark_location(JSON).

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
			meck:new(rpt_loc_server, [non_strict]),
			meck:new(cb_log_manager, [non_strict]),
			% Tells it to use the delivered_server to test mark_delivered if we receive a JSON, it will be successful. 
			meck:expect(rpt_loc_server, mark_location, fun(JSON) -> "Successful" end),
			meck:expect(cb_log_manager, log, fun(JSON) -> log_success end)
		end,
		fun(_) ->%This is the teardown fun. Notice it takes one parameter.
			meck:unload(rpt_loc_server),
			meck:unload(cb_log_manager)
		end,
		[%This is the list of tests to be generated and run.
			% It is successful if the JSON looks like that tuple below.
			?_assertEqual("Successful", call_mark_location("{\"loc_uuid\": \"550e8400-e29b-41d4-a716-446655440000\",\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\" \"lat\": 40.7128, \"long\": -74.0060, \"time\": 1634578382}")),
			?_assertEqual(log_success, cb_log_manager:log("{\"loc_uuid\": \"550e8400-e29b-41d4-a716-446655440000\",\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\" \"lat\": 40.7128, \"long\": -74.0060, \"time\": 1634578382}"))
		]}.

-endif.