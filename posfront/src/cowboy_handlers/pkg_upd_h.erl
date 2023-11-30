-module(pkg_upd_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	% Parsing the JSON file
	cb_log_manager:log(jsx:decode(Data)), % send to logger
	call_update_location(jsx:decode(Data)),     
	Response = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "Success", Req0),
	{ok, Response, Opts}.


call_update_location(JSON) ->
	%io:format("JSON: ~p~n", [JSON]),
	% Send parsed JSON to back end
	% riak@138.68.15.14
	rpc:cast('157.230.196.202',pkg_upd_server, update_location, [JSON]).
	%io:format("Sent to back end~n", []).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

% assert 200 = delivered_h.send({delivered, ...}, _where})

%update_location_test() ->
	%JSON = "{\"pkg_uuid\": \"550e8400-e29b-41d4-a716-446655440000\",\"loc_uuid\": \"550e8400-e29b-41d4-a716-446655440000\", \"time\": 1634578382}",
	%?_assertEqual(ok, update_location(JSON)).

update_locate_test_() ->
	{setup,
		fun() -> % This setup fun is ran once before the tests are run. Have to setup and teardown.
			% Call the delivered_server
			meck:new(pkg_upd_server, [non_strict]),
			meck:new(cb_log_manager, [non_strict]),
			% Tells it to use the delivered_server to test mark_delivered if we receive a JSON, it will be successful. 
			meck:expect(pkg_upd_server, update_location, fun(JSON) -> "Successful" end),
			meck:expect(cb_log_manager, log, fun(JSON) -> log_success end)
		end,
		fun(_) ->%This is the teardown fun. Notice it takes one parameter.
			meck:unload(pkg_upd_server),
			meck:unload(cb_log_manager)
		end,
		[%This is the list of tests to be generated and run.
			% It is successful if the JSON looks like that tuple below.
			?_assertEqual("Successful", call_update_location("{\"pkg_uuid\": \"550e8400-e29b-41d4-a716-446655440000\",\"loc_uuid\": \"550e8400-e29b-41d4-a716-446655440000\", \"time\": 1634578382}")),
			?_assertEqual(log_success, cb_log_manager:log("{\"pkg_uuid\": \"550e8400-e29b-41d4-a716-446655440000\",\"loc_uuid\": \"550e8400-e29b-41d4-a716-446655440000\", \"time\": 1634578382}"))
		]}.

-endif.