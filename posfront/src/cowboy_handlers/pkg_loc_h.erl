-module(pkg_loc_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	% Parsing the JSON file
	cb_log_manager:log(jsx:decode(Data)), % send to logger
	Package_data = call_package_locate(jsx:decode(Data)),
	io:format("~p~n", [Package_data]),
	Encoded_data = jsx:encode(Package_data),
	io:format("~p~n", [Encoded_data]),
	Response = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, jsx:encode(Package_data), Req0),
	{ok, Response, Opts}.



call_package_locate(JSON) ->
	% Send parsed JSON to back end
	rpc:call('riak@138.68.15.14',pkg_loc_server, package_locate, [JSON]).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

% assert 200 = delivered_h.send({delivered, ...}, _where})

%package_locate_test() ->
	%JSON = "{\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\"}",
	%?_assertEqual("{\"lat\":40.7128,\"long\":-74.0060,\"time\":1634578382}", package_locate(JSON)).

package_locate_test_() ->
	{setup,
		fun() -> % This setup fun is ran once before the tests are run. Have to setup and teardown.
			% Call the pkg_loc_server
			meck:new(pkg_loc_server, [non_strict]),
			meck:new(cb_log_manager, [non_strict]),
			% Tells it to use the pkg_loc_server to test mark_delivered if we receive a JSON, it will be successful. 
			meck:expect(pkg_loc_server, package_locate, fun(JSON) -> "{\"lat\":40.7128,\"long\":-74.0060,\"time\":1634578382}" end),
			meck:expect(cb_log_manager, log, fun(JSON) -> log_success end)
		end,
		fun(_) ->%This is the teardown fun. Notice it takes one parameter.
			meck:unload(pkg_loc_server),
			meck:unload(cb_log_manager)
		end,
		[%This is the list of tests to be generated and run.
			% It is successful if the JSON looks like that tuple below.
			?_assertEqual("{\"lat\":40.7128,\"long\":-74.0060,\"time\":1634578382}", call_package_locate("{\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\"}")),
			?_assertEqual(log_success, cb_log_manager:log("{\"uuid\": \"550e8400-e29b-41d4-a716-446655440000\"}"))
		]}.

-endif.