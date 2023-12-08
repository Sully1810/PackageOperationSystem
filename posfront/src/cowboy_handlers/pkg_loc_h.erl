-module(pkg_loc_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	% Parsing the JSON file
	cb_log_manager:log(jsx:decode(Data)), % send to logger
	Package_data = call_package_locate(jsx:decode(Data)),
	%io:format("successfully called package_locate in backend~n"),
	%io:format("~p~n", [Package_data]),
	Encoded_data = jsx:encode(lists:flatten(io_lib:format("~p", [Package_data]))),
	%io:format("successfully recieved~n"),
	%io:format("~p~n", [Encoded_data]),
	Response = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Encoded_data, Req0),
	{ok, Response, Opts}.



call_package_locate(JSON) ->
	% Send parsed JSON to back end
	%Print what nodes you know about through io format
	io:format("~p~n", [nodes()]),
	Backend_node = rpc:call('rrobin_serv@165.232.48.38', rrobin_serv, next, []),
	io:format("Backend node: ~p~n", [Backend_node]),
	rpc:call(Backend_node, list_to_atom("pkg_loc_server-" ++ Backend_node), package_locate, [JSON]),
	io:format("~p~n", [nodes()]).

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