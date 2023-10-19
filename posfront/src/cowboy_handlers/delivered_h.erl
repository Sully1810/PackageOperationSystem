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
	}, Encoded_message, Req0),
	{ok, Response, Opts}.



mark_delivered(JSON) ->
	% Send parsed JSON to back end
	delivered_server:mark_delivered(JSON).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.