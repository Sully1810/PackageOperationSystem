-module(cowboy_serv).

-behaviour(gen_server).

%% unit tests
-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

