%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(rpt_loc_server).
-behaviour(gen_server).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0, start/3,stop/0, mark_location/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
    %io:format("rpt_loc_server started ~p~n", [Pid]).
    %global:register_name(rpt_loc_server, Pid).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.

mark_location(Vehicle_data) ->
    io:format("Vehicle data: ~p~n",[Vehicle_data]),
  
    % Tuple requires two parameters: function name and Package_uuid data
    % Package_uuid data is now a map
    gen_server:cast({global,?MODULE}, {mark_location, Vehicle_data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
  {ok, Pid} =  riakc_pb_socket:start_link("143.198.57.177", 8087),
    io:format("rpt_loc_server started ~p~n", [Pid]),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
handle_call(_Request, _From, State) ->
        {reply,replace_started,State};
handle_call(stop, _From, _State) ->
        {stop,normal,
                replace_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), Riak_pid::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
% handle_cast({mark_location,Vehicle_data}, Riak_pid) when is_map_key(<<"loc_uuid">> , Vehicle_data) ->
%     % erpc:cast('riak@138.68.15.146',rpt_loc_server, mark_location, [test]).
%     io:format("Vehicle data: ~p~n",[Vehicle_data]),
%     db_api_service:store_loc_update(Vehicle_data,  Riak_pid),
%     {noreply, Riak_pid};
% handle_cast({mark_location, _},  Riak_pid) ->
%     {noreply, Riak_pid}.
% 
handle_cast(_Msg, State) ->
    io:format("Vehicle data: ~p~n",[test]),
    %Print the message
    io:format("Received ~p~n",[_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================



-ifdef(EUNIT).  

-include_lib("eunit/include/eunit.hrl").




rpt_loc_server_test_() ->
    {setup,
     fun() -> %this setup fun is run once befor the tests are run. If you want setup and teardown to run for each test, change {setup to {foreach
        % Need to mock the RIAK database and the logger event manager
        meck:new(db_api_service, [non_strict]),
        meck:expect(db_api_service, store_loc_update, fun(Data, Riak_pid) -> stored end)
     end,
     fun(_) ->%This is the teardown fun. Notice it takes one, ignored in this example, parameter.
        meck:unload(db_api_service)
    end,

    [%This is the list of tests to be generated and run.

        % fix these later to appropriate response value
        ?_assertEqual({noreply,some_state},
                            rpt_loc_server:handle_cast({mark_location,#{<<"loc_uuid">> => <<"550e8400-e29b-41d4-a716-446655440000">>, <<"lat">> => 40.7128, <<"long">> => -74.006, <<"time">> => 1634578382}},some_state)),
        ?_assertEqual({noreply,some_state},
                            rpt_loc_server:handle_cast({mark_location,[]},some_state))
    ]}.
%%
%% Unit tests go here. 
%%
-endif.