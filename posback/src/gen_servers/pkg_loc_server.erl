%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(pkg_loc_server).
-behaviour(gen_server).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0, start/3,stop/0, package_locate/1]).

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
    io:format("Starting ~p~n",[?MODULE]),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
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

package_locate(Package_data) ->
    % Tuple requires two parameters: function name and JSON data
    % JSON data is now a map
    io:format("Package data: ~p~n",[Package_data]),
    gen_server:call({global,?MODULE}, {package_locate, Package_data}).
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
    riakc_pb_socket:start_link("143.198.57.177", 8087).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), Riak_pid::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.


%         %% 
handle_call({package_locate,Package_data}, _From, Riak_pid) when is_map_key(<<"pkg_uuid">> , Package_data)->
    io:format("Package data: ~p~n",[Package_data]),
    Pkg_loc_data = db_api_service:get_pkg_location(Package_data, Riak_pid),
    io:format("Package location data: ~p~n",[Pkg_loc_data]),
    {reply,Pkg_loc_data,Riak_pid};

handle_call({package_locate, _}, _From, _Riak_pid) ->
    {reply,{fail,bad_data},_Riak_pid};

handle_call(stop, _From, _Riak_pid) ->
        {stop,normal,
                replace_stopped,
          down}. %% setting the server's internal state to down
% handle_call(Msg, _From, State) ->
%     io:format("Received ~p~n",[Msg]),
%     {reply, Msg, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
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




pkg_loc_server_test_() ->
    {setup,
     fun() -> %this setup fun is run once befor the tests are run. If you want setup and teardown to run for each test, change {setup to {foreach
        % Need to mock the RIAK database and the logger event manager
        meck:new(db_api_service, [non_strict]),
        meck:expect(db_api_service, get_pkg_location, fun(Data, Riak_pid) -> {lat,long,time, is_delivered} end)
     end,
     fun(_) ->%This is the teardown fun. Notice it takes one, ignored in this example, parameter.
     meck:unload(db_api_service)
    
    end,
    [%This is the list of tests to be generated and run.
        % fix these later to appropriate response value
        ?_assertEqual({reply,{lat,long,time, is_delivered},some_state},
                            pkg_loc_server:handle_call({package_locate,#{<<"uuid">> => <<"550e8400-e29b-41d4-a716-446655440000">>}}, from,some_state)),
        ?_assertEqual({reply,{fail,bad_data},some_state},
                            pkg_loc_server:handle_call({package_locate,"blah"},from,some_state))
    ]}.
       
%%
%% Unit tests go here. 
%%
-endif.