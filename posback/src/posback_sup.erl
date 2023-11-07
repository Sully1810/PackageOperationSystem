%%%-------------------------------------------------------------------
%% @doc posback top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(posback_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  io:format("Starting ~p~n",[?MODULE]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

% generate_spec(Module, Type, Name, Args) ->
%     #{
%         id => Module,
%         start => {Module, start, [Name, local, Args]},
%         restart => permanent,
%         shutdown => 2000,
%         type => Type,
%         modules => [Module]
%     }.

init([]) ->
  io:format("Initializing ~p~n",[?MODULE]),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => delivered_server,
          start => {delivered_server, start, []},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [delivered_server]},
        #{id => pkg_loc_server,
          start => {pkg_loc_server, start, []},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [pkg_loc_server]},
        #{id => pkg_upd_server,
          start => {pkg_upd_server, start, []},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [pkg_upd_server]},
        #{id => rpt_loc_server,
          start => {rpt_loc_server, start, []},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [rpt_loc_server]}
        ],
        io:format("ChildSpecs: ~p~n",[ChildSpecs]),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
