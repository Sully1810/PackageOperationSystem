%%%-------------------------------------------------------------------
%% @doc posfront public API
%% @end
%%%-------------------------------------------------------------------

-module(posfront_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
	    {'_', [
		%{"/", cowboy_static, {priv_file, db_access, "static/index.html"}},
	     {"/", default_page_h, []},
		{"/delivered",delivered_h,[]},
		{"/pkg_loc",pkg_loc_h,[]},
		{"/pkg_upd",pkg_upd_h,[]},
        {"/rpt_loc",rpt_loc_h,[]}
	    ]}
	]),

	PrivDir = code:priv_dir(posfront),
	%tls stands for transport layer security
        {ok,_} = cowboy:start_tls(https_listener, [
                  		{port, 443},
				{certfile, PrivDir ++ "/ssl/fullchain.pem"},
				{keyfile, PrivDir ++ "/ssl/privkey.pem"}
              		], #{env => #{dispatch => Dispatch}}),
	posfront_sup:start_link().
	
	% cowboy:start_clear(
    %     my_http_listener,
    %     [{port, 8080}],
    %     #{env => #{dispatch => Dispatch}}
    % ).
   % posfront_sup:start_link().


stop(_State) ->
    ok.

%% internal functions
