-module(db_api_service).


-export([store_delivery/2, get_pkg_location/2, store_pkg_update/2, store_loc_update/2]).


%% API
%% 
%% 
%% We need 3 puts and 1 get


%% store_delivery, the input is Package_data

store_delivery(Package_data, Riak_Pid) ->
    % get the uuid
    Uuid = maps:get(<<"uuid">>, Package_data),
    % Fetch the package data from riak
	{_,Fetched} = riakc_pb_socket:get(Riak_Pid, <<"packages">>, Uuid),
    % Convert the fetched data to a term
	Data = binary_to_term(riakc_obj:get_value(Fetched)),
    % Prepend the last package location/timestamp and delivery status
    Last_timestamp = {maps:get(<<"lat">>, Package_data), maps:get(<<"long">>, Package_data), maps:get(<<"time">>, Package_data)},
    Updated_data = {[Last_timestamp|Data], delivered},

    % Put the updated data back into riak
    Request = riakc_obj:new(<<"packages">>, Uuid, Updated_data),
    % Print the request to the console
    io:format("Request: sent to RIAK ~p~n", [Request]),
	riakc_pb_socket:put(Riak_Pid, Request).

get_pkg_location(Package_data, Riak_Pid) ->
    % get the uuid
    Uuid = maps:get(<<"pkg_uuid">>, Package_data),
    % Fetch the package data from riak
    {_,Fetched} = riakc_pb_socket:get(Riak_Pid, <<"packages">>, Uuid),
    % Convert the fetched data to a term and return it
    Data = binary_to_term(riakc_obj:get_value(Fetched)),
    %Print the data to the console
    io:format("Data: ~p~n", [Data]),
    binary_to_term(riakc_obj:get_value(Data)).

    store_pkg_update(Request_data, Riak_Pid) ->
        Pkg_uuid = maps:get(<<"pkg_uuid">>, Request_data),
        Loc_uuid = maps:get(<<"loc_uuid">>, Request_data),
    
        % Fetch the package & vehicle data from riak
        Package_data = fetch_or_init_data(Riak_Pid, <<"packages">>, Pkg_uuid),
        Location_data = fetch_or_init_data(Riak_Pid, <<"packages">>, Loc_uuid),
    
        % Logging the fetched data
        io:format("Fetched Package Data: ~p~n", [Package_data]),
        io:format("Fetched Location Data: ~p~n", [Location_data]),
    
        % Prepend the last package location/timestamp and delivery status
        Updated_data = [Location_data | Package_data],
    
        % Put the updated data back into riak
        Request = riakc_obj:new(<<"packages">>, Pkg_uuid, Updated_data),
        riakc_pb_socket:put(Riak_Pid, Request).
    
    fetch_or_init_data(Riak_Pid, Bucket, Key) ->
        case riakc_pb_socket:get(Riak_Pid, Bucket, Key) of
            {ok, Obj} -> binary_to_term(riakc_obj:get_value(Obj));
            {error, notfound} -> []; % Initialize with an empty list if not found
            {error, _} -> 
                io:format("Error fetching data for Key: ~p~n", [Key]),
                []
        end.
    
store_loc_update(Vehicle_data, Riak_Pid) ->
    % get the uuid
    Loc_uuid = maps:get(<<"loc_uuid">>, Vehicle_data),
    % Parse the Vehicle data into a tuple and put it into the db
    Updated_data = {maps:get(<<"lat">>, Vehicle_data), maps:get(<<"long">>, Vehicle_data), maps:get(<<"time">>, Vehicle_data)},
    % Put the updated data back into riak
    Request = riakc_obj:new(<<"packages">>, Loc_uuid, Updated_data),
	riakc_pb_socket:put(Riak_Pid, Request).