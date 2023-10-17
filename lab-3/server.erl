-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom).

% Join the given client to the given channel
handle(State, {join, Client, Channel}) ->
    case lists:member(Channel, State) of
        % If the channel does exist just join it
        true ->
            Response = catch channel:join(Channel, Client),
            case Response of
                ok ->
                    {reply, ok, State};
                _ ->
                    {reply, {error, user_already_joined, "User already joined"}, State}
            end;
        % If the channel does not exist, start it and join it
        false ->
            channel:start(Channel),
            channel:join(Channel, Client),
            {reply, ok, [Channel|State]}
    end;

% Leave the given client from the given channel
handle(State, {leave, Client, Channel}) ->
    % If the channel does exist, request to leave it
    case lists:member(Channel, State) of
        true -> 
            Response = catch channel:leave(Channel, Client),
            case Response of
                ok ->
                    {reply, ok, State};
                _ ->
                    {reply, {error, user_not_joined, "User not joined"}, State}
            end;
        false ->
            {reply, {error, "Channel does not exist"}, State}
    end;

% Stop all channels in the server
handle(State, stop_channels) -> 
    lists:foreach(fun (Channel) -> channel:stop(Channel) end, State),
    {reply, ok, []}.





