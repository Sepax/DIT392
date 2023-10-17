-module(channel).
-export([start/1, stop/1, join/2, leave/2]).

% ---------------------------- API ---------------------------- %

% Starts a new channel with the given name
start(Name) ->
    genserver:start(list_to_atom(Name), [], fun handle/2).

% Stops the channel with the given name
stop(Name) ->
    genserver:stop(list_to_atom(Name)).

% Joins the given client to the channel
join(Name, Client) ->
    genserver:request(list_to_atom(Name), {join, Client}).

% Leaves the given client from the channel
leave(Name, Client) ->
    genserver:request(list_to_atom(Name), {leave, Client}).

% --------------------- Request Handler ---------------------- %

handle(State, {join, Client}) ->
    case lists:member(Client, State) of
        true ->
            {reply, {error, user_already_joined ,  "User already joined"}, State};
        false ->
            {reply, ok, [Client | State]}
    end;

handle(State, {leave, Client}) ->
    case lists:member(Client, State) of
        true ->
            {reply, ok, lists:delete(Client, State)};
        false ->
            {reply, {error, user_not_joined , "User not Joined"}, State}
    end;


handle(State, {message_send, Channel, Client, Nick, Msg}) ->
    case lists:member(Client, State) of
        true ->
            Response = {request, self(), make_ref(), {message_receive, Channel, Nick, Msg}},
            Receivers = lists:filter(fun (C) -> C /= Client end, State),
            lists:foreach(
                fun (Receiver) -> Receiver ! Response end, Receivers
            ),
            {reply, ok, State};
        false ->
            {reply, {error, user_not_joined , "User not Joined"}, State}
    end.


