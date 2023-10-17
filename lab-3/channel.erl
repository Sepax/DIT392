-module(channel).
-export([start/1, stop/1, join/2, leave/2]).

% ---------------------------- API ---------------------------- %

-record(cState, {
    name,   % Channel name
    users % list of clients
}).

init_state(Name) -> 
    #cState{
name = Name,
users = []
}.

% Starts a new channel with the given name
start(Name) ->
    genserver:start(list_to_atom(Name), init_state(Name), fun handle/2).

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
    case lists:member(Client, State#cState.users) of
        true ->
            {reply, {error, user_already_joined ,  "Already joined"}, State};
        false ->
            {reply, ok, State#cState{users=[Client | State#cState.users]}}
    end;

handle(State, {leave, Client}) ->
    case lists:member(Client, State#cState.users) of
        true ->
            {reply, ok, lists:delete(Client, State#cState.users)};
        false ->
            {reply, {error, user_not_joined , "Client is not joined"}, State}
    end;


handle(State, {message_send, Client, Nick, Msg}) ->
    case lists:member(Client, State#cState.users) of
        true ->
            Response = {request, self(), make_ref(), {message_receive, State#cState.name , Nick, Msg}},
            lists:foreach(fun (Receiver) -> Receiver ! Response end, State#cState.users),
            {reply, ok, State};
        false ->
            {reply, {error, user_not_joined , "Client is not joined"}, State}
    end.


