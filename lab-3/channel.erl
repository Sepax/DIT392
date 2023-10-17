-module(channel).
-export([start/1, stop/1, join/2, leave/2]).

% ---------------------------- API ---------------------------- %

-record(c_state, {
    name,   % Channel name
    clients % list of clients
}).

init_state(Name) -> 
    #c_state{
        name = Name, 
        clients = []
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
    case lists:member(Client, State#c_state.clients) of
        true ->
            {reply, {error, user_already_joined ,  "Already joined"}, State};
        false ->
            {reply, ok, State#c_state{clients=[Client | State#c_state.clients]}}
    end;

handle(State, {leave, Client}) ->
    case lists:member(Client, State#c_state.clients) of
        true ->
            {reply, ok, lists:delete(Client, State#c_state.clients)};
        false ->
            {reply, {error, user_not_joined , "Client is not joined"}, State}
    end;


handle(State, {message_send, Client, Nick, Msg}) ->
    case lists:member(Client, State#c_state.clients) of
        true ->
            Response = {request, self(), make_ref(), {message_receive, State#c_state.name , Nick, Msg}},
            Receivers = lists:filter(fun (C) -> C /= Client end, State#c_state.clients),
            lists:foreach(
                fun (Receiver) -> Receiver ! Response end, Receivers
            ),
            {reply, ok, State};
        false ->
            {reply, {error, user_not_joined , "Client is not joined"}, State}
    end.


