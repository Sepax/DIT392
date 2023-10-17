-module(client).
-export([handle/2, initial_state/3]).

% ---------------------------------------------------------------------------- %
%                                 Client State                                 %
% ---------------------------------------------------------------------------- %

% This record defines the structure of the state of a client.
-record(client_state, {
    % atom of the GUI process
    gui,
    % nick/username of the client
    nick,
    % atom of the chat server
    server
}).

% Return an initial state record. This is called from GUI.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_state{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% ---------------------------------------------------------------------------- %
%                                    Helpers                                   %
% ---------------------------------------------------------------------------- %

server_active(State) ->
    lists:member(State#client_state.server, registered()).

% Checks if the channel with the given name is active
channel_active(Channel) ->
    lists:member(list_to_atom(Channel), registered()).

join(Channel, State) ->
    genserver:request(State#client_state.server, {join, self(), Channel}).

% ---------------------------------------------------------------------------- %
%                                Request Handler                               %
% ---------------------------------------------------------------------------- %

% Join channel
handle(State, {join, Channel}) ->
    case server_active(State) of
        true ->
            Response = catch join(Channel, State),
            case Response of
                ok ->
                    {reply, ok, State};
                {error, user_already_joined, _} ->
                    {reply, {error, user_already_joined, "User already joined"}, State};
                _ ->
                    {reply, {error, server_not_reached, "Server unreachable"}, State}
            end;
        false ->
            {reply, {error, server_not_reached, "Server unreachable"}, State}
    end;

% Leave channel
handle(State, {leave, Channel}) ->
    Response = catch channel:leave(Channel, self()),
    {reply, Response, State};
    
% Sending message (from GUI, to channel)
handle(State, {message_send, Channel, Msg}) ->
    case channel_active(Channel) of
        true ->
            Response = genserver:request(list_to_atom(Channel), {message_send, Channel, self(), State#client_state.nick, Msg}),
            {reply, Response, State};
        false ->
            {reply, {error, server_not_reached, "Channel unreachable"}, State}
    end;

% Change nick (no check, local only)
handle(State, {nick, NewNick}) ->
    {reply, ok, State#client_state{nick = NewNick}};

% Get current nick
handle(State, whoami) ->
    {reply, State#client_state.nick, State};

% Incoming message (from channel, to GUI)
handle(State = #client_state{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
    {reply, ok, State};

% Quit client via GUI
handle(State, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, State};

% Catch-all for any unhandled requests
handle(State, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, State}.
