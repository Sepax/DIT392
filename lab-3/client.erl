-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_state, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_state{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (State)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(State, {join, Channel}) ->
    genserver:request(State#client_state.server, {join, self(), Channel}),
    {reply, ok, State};

% Leave channel
handle(State, {leave, Channel}) ->
    channel:leave(Channel, self()),
    {reply, ok, State};

% Sending message (from GUI, to channel)
handle(State, {message_send, Channel, Msg}) ->
    Result = genserver:request(list_to_atom(Channel), {message_send, self(), State#client_state.nick, Msg}),
    {reply, Result, State};

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(State, {nick, NewNick}) ->
    {reply, ok, State#client_state{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(State, whoami) ->
    {reply, State#client_state.nick, State} ;

% Incoming message (from channel, to GUI)
handle(State = #client_state{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, State} ;

% Quit client via GUI
handle(State, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, State} ;

% Catch-all for any unhandled requests
handle(State, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, State} .