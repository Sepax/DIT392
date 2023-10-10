-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it (handleRequest) , then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    not_implemented.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.



handleRequest() % request type
    joinRoom() % if request type is joining room
    leaveRoom() % if request type is leave room
    sendMessage() % if request type is send message
    




