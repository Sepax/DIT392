-module(server).
-export([start/1, stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  genserver:start(ServerAtom, [], fun handler/2).

handler(S, {join, Ch, Client}) ->
  case lists:member(Ch, S) of
    %Join already existing channel
    true -> Result = genserver:request(list_to_atom(Ch), {join, Client}),
      case Result of
        joined -> {reply, joined, S};
        failed -> {reply, failed, S}
      end;
    %Start new channel process, with channel as pid, and a list with client pid
    false -> genserver:start(list_to_atom(Ch), [Client], fun channel/2),
      {reply, joined, [Ch | S]}
  end;
handler(S, kill_channels) ->
  lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, S),
  {reply, ok, []}.

channel(Clients, {join, Client}) ->
  case lists:member(Client, Clients) of
    %Already in channel
    true -> {reply, failed, Clients};
    %Not in channel
    false -> {reply, joined, [Client | Clients]}
  end;

channel(Clients, {leave, Client}) ->
  case lists:member(Client, Clients) of
    %In channel
    true -> NewClients = lists:delete(Client, Clients),
      {reply, ok, NewClients};
    %Not in channel
    false -> {reply, never_joined, [Clients]}
  end;

channel(Clients, {message, Channel, Nick, Msg, From}) ->
  case lists:member(From, Clients) of
    %In channel
    true -> spawn(fun() -> lists:foreach(
      fun(Pid) ->
        if
          Pid == From -> skip;
          true -> genserver:request(Pid, {message_receive, Channel, Nick, Msg})
        end
      end,
      Clients) end),
      {reply, ok, Clients};
    %Not in channel
    false -> {reply, failed, Clients}
  end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  genserver:request(ServerAtom, kill_channels),
  genserver:stop(ServerAtom).