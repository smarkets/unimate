-module(unimate_command).

-export([handle/1]).

-export([help/1, broadcast/1, serverinfo/1]).

-define(SEP, <<"\r\n">>).

-define(COMMANDS, [help, broadcast, quit, serverinfo]).

handle(Socket) ->
  Data = read_all(Socket),
  Commands = binary:split(Data, ?SEP, [global]),
  [exec_command(C) || C <- Commands, C =/= <<>>],
  gen_tcp:close(Socket).

read_all(Socket) ->
  read_all(Socket, <<>>).
read_all(Socket, Acc) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Packet} ->
      read_all(Socket, <<Acc/binary,Packet/binary>>);
    {error, closed} ->
      Acc
  end.

exec_command(<<>>) ->
  ok;
exec_command(Data) ->
  [C0|R] = binary:split(Data, <<" ">>),
  C = existing_command(C0),
  apply(?MODULE, C, R).

%% exec_command(<<>>) ->
%%   ok;
%% exec_command(Data) ->
%%   case binary:split(Data, <<" ">>) of
%%     [C0, R] ->
%%       C = existing_command(C0),
%%       ?MODULE:C(R);
%%     [C0] ->
%%       C = existing_command(C0),
%%       ?MODULE:C()
%%   end.

existing_command(C0) when is_binary(C0) ->
  try
    C = list_to_existing_atom(string:to_lower(binary_to_list(C0))),
    true = lists:member(C, ?COMMANDS),
    C
  catch _:_ ->
      throw({unknown_command, C0})
  end.

help(_Socket) ->
  %% TODO
  todo.

broadcast(Data) ->
  case binary:split(Data, <<" ">>) of
    [Room, Rest] ->
      case re:run(Room, <<"([^ ]+@[^ ]+)">>, [{capture, all_but_first, binary}]) of
        {match, [Jid]} ->
          unimate_xmpp_client:send_groupchat(Rest, Jid);
        _ ->
          unimate_xmpp_client:send_groupchat(Rest)
      end;
    _ ->
      unimate_xmpp_client:send_groupchat(Data)
  end.

serverinfo(_Socket) ->
  %% TODO
  todo.
