-module(unimate_command).

-export([handle/1]).

-export([help/1, broadcast/2, serverinfo/1]).

-define(SEP, <<"\r\n">>).

-define(COMMANDS, [help, broadcast, quit, serverinfo]).

handle(Sock) ->
   handle(Sock, <<>>).

handle(Sock, Buf) ->
   case gen_tcp:recv(Sock, 0) of
      {ok, Packet} ->
         Buf2 = pop_commands(Sock, <<Buf/binary,Packet/binary>>),
         handle(Sock, Buf2);
      {error, closed} ->
         pop_commands(Sock, Buf)
   end.

pop_commands(Sock, Buf) ->
   case binary:split(Buf, ?SEP) of
     [<<>>,<<>>] ->
       <<>>;
     [E] ->
       E;
     [C, R] ->
       exec(Sock, C),
       pop_commands(Sock, R)
   end.

exec(Sock, Data) ->
  case binary:split(Data, <<" ">>) of
    [C0, R] ->
      C = command(C0),
      ?MODULE:C(Sock, R);
    [C0] ->
      C = command(C0),
      ?MODULE:C(Sock)
  end.

command(C0) when is_binary(C0) ->
  try
    C = list_to_existing_atom(string:to_lower(binary_to_list(C0))),
    true = lists:member(C, ?COMMANDS),
    C
  catch _:_ ->
      error_logger:info_msg("Unknown command ~s", [C0])
  end.

help(Sock) ->
  Help = ["",
          "commands:",
          "",
          "    help",
          "    broadcast <chan>? <msg>",
          "    serverinfo",
          ""],
  gen_tcp:send(Sock, string:join(Help, "\n")).

broadcast(_Sock, Data) ->
  case binary:split(Data, <<" ">>) of
    [Room, Rest] ->
      case unimate_xmpp_client:is_room(Room) of
        true ->
          unimate_xmpp_client:send_groupchat(Rest, Room);
        false ->
          unimate_xmpp_client:send_groupchat(Data)
      end;
    _ ->
      unimate_xmpp_client:send_groupchat(Data)
  end.

serverinfo(Sock) ->
  I = io_lib:format("~p\n~p\n",
                    [erlang:memory(), application:loaded_applications()]),
  gen_tcp:send(Sock, I).
