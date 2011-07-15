-module(unimate_command).

-export([handle/1]).

-export([help/1, broadcast/2, serverinfo/1]).

-define(SEP, <<"\r\n">>).

-define(COMMANDS, [help, broadcast, quit, serverinfo]).

-spec handle(gen_tcp:socket()) -> binary().
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

-spec pop_commands(gen_tcp:socket(), binary()) -> binary().
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

-spec exec(gen_tcp:socket(), binary()) -> ok.
exec(Sock, Data) ->
  case binary:split(Data, <<" ">>) of
    [C0, R] ->
      C = command(C0),
      ?MODULE:C(Sock, R);
    [C0] ->
      C = command(C0),
      ?MODULE:C(Sock)
  end.

-spec command(binary()) -> atom().
command(C0) when is_binary(C0) ->
  try
    C = list_to_existing_atom(string:to_lower(binary_to_list(C0))),
    true = lists:member(C, ?COMMANDS),
    C
  catch _:_ ->
      throw({unknown_command, C0})
  end.

-spec help(gen_tcp:socket()) -> ok.
help(Sock) ->
  Help = ["",
          "commands:",
          "",
          "    help",
          "    broadcast <chan>? <msg>",
          "    serverinfo",
          ""],
  ok = gen_tcp:send(Sock, string:join(Help, "\n")).

-spec broadcast(gen_tcp:socket(), binary()) -> ok.
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
  end,
  ok.

-spec serverinfo(gen_tcp:socket()) -> ok.
serverinfo(Sock) ->
  I = io_lib:format("~p\n~p\n",
                    [erlang:memory(), application:loaded_applications()]),
  ok = gen_tcp:send(Sock, I).
