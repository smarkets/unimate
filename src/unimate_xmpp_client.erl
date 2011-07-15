%% -*- erlang-indent-level: 2 -*-
-module(unimate_xmpp_client).

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

%% API
-export([start_link/0, send_groupchat/1, send_groupchat/2, is_room/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session,
                jid :: #jid{},
                broadcast_room_jid :: #jid{},
                conference_server :: string(),
                rooms = [] :: [ {binary(), #jid{}} ]
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec send_groupchat(binary()) -> ok.
send_groupchat(Msg) when is_binary(Msg) ->
  gen_server:call(?SERVER, {send_groupchat, Msg}).

-spec send_groupchat(binary(), binary()) -> ok.
send_groupchat(Msg, Room) when is_binary(Msg) andalso is_binary(Room) ->
  gen_server:call(?SERVER, {send_groupchat, Msg, Room}).

-spec is_room(binary()) -> boolean().
is_room(Room) when is_binary(Room) ->
  gen_server:call(?SERVER, {is_room, Room}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, Port} = application:get_env(jabber_port),
  {ok, Server} = application:get_env(jabber_server),
  ServerIp =
    case application:get_env(jabber_server_ip) of
      {ok, ServerIp1} -> ServerIp1;
      undefined       -> Server
    end,
  UseSsl =
    case application:get_env(jabber_server_tls) of
      {ok, true} -> true;
      _          -> false
    end,
  {ok, User} = application:get_env(jabber_user),
  Nick =
    case application:get_env(jabber_nickname) of
      {ok, Nick1} -> Nick1;
      undefined   -> User
    end,
  {ok, Resource} = application:get_env(jabber_resource),
  {ok, Password} = application:get_env(jabber_password),
  {ok, ConferenceServer} = application:get_env(conference_server),
  {ok, BroadcastRoom} = application:get_env(broadcast_room),
  {ok, Rooms} = application:get_env(rooms),
  Session = exmpp_session:start(),
  Jid = exmpp_jid:make(User, Server, Resource),
  exmpp_session:auth_basic_digest(Session, Jid, Password),
  {ok, _StreamId} =
    case UseSsl of
      true ->
        exmpp_session:connect_SSL(Session, ServerIp, Port);
      false ->
        exmpp_session:connect_TCP(Session, ServerIp, Port)
    end,
  exmpp_session:login(Session),
  Status = exmpp_presence:set_status(exmpp_presence:available(), "Ready"),
  exmpp_session:send_packet(Session, Status),
  BroadCastRoomJid = exmpp_jid:make(BroadcastRoom, ConferenceServer),
  State1 = #state{session=Session,
                  jid=Jid,
                  conference_server=ConferenceServer,
                  broadcast_room_jid=BroadCastRoomJid},
  %% Connect to all rooms in our config
  State2 = lists:foldl(
            fun(R, S) ->
                J = exmpp_jid:make(R, ConferenceServer, Nick),
                join_room(J, S)
            end,
            State1,
            Rooms),
  {ok, State2}.


%% Broadcast
handle_call({send_groupchat, Msg}, _From,
            State=#state{jid=FromJid, broadcast_room_jid=ToJid}) ->
  send_groupchat_msg_to_jid(Msg, FromJid, ToJid, State),
  {reply, ok, State};
%% To room
handle_call({send_groupchat, Msg, Room}, _From, State) ->
  ok = send_groupchat_int(Msg, Room, State),
  {reply, ok, State};
handle_call({is_room, Room}, _From, State) ->
  Reply = (get_room(Room, State) =/= not_found),
  {reply, Reply, State}.


handle_cast(_Msg, State) ->
  {stop, unhandled_cast, State}.


handle_info(#received_packet{packet_type=message,
                               type_attr="chat",
                               raw_packet=Raw},
            State=#state{jid=Jid}) ->
  case exmpp_message:get_body(Raw) of
    undefined -> ok;
    Msg ->
      OurJidBin = exmpp_jid:to_binary(Jid),
      case exmpp_xml:get_attribute_as_binary(Raw, <<"from">>, <<"unknown">>) of
        OurJidBin -> ok;
        From ->
          error_logger:info_msg("~s: ~s", [From, Msg])
      end
  end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec send_groupchat_int(binary(), binary(), #state{}) -> ok.
send_groupchat_int(Msg, Room, State=#state{jid=Jid, broadcast_room_jid=BJid}) ->
  ToJid =
    case get_room(Room, State) of
      not_found ->
        BJid;
      RJid ->
        RJid
  end,
  send_groupchat_msg_to_jid(Msg, Jid, ToJid, State).

-spec send_groupchat_msg_to_jid(binary(), #jid{}, #jid{}, #state{}) -> ok.
send_groupchat_msg_to_jid(Msg, FromJid, ToJid, #state{session=Session}) ->
  Packet = exmpp_stanza:set_sender(
             exmpp_stanza:set_recipient(
               exmpp_message:groupchat(Msg),
               ToJid),
             FromJid),
  exmpp_session:send_packet(Session, Packet),
  ok.

-spec join_room(#jid{}, #state{}) -> #state{}.
join_room(RoomJid = #jid{}, State=#state{session=Session, jid=Jid}) ->
  Packet = room_presence(Jid, RoomJid),
  exmpp_session:send_packet(Session, Packet),
  add_room(RoomJid, State).

-spec room_presence(#jid{}, #jid{}) -> #xmlel{}.
room_presence(Jid = #jid{}, RoomJid = #jid{}) ->
  User = exmpp_jid:node(Jid),
  RoomBin = exmpp_jid:to_binary(RoomJid),
  To = #xmlattr{name = <<"to">>, value = <<RoomBin/binary,"/",User/binary>>},
  XMLNSAttr = #xmlattr{name= <<"xmlns">>, value= <<"http://jabber.org/protocol/muc">>},
  XMLNS = #xmlel{name=x, attrs=[XMLNSAttr]},
  #xmlel{name=presence, attrs=[To], children=[XMLNS]}.

-spec add_room(#jid{}, #state{}) -> #state{}.
add_room(Jid, State=#state{rooms=Rooms}) ->
  Name = exmpp_jid:node(Jid),
  Rooms2 = [{Name, Jid}|Rooms],
  State#state{rooms=Rooms2}.

-spec get_room(binary(), #state{}) -> #jid{} | not_found.
get_room(Room, #state{rooms=Rooms}) ->
  proplists:get_value(Room, Rooms, not_found).
