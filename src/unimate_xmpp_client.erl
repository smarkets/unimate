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
      {ok, Nick1} -> list_to_binary(Nick1);
      undefined   -> list_to_binary(User)
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
                J = exmpp_jid:make(R, ConferenceServer),
                join_room(J, S, Nick)
            end,
            State1,
            Rooms),
  %% Send an avatar to the server if one exists in our priv dir
  Filename = filename:join([code:priv_dir(unimate), "avatar.png"]),
  send_avatar(Filename, State2),
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
handle_info(Packet = #received_packet{packet_type=presence}, State) ->
  ok = handle_presence(Packet, State),
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

-spec join_room(#jid{}, #state{}, binary()) -> #state{}.
join_room(RoomJid = #jid{}, State=#state{session=Session}, Nick) ->
  Packet = room_presence(RoomJid, Nick),
  exmpp_session:send_packet(Session, Packet),
  add_room(RoomJid, State).

-spec room_presence(#jid{}, binary()) -> #xmlel{}.
room_presence(RoomJid = #jid{}, Nick) ->
  RoomBin = exmpp_jid:to_binary(RoomJid),
  To = #xmlattr{name = <<"to">>, value = <<RoomBin/binary,"/",Nick/binary>>},
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

-spec handle_presence(#received_packet{}, #state{}) -> ok.
handle_presence(#received_packet{from=From, type_attr="subscribe"},
                #state{jid=Jid, session=Session}) ->
  case exmpp_jid:make(From) of
    Jid ->
      ok;
    FromJid ->
      Subscribed = exmpp_stanza:set_recipient(exmpp_presence:subscribed(), FromJid),
      exmpp_session:send_packet(Session, Subscribed),
      Subscribe = exmpp_stanza:set_recipient(exmpp_presence:subscribe(), FromJid),
      exmpp_session:send_packet(Session, Subscribe),
      ok
  end;
handle_presence(_, _) ->
  ok.

-spec send_avatar(string(), #state{}) -> ok.
send_avatar(Filename, State=#state{session=Session}) ->
  case file:read_file_info(Filename) of
    {ok, _} ->
      {ok, Bin} = file:read_file(Filename),
      Base64 = base64:encode(Bin),
      Id = hexstring(crypto:sha(Bin)),
      Bytes = byte_size(Bin),
      AvatarPacket = avatar_packet(Id, Base64, State),
      AvatarMetadataPacket = avatar_metadata_packet(Id, Bytes, State),
      VCardPacket = vcard_packet(Base64, State),
      VCardPresence = vcard_presence(Id, State),
      exmpp_session:send_packet(Session, AvatarPacket),
      exmpp_session:send_packet(Session, AvatarMetadataPacket),
      exmpp_session:send_packet(Session, VCardPacket),
      exmpp_session:send_packet(Session, VCardPresence),
      ok;
    {error, Reason} ->
      error_logger:info_msg("Failed to read avatar: ~p - ~p", [Filename, Reason]),
      ok
  end.

-spec avatar_packet(binary(), binary(), #state{}) -> #xmlel{}.
avatar_packet(Id, Base64, State) ->
  Data0 = exmpp_xml:element('urn:xmpp:avatar:data', data, [], []),
  Data = exmpp_xml:set_cdata(Data0, Base64),
  ItemAttr = exmpp_xml:attribute(<<"id">>, Id),
  Item = exmpp_xml:element(undefined, item, [ItemAttr], [Data]),
  publish_element(Item, State).

-spec avatar_metadata_packet(binary(), binary(), #state{}) -> #xmlel{}.
avatar_metadata_packet(Id, Bytes, State) ->
  BytesAttr = exmpp_xml:attribute(<<"bytes">>, integer_to_list(Bytes)),
  TypeAttr = exmpp_xml:attribute(<<"type">>, <<"image/png">>),
  Info = exmpp_xml:element(undefined, info, [BytesAttr, TypeAttr], []),
  Metadata = exmpp_xml:element('urn:xmpp:avatar:metadata', metadata, [], [Info]), 
  ItemAttr = exmpp_xml:attribute(<<"id">>, Id),
  Item = exmpp_xml:element(undefined, item, [ItemAttr], [Metadata]),
  publish_element(Item, State).

-spec publish_element(#xmlel{}, #state{}) -> #xmlel{}.
publish_element(Item, #state{jid=Jid}) ->
  NodeAttr = exmpp_xml:attribute(<<"node">>, 'urn:xmpp:avatar:data'),
  Publish = exmpp_xml:element(undefined, publish, [NodeAttr], [Item]),
  PubSub = exmpp_xml:element('http://jabber.org/protocol/pubsub', pubsub, [], [Publish]),
  FromAttr = exmpp_xml:attribute(<<"from">>, exmpp_jid:to_binary(Jid)),
  Iq0 = exmpp_iq:set(undefined, PubSub, 'publish2'),
  exmpp_xml:set_attribute(Iq0, FromAttr).

-spec vcard_packet(binary(), #state{}) -> #xmlel{}.
vcard_packet(Base64, #state{jid=Jid}) ->
  Binval0 = exmpp_xml:element(undefined, 'BINVAL', [], []),
  Binval = exmpp_xml:set_cdata(Binval0, Base64),
  Type0 = exmpp_xml:element(undefined, 'TYPE', [], []),
  Type = exmpp_xml:set_cdata(Type0, <<"image/png">>),
  Photo = exmpp_xml:element(undefined, 'PHOTO', [], [Type, Binval]),
  VCard = exmpp_xml:element('vcard-temp', 'vCard', [], [Photo]),
  FromAttr = exmpp_xml:attribute(<<"from">>, exmpp_jid:to_binary(Jid)),
  Iq0 = exmpp_iq:set(undefined, VCard, 'vc1'),
  exmpp_xml:set_attribute(Iq0, FromAttr).

-spec vcard_presence(binary(), #state{}) -> #xmlel{}.
vcard_presence(Id, #state{jid=Jid}) ->
  Photo0 = exmpp_xml:element(undefined, 'photo', [], []),
  Photo = exmpp_xml:set_cdata(Photo0, Id),
  X = exmpp_xml:element('vcard-temp:x:update', 'x', [], [Photo0]),
  Presence0 = exmpp_presence:presence(available, <<"Ready">>),
  exmpp_xml:set_children(Presence0, [X]).

hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).
