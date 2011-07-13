-module(unimate_xmpp_client).

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

%% API
-export([start_link/0, send/1, send/2, send_groupchat/1, send_groupchat/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session,
                jid :: #jid{},
                broadcast_room_jid :: #jid{},
                rooms = [] :: [ #jid{} ]
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send(Msg) ->
  gen_server:call(?SERVER, {send, Msg}).

send(Msg, Jid) ->
  gen_server:call(?SERVER, {send, Msg, Jid}).

send_groupchat(Msg) ->
  gen_server:call(?SERVER, {send_groupchat, Msg}).

send_groupchat(Msg, Jid) ->
  gen_server:call(?SERVER, {send_groupchat, Msg, Jid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  %% TODO: Make exmpp app work with rebar
  %% {ok, User} = application:get_env(user),
  %% {ok, Password} = application:get_env(password),
  %% {ok, Resource} = application:get_env(resource),
  %% {ok, Server} = application:get_env(server),
  %% {ok, Port} = application:get_env(port),
  Password = "CHANGEME",
  JidBin = <<"echo@derbrain.com/work">>,
  Port = 5222,
  Rooms = ["test@conference.derbrain.com"],
  BroadCastRoom = "test@conference.derbrain.com",

  Session = exmpp_session:start(),
  Jid = exmpp_jid:parse(JidBin),
  exmpp_session:auth_basic_digest(Session, Jid, Password),
  {ok, _StreamId} = exmpp_session:connect_TCP(Session, exmpp_jid:domain_as_list(Jid), Port),
  exmpp_session:login(Session),
  Status = exmpp_presence:set_status(exmpp_presence:available(), "Ready"),
  exmpp_session:send_packet(Session, Status),

  BroadCastRoomJid = exmpp_jid:parse(BroadCastRoom),

  State1 = #state{session=Session,
                  jid=Jid,
                  broadcast_room_jid=BroadCastRoomJid},

  %% Connect to all rooms in our config
  State2 = lists:foldl(
            fun(R, S) ->
                J = exmpp_jid:parse(R),
                join_room(J, S)
            end,
            State1,
            Rooms),
  {ok, State2}.


handle_call({send, Msg}, _From, State) ->
  State2 = send_int(Msg, <<"puzza007@derbrain.com">>, State),
  {reply, ok, State2};
handle_call({send, Msg, JidBin}, _From, State) ->
  State2 = send_int(Msg, JidBin, State),
  {reply, ok, State2};
handle_call({send_groupchat, Msg}, _From, State=#state{broadcast_room_jid=Jid}) ->
  State2 = send_groupchat_int(Msg, Jid, State),
  {reply, ok, State2};
handle_call({send_groupchat, Msg, JidBin}, _From, State) ->
  State2 = send_groupchat_int(Msg, JidBin, State),
  {reply, ok, State2}.


handle_cast(_Msg, State) ->
  {stop, unhandled_cast, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_int(Msg, ToJidBin, State) when is_binary(ToJidBin) orelse is_list(ToJidBin) ->
  ToJid = exmpp_jid:parse(ToJidBin),
  send_int(Msg, ToJid, State);

send_int(Msg, ToJid = #jid{}, State = #state{session = Session, jid = Jid}) ->
  Packet = exmpp_stanza:set_sender(
             exmpp_stanza:set_recipient(
               exmpp_message:chat(Msg),
               ToJid),
             Jid),
  exmpp_session:send_packet(Session, Packet),
  State.


send_groupchat_int(Msg, JidBin, State) when is_binary(JidBin) orelse is_list(JidBin) ->
  Jid = exmpp_jid:parse(JidBin),
  send_groupchat_int(Msg, Jid, State);

send_groupchat_int(Msg, ToJid = #jid{},
                   State = #state{session = Session, jid = Jid, rooms = Rooms}) ->
  State2 =
    case lists:member(ToJid, Rooms) of
      true -> State;
      false -> join_room(ToJid, State)
    end,
  Packet = exmpp_stanza:set_sender(
             exmpp_stanza:set_recipient(
               exmpp_message:groupchat(Msg),
               ToJid),
             Jid),
  exmpp_session:send_packet(Session, Packet),
  State2.

join_room(RoomJid = #jid{}, State=#state{session=Session, jid=Jid, rooms=Rooms}) ->
  Packet = room_presence(Jid, RoomJid),
  exmpp_session:send_packet(Session, Packet),
  State#state{rooms=[RoomJid|Rooms]}.

room_presence(Jid = #jid{}, RoomJid = #jid{}) ->
  User = exmpp_jid:node(Jid),
  RoomBin = exmpp_jid:to_binary(RoomJid),
  To = #xmlattr{name = <<"to">>, value = <<RoomBin/binary,"/",User/binary>>},
  XMLNSAttr = #xmlattr{name= <<"xmlns">>, value= <<"http://jabber.org/protocol/muc">>},
  XMLNS = #xmlel{name=x, attrs=[XMLNSAttr]},
  #xmlel{name=presence, attrs=[To], children=[XMLNS]}.
