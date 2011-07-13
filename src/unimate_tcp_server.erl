-module(unimate_tcp_server).

-behaviour(gen_nb_server).

%% API
-export([start_link/0]).

%% gen_nb_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, sock_opts/0, new_connection/2]).

-define(SOCKET_OPTS, [binary, {packet, 0}, {active, false}]).

-record(state, {}).

start_link() ->
  gen_nb_server:start_link(?MODULE, "127.0.0.1", 10101, []).

sock_opts() ->
  ?SOCKET_OPTS.

new_connection(Sock, State) ->
  P = spawn(fun() -> unimate_command2:handle(Sock) end),
  gen_tcp:controlling_process(Sock, P),
  {ok, State}.

init([]) ->
  {ok, #state{}}.

handle_call(_msg, _From, State) ->
  {stop, unhandled_call, State}.

handle_cast(_Msg, State) ->
  {stop, unhandled_cast, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
