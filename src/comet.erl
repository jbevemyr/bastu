%    -*- Erlang -*-
%    File:      comet.erl  (~jb/work/bastu/src/comet.erl)
%    Author:    Johan Bevemyr
%    Created:   Tue Jan  5 12:28:41 2016
%    Purpose:

-module('comet').
-author('jb@bevemyr.com').

-compile(export_all).

%% External exports
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%

-define(SERVER, ?MODULE).
-define(i2l(X), integer_to_list(X)).
-define(l2i(X), list_to_integer(X)).
-define(l2b(X), list_to_binary(X)).
-define(l2f(X), list_to_float(X)).
-define(b2l(X), binary_to_list(X)).
-define(a2l(X), atom_to_list(X)).
-define(l2a(X), list_to_atom(X)).

-define(stack(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).
-define(liof(Fmt, Args), io:format(user, "~w:~w " ++ Fmt,[?MODULE,?LINE|Args])).
-define(liof_bt(Fmt, Args), io:format(user, "~w:~w ~s ~p\n",
                             [?MODULE, ?LINE,
                              io_lib:format(Fmt, Args), ?stack()])).

%%

-define(ENTRY_TTL, (1000*60*30)).  %% 30 minutes
-define(COMET_TTL, (1000*60)).     %% keep device comet query for 1 minute
-define(RPC_TTL, (1000*5)).       %% query timeout, 1 minute

%%

-record(device, {
          id,          %% device id
          last_seen,   %% time of last seen
          dev_from,    %% from devide
          dev_tref,    %% device timer ref
          timeref,     %% entry timer ref
          queue=[],    %% [{From, Rpc}]
          pending      %% {From, RpcRef, TimeRef}
         }).

-record(state, {
          devices=[]       %% [#device{}]
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({call, Dev, Rpc}, From, S) ->
    Devices = S#state.devices,
    case lists:keysearch(Dev, #device.id, Devices) of
        {value, D} ->
            RpcRef = mk_id(),
            {ok, TRef} = timer:send_after(?RPC_TTL,
                                          {rpc_timeout, Dev, RpcRef}),
            D2 = D#device{queue=D#device.queue++[{From, Rpc, RpcRef, TRef}]},
            D3 = dispatch(D2),
            NewDevices = update_devices(Devices, D3),
            {noreply, S#state{devices=NewDevices}};
        false ->
            {reply, {error, "no such device"}, S}
    end;
handle_call({get_work, Dev}, From, S) ->
    Devices = S#state.devices,
    case lists:keysearch(Dev, #device.id, Devices) of
        {value, D} ->
            ok;
        false ->
            D = #device{id=Dev}
    end,
    D2 = cancel_request(D),
    D3 = cancel_pending(D2),
    {ok, TRef} = timer:send_after(?COMET_TTL, {comet_timeout, Dev}),
    D4 = D3#device{last_seen=gnow(), dev_from=From, dev_tref=TRef},
    D5 = dispatch(D4),
    D6 = reset_device_timer(D5),
    NewDevices = update_devices(Devices, D6),
    {noreply, S#state{devices=NewDevices}};
handle_call({reply, Dev, Res, Ref}, From, S) ->
    Devices = S#state.devices,
    case lists:keysearch(Dev, #device.id, Devices) of
        {value, D} ->
            ok;
        false ->
            D = #device{id=Dev}
    end,
    D2 = cancel_request(D),
    D3 = reply_pending(D2, Res, Ref),
    {ok, TRef} = timer:send_after(?COMET_TTL, {comet_timeout, Dev}),
    D4 = D3#device{last_seen=gnow(), dev_from=From, dev_tref=TRef},
    D5 = dispatch(D4),
    D6 = reset_device_timer(D5),
    NewDevices = update_devices(Devices, D6),
    {noreply, S#state{devices=NewDevices}};
handle_call(get_ids, _From, S) ->
    Ids = [D#device.id || D <- S#state.devices],
    {reply, Ids, S};
handle_call(stop, _From, S) ->
    {stop, normal, S};

handle_call(Request, _From, S) ->
    {reply, {error, {unknown_request, Request}}, S}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({comet_timeout, Dev}, State) ->
    Devices = State#state.devices,
    case lists:keysearch(Dev, #device.id, Devices) of
        {value, D} ->
            D2 = cancel_request(D),
            NewDevices = update_devices(Devices, D2),
            {noreply, State#state{devices=NewDevices}};
        _ ->
            {noreply, State}
    end;

handle_info({device_timeout, Dev}, State) ->
    %% haven't heard from device in a long time, remove
    Devices = State#state.devices,
    case lists:keysearch(Dev, #device.id, Devices) of
        {value, D} ->
            D2 = cancel_request(D),
            cancel_queue(D2),
            NewDevices = lists:keydelete(Dev, #device.id, Devices),
            {noreply, State#state{devices=NewDevices}};
        _ ->
            {noreply, State}
    end;

handle_info({rpc_timeout, Dev, RpcRef}, State) ->
    ?liof("rpc timeout ~p ~p\n", [Dev, RpcRef]),
    %% haven't heard from device in a long time, remove
    Devices = State#state.devices,
    case lists:keysearch(Dev, #device.id, Devices) of
        {value, D} ->
            D2 = cancel_rpc(D, RpcRef),
            NewDevices = update_devices(Devices, D2),
            {noreply, State#state{devices=NewDevices}};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    ?liof("unknown info ~p\n", [_Info]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

dispatch(D) when D#device.queue == [] orelse
                 D#device.pending =/= undefined orelse
                 D#device.dev_from == undefined ->
    %% nothing to do:
    %% - queue empty or
    %% - rpc already in progress or
    %% - device not asking for work
    D;
dispatch(D=#device{queue=[{From, Rpc, RpcRef, TRef}|Rest]}) ->
    %% device ready to get request
    gen_server:reply(D#device.dev_from, {Rpc, RpcRef}),
    timer:cancel(D#device.dev_tref),
    D#device{pending={From, RpcRef, TRef},
             queue = Rest,
             dev_from=undefined,
             dev_tref=undefined}.

cancel_request(D) ->
    if D#device.dev_from =/= undefined ->
            gen_server:reply(D#device.dev_from, nowork);
       true ->
            ok
    end,
    if D#device.dev_tref =/= undefined ->
            timer:cancel(D#device.dev_tref);
       true ->
            ok
    end,
    D#device{dev_from=undefined, dev_tref=undefined}.

cancel_pending(D) ->
    if D#device.pending =/= undefined ->
            {From, _RpcRef, TRef} = D#device.pending,
            timer:cancel(TRef),
            gen_server:reply(From, {error, "no response"});
       true ->
            ok
    end,
    D#device{pending = undefined}.

cancel_queue(D) ->
    if D#device.pending =/= undefined ->
            {From, _RpcRef, TRef} = D#device.pending,
            timer:cancel(TRef),
            gen_server:reply(From, {error, "no response"});
       true ->
            ok
    end,
    [timer:cancel(TRef) ||
        {_From, _Rpc, _RpcRef, TRef} <- D#device.queue],
    [gen_server:reply(From, {error, "no response"}) ||
        {From, _Rpc, _RpcRef, _TRef} <- D#device.queue],
    D#device{pending=undefined, queue=[]}.

cancel_rpc(D, RpcRef) ->
    case D#device.pending of
        {PFrom, RpcRef, _PTRef} ->
            gen_server:reply(PFrom, {error, "no response"}),
            NewPending = undefined;
        _ ->
            NewPending = D#device.pending
    end,
    case lists:keysearch(RpcRef, 3, D#device.queue) of
        {value, {QFrom, _Rpc, RpcRef, _QTRef}} ->
            gen_server:reply(QFrom, {error, "no response"}),
            NewQueue = lists:keydelete(RpcRef, 3, D#device.queue);
        _ ->
            NewQueue = D#device.queue
    end,
    D#device{pending=NewPending, queue=NewQueue}.

reply_pending(D, Res, Ref) ->
    if D#device.pending =/= undefined ->
            {From, RpcRef, TRef} = D#device.pending,
            if Ref == RpcRef ->
                    timer:cancel(TRef),
                    gen_server:reply(From, {ok, Res}),
                    D#device{pending=undefined};
               true ->
                    D
            end;
       true ->
            D
    end.

update_devices(Devices, D) ->
    case lists:keysearch(D#device.id, #device.id, Devices) of
        {value, _} ->
            lists:keyreplace(D#device.id, #device.id, Devices, D);
        _ ->
            [D|Devices]
    end.

reset_device_timer(D) ->
    if D#device.timeref =/= undefined ->
            timer:cancel(D#device.timeref);
       true ->
            ok
    end,
    {ok, Tref} = timer:send_after(?ENTRY_TTL, {device_timeout, D#device.id}),
    D#device{timeref=Tref}.

gnow() ->
    {date(), time()}.

mk_id() ->
    N = random:uniform(16#ffffffffffffffff), %% 64 bits
    ?i2l(N).
