%    -*- Erlang -*-
%    File:      bastu_comet.erl  (~jb/work/bastu/pi/bastu/src/bastu_comet.erl)
%    Author:    Johan Bevemyr
%    Created:   Tue Jan  5 15:05:18 2016
%    Purpose:

-module('bastu_comet').
-author('jb@bevemyr.com').

-compile(export_all).

-define(SERVER, ?MODULE).
-define(PUBSERVER, "http://bastu.gt16.se").
-define(WORK_REQUEST_TTL, 5000).


-define(i2l(X), integer_to_list(X)).
-define(l2i(X), list_to_integer(X)).
-define(l2b(X), list_to_binary(X)).
-define(l2f(X), list_to_float(X)).
-define(b2l(X), binary_to_list(X)).
-define(a2l(X), atom_to_list(X)).
-define(l2a(X), list_to_atom(X)).

-define(stack(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).
-define(liof(Fmt, Args), io:format(user, "~w:~w ~p" ++ Fmt,[?MODULE,?LINE,time()|Args])).
-define(liof_bt(Fmt, Args), io:format(user, "~w:~w ~s ~p\n",
                             [?MODULE, ?LINE,
                              io_lib:format(Fmt, Args), ?stack()])).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-record(state, {
          id
         }).

init([]) ->
    timer:send_after(?WORK_REQUEST_TTL, work_request),
    Id = get_id(),
    {ok, #state{id=Id}}.

handle_call(Request, _From, S) ->
    error_logger:format("unknown request: ~p\n", [Request]),
    {reply, ok, S}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({reply, RId, Res}, State) ->
    %% Reply to previous request, and ask for more work
    %% at the same time
    %% ?liof("sending reply ~p ~p\n", [Res, RId]),
    Json = lists:flatten(json2:encode(to_string(Res))),
    Url = ?PUBSERVER++"/bastu_pub/reply?dev="++State#state.id++"&id="++RId,
    %% ?liof("POST ~p\n", [Json]),
    WorkResponse = url:post(Url, Json),
    %% ?liof("SENT ~p\n", [Json]),
    process_work_response(WorkResponse),
    {noreply, State};

handle_info(work_request, State) ->
    %% Ask for work
    Url = ?PUBSERVER++"/bastu_pub/get-work?dev="++State#state.id,
    WorkResponse = url:get(Url),
    process_work_response(WorkResponse),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_work_response(Work) ->
    case Work of
        {200, _Header, Body0} ->
            Body=lists:flatten(Body0),
            %% ?liof("process_work_response: ~p\n",
            %%       [json2:decode_string(Body)]),
            case json2:decode_string(Body) of
                {ok, "nowork"} ->
                    self() ! work_request;
                {ok, {struct, Args}} ->
                    Rpc = get_opt("rpc", Args, "noop"),
                    Id = get_opt("id", Args, "noref"),
                    do_rpc(Rpc, Id);
                Other ->
                    error_logger:format("got error when decoding json: ~p\n",
                                        [Other]),
                    self() ! work_request
            end;
        Error ->
            error_logger:format("got error from server: ~p\n", [Error]),
            %% got an error, wait a short time before we try again
            %% to avoid flooding server with requests
            timer:send_after(?WORK_REQUEST_TTL, work_request),
            ok
    end.

do_rpc(Request, Id) ->
    %% ?liof("do_rpc ~p ~p\n", [Request, Id]),
    Res = gen_server:call(bastu, ?l2a(Request)),
    %% ?liof("do_rpc response ~p ~p\n", [Res, Id]),
    self() ! {reply, Id, Res}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_id() ->
    {ok, Ifaces} = inet:getifaddrs(),
    Hw = get_hw(Ifaces),
    string:join([lists:flatten(
                   io_lib:format("~2.16.0B", [A])) || A <- Hw], ":").

get_hw([]) ->
    %% dummy
    [1,2,3,4,5,6];
get_hw([{_,If}|Ifs]) ->
    case lists:keysearch(hwaddr, 1, If) of
        {value, {_, [0,0,0,0,0,0]}} ->
            get_hw(Ifs);
        {value, {_, Hw}} ->
            Hw;
        _ ->
            get_hw(Ifs)
    end.

get_opt(_Tag, [], Default) ->
    Default;
get_opt(Tag, [{Tag, Value}|_Rest], _Default) ->
    Value;
get_opt(Tag, [_Opt|Rest], Default) ->
    get_opt(Tag, Rest, Default).


to_string(A) when is_atom(A) ->
    ?a2l(A);
to_string(B) when is_binary(B) ->
    ?b2l(B);
to_string(T) ->
    T.
