%    -*- Erlang -*- 
%    File:	bastu_comet.erl  (~jb/work/bastu/pi/bastu/src/bastu_comet.erl)
%    Author:	Johan Bevemyr
%    Created:	Tue Jan  5 15:05:18 2016
%    Purpose:   

-module('bastu_comet').
-author('jb@bevemyr.com').

-compile(export_all).

-define(SERVER, ?MODULE).
-define(PUBSERVER, "http://sauna.bevemyr.com").
-define(RETRY_TTL, 5000).


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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-record(state, {
          id
         }).

init([]) ->
    timer:send_after(?RETRY_TTL, retry),
    Id = get_id(),
    {ok, #state{id=Id}}.

handle_call(Request, _From, S) ->
    error_logger:format("unknown request: ~p\n", [Request]),
    {reply, ok, S}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({reply, Id, Res}, State) ->
    Json = lists:flatten(json2:encode(Res)),
    case url:post(?PUBSERVER++"/bastu_pub/reply?dev="++State#state.id++"&id="++Id, Json) of
        {200, _Header, Body} ->
            case json2:decode_string(Body) of
                {ok, "nowork"} ->
                    ?liof("nothing to do\n", []),
                    self() ! retry;
                {ok, {struct, Args}} ->
                    Rpc = get_opt("rpc", Args, "noop"),
                    Id = get_opt("id", Args, "noref"),
                    ?liof("Got work Json from server: ~p\n", [Json]),
                    do_rpc(Rpc, Id);
                Other ->
                    error_logger:format("got error when decoding json: ~p\n",
                                        [Other]),
                    self() ! retry
            end;
        {error, Reason} ->
            error_logger:format("got error from server: ~p\n", [Reason]),
            %% got an error, wait for retry
            timer:send_after(?RETRY_TTL, retry),
            ok;
        Other ->
            error_logger:format("got other from server: ~p\n", [Other]),
            %% got an error, wait for retry
            timer:send_after(?RETRY_TTL, retry),
            ok
    end,
    {noreply, State};

handle_info(retry, State) ->
    case url:get(?PUBSERVER++"/bastu_pub/get-work?dev="++State#state.id) of
        {200, _Header, Body} ->
            case json2:decode_string(Body) of
                {ok, {struct, Args}} ->
                    Rpc = get_opt("rpc", Args, "noop"),
                    Id = get_opt("id", Args, "noref"),
                    ?liof("Got work from server: ~p:~p\n", [Rpc, Id]),
                    do_rpc(Rpc, Id);
                Other ->
                    error_logger:format("got error when decoding json: ~p\n",
                                        [Other]),
                    self() ! retry
            end;
        {error, Reason} ->
            error_logger:format("got error from server: ~p\n", [Reason]),
            %% got an error, wait for retry
            timer:send_after(?RETRY_TTL, retry),
            ok;
        Other ->
            error_logger:format("got other from server: ~p\n", [Other]),
            %% got an error, wait for retry
            timer:send_after(?RETRY_TTL, retry),
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_rpc(Request, Id) ->
    Res = gen_server:call(bastu, ?l2a(Request)),
    self() ! {reply, Id, Res}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_id() ->
    {ok, Ifaces} = inet:getifaddrs(),
    Hw = get_hw(Ifaces),
    string:join([lists:flatten(io_lib:format("~2.16.0B", [A])) || A <- Hw], ":").

get_hw([]) ->
    %% dummy
    [1,2,3,4,5,6];
get_hw([If|Ifs]) ->
    case lists:keysearch(1, hwaddr, If) of
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

