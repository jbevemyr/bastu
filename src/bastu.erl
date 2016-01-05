%    -*- Erlang -*-
%    File:      bastu.erl
%    Author:    Johan Bevemyr
%    Created:   Tue Aug  5 14:29:44 2014
%    Purpose:

-module(bastu).
-author('jb@bevemyr.com').

-compile(export_all).
%%-export([Function/Arity, ...]).

-include("/usr/lib/yaws/include/yaws_api.hrl").

-behaviour(gen_server).

%% External exports
-export([start/0, stop/0, reset/0, hard_reset/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% appmod callback
-export([out/1]).

%%

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

-record(post_state, {count=0, acc=[]}).

out(#arg{req = #http_request{method = 'POST'},
         headers = #headers{
           content_type = "multipart/form-data"++_}} = A) ->
    io:format("here\n", []),
    case yaws_api:parse_multipart_post(A) of
        [] ->
            Res = {struct, [{status, "error"},{reason, "broken post"}]},
            rpcreply(Res);
        {cont, _, _}
          when is_record(A#arg.state, post_state),
               A#arg.state#post_state.count == 10 ->
            Res = {struct, [{status, "error"},{reason, "to big post"}]},
            rpcreply(Res);
        {cont, Cont, Res}  ->
            PState = A#arg.state,
            Count = PState#post_state.count,
            Acc = PState#post_state.acc,
            {get_more, Cont, PState#post_state{count=Count+1,acc=[Acc|Res]}};
        {result, Res} ->
            PState = A#arg.state,
            Acc = PState#post_state.acc,
            PostStr = skip_ws(dequote(?b2l(?l2b([Acc|Res])))),
            case json2:decode_string(PostStr) of
                {ok, Json} ->
                    L = yaws_api:parse_query(A),
                    do_op(A#arg.appmoddata, L, Json, A);
                _Reason ->
                    Res = {struct, [{status, "error"},
                                    {reason, "invalid json"}]},
                    rpcreply(Res)
            end
    end;
out(#arg{req = #http_request{method = 'POST'},
           headers = #headers{
             content_type = "application/x-www-form-urlencoded"++_}} = A) ->
    Clidata = ?b2l(A#arg.clidata),
    DecodedClidata = yaws_api:url_decode(Clidata),
    L = yaws_api:parse_query(A),
    %% io:format("got appmod request: ~p\n", [A#arg.appmoddata]),
    case json2:decode_string(dequote(DecodedClidata)) of
        {ok, Json} ->
            do_op(A#arg.appmoddata, L, Json, A);
        _Reason ->
            io:format("json=~p\n", [DecodedClidata]),
            io:format("got error ~p\n", [_Reason]),
            Res = {struct, [{status, "error"},
                            {reason, "invalid json"}]},
            rpcreply(Res)
    end;
out(#arg{req = #http_request{method = 'POST'}} = A) ->
    Clidata = ?b2l(A#arg.clidata),
    L = yaws_api:parse_query(A),
    %% io:format("got appmod request: ~p\n", [A#arg.appmoddata]),
    case json2:decode_string(dequote(Clidata)) of
        {ok, Json} ->
            do_op(A#arg.appmoddata, L, Json, A);
        _Reason ->
            io:format("json=~p\n", [Clidata]),
            io:format("got error ~p\n", [_Reason]),
            Res = {struct, [{status, "error"},
                            {reason, "invalid json"}]},
            rpcreply(Res)
    end;
out(A) ->
    QueryL = yaws_api:parse_query(A),
    L = case (A#arg.req)#http_request.method of
            'GET' ->
                QueryL;
            'POST' ->
                io:format("post=~p\n", [yaws_api:parse_post(A)]),
                QueryL ++ yaws_api:parse_post(A)
        end,
    %% io:format("got appmod request: ~p\n", [A#arg.appmoddata]),
    do_op(A#arg.appmoddata, L, [], A).

do_op(Cmd, L, Json, Arg) ->
    try
        Res = gen_server:call(?SERVER, {Cmd, L, Json, Arg}, infinity),
        rpcreply(Res)
    catch
        X:Y ->
            error_logger:format("crashed for ~p:~p:~p\n~p:~p\n~p\n",
                                [Cmd, L, Json, X, Y, erlang:get_stacktrace()]),
            Res2 = {struct, [{status, "error"},
                            {reason, "internal error"}]},
            rpcreply(Res2)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen server

-record(state, {
          password="gt1722kj"
         }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop, infinity).

reset() ->
    gen_server:call(?SERVER, reset, infinity).

hard_reset() ->
    gen_server:call(?SERVER, hard_reset, infinity).

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
    {X,Y,Z} = erlang:now(),
    random:seed(X, Y, Z),
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
handle_call(stop, _From, S) ->
    {stop, normal, S};

handle_call({Cmd, L, Json, Arg}, _From, S) ->
    try
        %% ?liof("Json=~p\n", [Json]),
        {Res, NewS} = do_cmd(Cmd, L, Json, Arg, S),
        {reply, Res, NewS}
    catch
        X:Y ->
            error_logger:format(
              "error during exection of cmd ~p: ~p ~p\n",
              [{Cmd, L, Json}, {X,Y}, erlang:get_stacktrace()]),
            {reply, {struct, [{status, "error"}, {reason, "internal"}]}, S}
    end;

handle_call(_Request, _From, S) ->
    Res = {struct, [{status, "error"},
                    {reason, "unknown request"}]},
    {reply, Res, S}.

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
handle_info(_Info, State) ->
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

%% http://idrott/idrott/login?user=johan&password=test
do_cmd("get_status", _L, _Json, _Arg, S) ->
    Temp = gen_server:call({bastu, 'bastu@bastu'}, get_temp),
    Status = gen_server:call({bastu, 'bastu@bastu'}, get_status),
    EndsAt = gen_server:call({bastu, 'bastu@bastu'}, get_end_time),
    Res = {struct, [{"temperature", Temp},
                    {"status", Status},
                    {"end_time", EndsAt}]},
    {Res, S};
do_cmd("switch", L, _Json, Arg, S) ->
    %% Needs status and password
    Password = get_val("password", L, ""),
    Status = get_val("status", L, "off"),
    Cookie = get_cookie_val("bastu", Arg),
    if Password == S#state.password orelse Cookie == S#state.password,
       Status == "off" ->
            Ok = gen_server:call({bastu, 'bastu@bastu'}, sauna_off),
            Res = {struct, [{"status", "ok"}, {"result", to_string(Ok)}]},
            {Res, S};
       Password == S#state.password,
       Status == "on" ->
            Ok = gen_server:call({bastu, 'bastu@bastu'}, sauna_on),
            Res = {struct, [{"status", "ok"}, {"result", to_string(Ok)}]},
            {Res, S};
       Password /= S#state.password ->
            Res = {struct, [{"status", "error"},
                            {"result", "invalid password"}]},
            {Res, S};
       true ->
            Res = {struct, [{"status", "error"},
                            {"result", "invalid password"}]},
            {Res, S}
    end;
do_cmd("switch_on", L, _Json, Arg, S) ->
    %% Needs status and password
    Password = get_val("password", L, ""),
    Cookie = get_cookie_val("bastu", Arg),
    if Password == S#state.password orelse Cookie == S#state.password ->
            Ok = gen_server:call({bastu, 'bastu@bastu'}, sauna_on),
            Res = {struct, [{"status", "ok"}, {"result", to_string(Ok)}]},
            {Res, S};
       Password /= S#state.password andalso Cookie /= S#state.password ->
            Res = {struct, [{"status", "error"},
                            {"result", "invalid password"}]},
            {Res, S};
       true ->
            Res = {struct, [{"status", "error"},
                            {"result", "invalid password"}]},
            {Res, S}
    end;
do_cmd("switch_off", L, _Json, Arg, S) ->
    %% Needs status and password
    Password = get_val("password", L, ""),
    Cookie = get_cookie_val("bastu", Arg),
    if Password == S#state.password orelse Cookie == S#state.password ->
            Ok = gen_server:call({bastu, 'bastu@bastu'}, sauna_off),
            Res = {struct, [{"status", "ok"}, {"result", to_string(Ok)}]},
            {Res, S};
       Password /= S#state.password andalso Cookie /= S#state.password ->
            Res = {struct, [{"status", "error"},
                            {"result", "invalid password"}]},
            {Res, S};
       true ->
            Res = {struct, [{"status", "error"},
                            {"result", "invalid password"}]},
            {Res, S}
    end;
do_cmd(Unknown, _L, _Json, _Arg, S) ->
    Error = lists:flatten(io_lib:format("unknown request: ~p", [Unknown])),
    error_logger:format("~s", [Error]),
    {{struct, [{"status", "error"}, {reason, Error}]}, S}.

rpcreply(Response) ->
    X = json2:encode(Response),
    [{header, {cache_control, "no-cache"}},
     {header, "Access-Control-Allow-Origin: *"},
     {header, "Expires: -1"},
     {html, X}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_attrs(L, {struct, Ops}) ->
    L ++ Ops;
merge_attrs(L, _Json) ->
    L.


%% Time management

gnow() -> calendar:datetime_to_gregorian_seconds(calendar:local_time()).

gtostr(Secs) -> gtostr(Secs, date_time).

gtostr(undefined, _) -> "-";
gtostr(Secs, date) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w", [Year, Month, Day]));
gtostr(Secs, time) ->
    {_, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
                                [Hour, Minute, Second]));
gtostr(Secs, time24hm) ->
    {_, {Hour, Minute, _Sec}} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w", [Hour, Minute]));
gtostr(Secs, date_time) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                                [Year, Month, Day, Hour, Minute, Second]));
gtostr(Secs, date_time_nospace) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w_~2.2.0w:~2.2.0w:~2.2.0w",
                                [Year, Month, Day, Hour, Minute, Second])).


days_diff(A, B) when is_integer(A), is_integer(B) ->
    DiffSecs = A - B,
    DiffSecs div 86400;
days_diff(A, B) when is_tuple(A), is_tuple(B) ->
    days_diff(date2gdate(A), date2gdate(B)).

date2gdate(YMD) ->
    calendar:datetime_to_gregorian_seconds({YMD, {0,0,0}}).

%%

get_val(Key, L, Default) ->
    case lists:keysearch(Key, 1, L) of
        {value, {_, undefined}} -> Default;
        {value, {_, Val}} -> Val;
        _ -> Default
    end.


to_int(Str) ->
    case catch ?l2i(Str) of
        I when is_integer(I) ->
            I;
        _ ->
            -1
    end.

dequote([]) -> [];
dequote([$\\, $t|Rest]) -> [$\t|dequote(Rest)];
dequote([$\\, $n|Rest]) -> [$\t|dequote(Rest)];
dequote([$\\, $r|Rest]) -> [$\t|dequote(Rest)];
dequote([C|Rest]) -> [C|dequote(Rest)].

skip_ws([$\t|Rest]) -> skip_ws(Rest);
skip_ws([$\n|Rest]) -> skip_ws(Rest);
skip_ws([$\r|Rest]) -> skip_ws(Rest);
skip_ws([$ |Rest]) -> skip_ws(Rest);
skip_ws(Rest) -> Rest.

get_cookie_val(Cookie, Arg) ->
    H = Arg#arg.headers,
    yaws_api:find_cookie_val(Cookie, H#headers.cookie).


to_string(A) when is_atom(A) ->
    atom_to_list(A);
to_string(L) when is_list(L) ->
    L;
to_string(T) ->
    io_lib:format("~p", [T]).



