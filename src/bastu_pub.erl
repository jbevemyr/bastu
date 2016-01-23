%    -*- Erlang -*-
%    File:      bastu_pub.erl
%    Author:    Johan Bevemyr
%    Created:   Tue Aug  5 14:29:44 2014
%    Purpose:

-module(bastu_pub).
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

-define(EMAIL_SENDER, "info@gt16.se").
-define(MAILSERVER, "mail.bevemyr.com").

-define(USER_DB, "/home/share/jb/work/bastu/user.db.json").
-define(USER_DB_TMP, "/home/share/jb/work/bastu/user.db.json.tmp").

%%

-record(post_state, {count=0, acc=[]}).

out(#arg{req = #http_request{method = 'POST'},
         headers = #headers{
           content_type = "multipart/form-data"++_}} = A) ->
    case yaws_api:parse_multipart_post(A) of
        [] ->
            Res = {struct, [{error, "broken post"}]},
            rpcreply(Res);
        {cont, _, _}
          when is_record(A#arg.state, post_state),
               A#arg.state#post_state.count == 10 ->
            Res = {struct, [{error, "to big post"}]},
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
                    Res = {struct, [{error,"invalid json"}]},
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
            Res = {struct, [{error,"invalid json"}]},
            rpcreply(Res)
    end;
out(#arg{req = #http_request{method = 'POST'}} = A) ->
    case A#arg.clidata of
        {partial, Data} when A#arg.cont =:= undefined ->
            %% first chunk
            {get_more, {cont, size(Data)},
             #post_state{count=size(Data), acc=[Data]}};
        {partial, Data} ->
            {cont, Sz0} = A#arg.cont,
            PState = A#arg.state,
            Acc = PState#post_state.acc,
            {get_more, {cont, Sz0+size(Data)},
             PState#post_state{
               count=PState#post_state.count+size(Data),
               acc = [Acc|Data]}};
        Data ->
            if A#arg.state == undefined ->
                    Clidata = ?b2l(Data);
               true ->
                    PState = A#arg.state,
                    Acc = PState#post_state.acc,
                    Clidata = ?b2l(?l2b([Acc|Data]))
            end,
            L = yaws_api:parse_query(A),
            case json2:decode_string(dequote(Clidata)) of
                {ok, Json} ->
                    do_op(A#arg.appmoddata, L, Json, A);
                _Reason ->
                    ?liof("json=~p\n", [Clidata]),
                    ?liof("got error ~p\n", [_Reason]),
                    Res = {struct, [{error, "invalid json"}]},
                    rpcreply(Res)
            end
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

do_op("get-work", L, Json, _Arg) ->
    try
        Dev = get_val("dev", L, ""),
        %% ?liof("get-work ~p\n", [Dev]),
        case gen_server:call(comet, {get_work, Dev}, infinity) of
            nowork ->
                %% ?liof("no work for ~p\n", [Dev]),
                rpcreply({"nowork", []});
            {Rpc, RpcRef} ->
                %% ?liof("got work for ~p ~p ~p\n", [Dev, Rpc, RpcRef]),
                rpcreply({{struct, [{rpc, Rpc}, {id, RpcRef}]}, []})
        end
    catch X:Y ->
            error_logger:format("crashed for ~p:~p:~p\n~p:~p\n~p\n",
                                ["get-work", L, Json, X, Y,
                                 erlang:get_stacktrace()]),
            Res2 = {struct, [{error, "internal error"}]},
            rpcreply({Res2, []})
    end;

do_op("reply", L, Json, _Arg) ->
    try
        Dev = get_val("dev", L, ""),
        Id = get_val("id", L, ""),
        %% ?liof("reply ~p ~p\n~p\n", [Dev, Id, Json]),
        case gen_server:call(comet, {reply, Dev, Json, Id}, infinity) of
            nowork ->
                %% ?liof("no work for ~p\n", [Dev]),
                rpcreply({"nowork", []});
            {Rpc, RpcRef} ->
                %% ?liof("got work for ~p ~p ~p\n", [Dev, Rpc, RpcRef]),
                rpcreply({{struct, [{rpc, Rpc}, {id, RpcRef}]}, []})
        end
    catch
        X:Y ->
            error_logger:format("crashed for ~p:~p:~p\n~p:~p\n~p\n",
                                ["reply", L, Json, X, Y,
                                 erlang:get_stacktrace()]),
            Res2 = {struct, [{error, "internal error"}]},
            rpcreply({Res2, []})
    end;

do_op(Cmd, L, Json, Arg) ->
    try
        Res = gen_server:call(?SERVER, {Cmd, L, Json, Arg}, infinity),
        rpcreply(Res)
    catch
        X:Y ->
            error_logger:format("crashed for ~p:~p:~p\n~p:~p\n~p\n",
                                [Cmd, L, Json, X, Y, erlang:get_stacktrace()]),
            Res2 = {struct, [{error, "internal error"}]},
            rpcreply(Res2)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen server

-record(user, {
          username = [],          %% string() - username (email addess)
          password = [],          %% string() - password MD5
          sid = [],               %% string() - session id
          confirmed = false,      %% boolean() - confirmed
          role = user,            %% admin | user - privilege group
          passwd_reset_id = [],   %% string() - password reset id
          passwd_reset_send_time=0, %% date_time() - send time time of last
                                    %% password reset email
          device=[],              %% device id
          comment=[],             %% info on reverse ssh port, for example
          data=[]                 %% [{string(), string()}]
         }).


-record(state, {
          users=[]   %% [#user{}]
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
    Users = read_users(),
    comet:start_link(),
    {ok, #state{users=Users}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(hard_reset, _From, S) ->
    file:copy(?USER_DB++".start", ?USER_DB),
    Users = read_users(),
    {reply, ok, S#state{users=Users}};

handle_call(stop, _From, S) ->
    {stop, normal, S};

handle_call({Cmd, L, Json, Arg}, From, S) ->
    try
        %% ?liof("Json=~p\n", [Json]),
        do_cmd(Cmd, L, Json, Arg, From, S)
    catch
        X:Y ->
            error_logger:format(
              "error during exection of cmd ~p: ~p ~p\n",
              [{Cmd, L, Json}, {X,Y}, erlang:get_stacktrace()]),
            {reply, {struct, [{error, "internal"}]}, S}
    end;

handle_call(_Request, _From, S) ->
    Res = {struct, [{error, "unknown request"}]},
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

%% http://bastu.gt16.se/bastu/login?user=johan&password=test
do_cmd("login", L, _Json, _A, _From, S) ->
    User = get_val("user", L, ""),
    Password = get_val("password", L, ""),
    Remember = get_val("remember", L, "false"),
    Md5Pass = encode_pass(Password),
    case get_user_by_name(User, S) of
        U=#user{password = Md5Pass} when Remember == "true" ->
            %% login successful
            {Y,M,D} = date(),
            Expires= format_expires_date({{Y+1,M,D},time()}),
            Headers=[yaws_api:setcookie("sid", U#user.sid, "/", Expires)],
            Res = {struct, [{"sid", U#user.sid},
                            {group, ?a2l(U#user.role)}]};
        U=#user{password = Md5Pass} ->
            %% login successful
            Headers=[yaws_api:setcookie("sid", U#user.sid, "/")],
            Res = {struct, [{"sid", U#user.sid},
                            {group, ?a2l(U#user.role)}]};
        #user{} ->
            Headers=[],
            Res = {struct, [{error, "invalid password"}]};
        _ ->
            Headers=[],
            Res = {struct, [{error, "unknown user"}]}
    end,
    {reply, {Res,Headers}, S};

%% http://bastu.gt16.se/bastu/send_reset_password?user=johan
do_cmd("send_reset_password", L, _Json, _A, _From, S) ->
    User = get_val("user", L, ""),
    case get_user_by_name(User, S) of
        U=#user{} ->
            RPid = mk_rpid(S),
            smtp:send(?MAILSERVER, ?EMAIL_SENDER, [User],
                      "Password reset request",
                      "You have requested a password reset. Follow the link\n"
                      "below to reset the password. Ignore this email if you "
                      "haven't\n"
                      "requested a reset.",
                      "http://bastu.gt16.se/reset_password.html?rpid="++RPid,
                      []),
            %% Note that the reset_password.html page should make a AJAX call to
            %% the reset_password?rpdi=<rpid from post to .html
            %% page>&password=<new password>
            Res = {struct, [{state, "ok"}]},
            NewUser = U#user{passwd_reset_id=RPid,
                             passwd_reset_send_time=gnow()},
            NewUsers = update_user(NewUser, S#state.users),
            NewS = S#state{users=NewUsers};
        _ ->
            Res = {struct, [{state, "error"}, {reason, "unknown user"}]},
            NewS = S
    end,
    {reply, {Res,[]}, NewS};

%% http://bastu.gt16.se/bastu/reset_password?rpid=1234567890&password=test
do_cmd("reset_password", L, _Json, _A, _From, S) ->
    Rpid = get_val("rpid", L, ""),
    Password = get_val("password", L, ""),
    case get_user_by_rpid(Rpid, S) of
        U=#user{} ->
            Age = days_diff(U#user.passwd_reset_send_time, gnow()),
            if Age < 5 ->
                    %% login successful
                    Md5Pass = ?b2l(base64:encode(crypto:hash(md5, Password))),
                    NewU = U#user{password=Md5Pass},
                    Res = {struct, [{user, user2object(U)}]},
                    {{Res,[]}, S#state{users=update_user(NewU, S#state.users)}};
               true ->
                    Res = {struct,
                           [{error, "password reset link has expired"}]},
                    {reply, {Res, []}, S}
            end;
        _ ->
            Res = {struct, [{error, "unknown sid"}]},
            {reply, {Res, []}, S}
    end;

%% http://bastu.gt16.se/bastu/set_password?old_password=test&new_password=test2
do_cmd("set_password", L, _Json, A, _From, S) ->
    Sid = get_sid(A,L),
    OldPass = get_val("old_password", L, ""),
    NewPass = get_val("new_password", L, ""),
    Md5OldPass = ?b2l(base64:encode(crypto:hash(md5, OldPass))),
    Md5NewPass = ?b2l(base64:encode(crypto:hash(md5, NewPass))),
    case get_user_by_id(Sid, S) of
        U=#user{} when Md5OldPass == U#user.password ->
            %% successful auth of old pass
            NewU = U#user{password=Md5NewPass},
            Res = {struct, [{status, "ok"}]},
            {reply, {Res,[]}, S#state{users=update_user(NewU, S#state.users)}};
        _ ->
            Res = {struct, [{error, "unknown sid"}]},
            {reply, {Res, []}, S}
    end;

%% http://bastu.gt16.se/bastu/set_user_password?username=jb&password=test
do_cmd("set_user_password", L, _Json, A, _From, S) ->
    Sid = get_sid(A,L),
    Username = get_val("username", L, ""),
    NewPass = get_val("new_password", L, ""),
    Md5NewPass = ?b2l(base64:encode(crypto:hash(md5, NewPass))),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            case get_user_by_name(Username, S) of
                OU=#user{} ->
                    NewU = OU#user{password=Md5NewPass},
                    Res = {struct, [{status, "ok"}]},
                    NewS = S#state{users=update_user(NewU, S#state.users)},
                    {reply, {Res, []}, NewS};
                false ->
                    Res = {struct, [{error, "unknown user"}]},
                    {reply, {Res, []}, S}
            end;
        U=#user{} when U#user.username == Username ->
            NewU = U#user{password=Md5NewPass},
            Res = {struct, [{status, "ok"}]},
            NewS = S#state{users=update_user(NewU, S#state.users)},
            {reply, {Res, []}, NewS};
        #user{} ->
            Res = {struct, [{error, "only allowed for admin user"}]},
            {reply, {Res,[]}, S};
        _ ->
            Res = {struct, [{error,"unknown sid"}]},
            {reply, {Res,[]}, S}
    end;

%% http://bastu.gt16.se/bastu/get_all_users
do_cmd("get_all_users", L, _Json, A, _From, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        User=#user{} when User#user.role == admin ->
            %% login successful
            Res = {array, [user2object(U) || U <- S#state.users]};
        User=#user{} ->
            %% login successful
            Res = {array, [user2object(User)]};
        _ ->
            Res = {struct, [{error,"unknown sid"}]}
    end,
    {reply, {Res,[]}, S};

%% http://bastu.gt16.se/bastu/get_user
do_cmd("get_user", L, _Json, A, _From, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} ->
            %% login successful
            Res = user2object(U);
        _ ->
            Res = {struct, [{error,"unknown sid"}]}
    end,
    {reply, {Res, []}, S};

%% http://bastu.gt16.se/bastu/get_named_user?username=jb
do_cmd("get_named_user", L, _Json, A, _From, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            Username = get_val("username", L, ""),
            case get_user_by_name(Username, S) of
                OU=#user{} ->
                    Res = user2object(OU);
                false ->
                    Res = {struct, [{error, "unknown user"}]}
            end;
        #user{} ->
            Res = {struct, [{error, "only allowed for admin user"}]};
        _ ->
            Res = {struct, [{error, "unknown sid"}]}
    end,
    {reply, {Res,[]}, S};

%% http://bastu.gt16.se/bastu/set_user
do_cmd("set_user", L, Json, A, _From, S) ->
    Sid = get_sid(A,L),
    case get_user_by_id(Sid, S) of
        U=#user{} ->
            %% login successful
            U2 = apply_user_ops(U,L,U),
            NewU = apply_user_json(U2, Json,U),
            Res = {struct, [{status, "ok"}]},
            {reply, {Res,[]}, S#state{users=update_user(NewU, S#state.users)}};
        _ ->
            Res = {struct, [{error, "unknown sid"}]},
            {reply, {Res, []}, S}
    end;

%% http://bastu.gt16.se/bastu/set_user?username=bar
do_cmd("set_named_user", L, Json, A, _From, S) ->
    Sid = get_sid(A,L),
    Username = get_val("username", L, ""),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            case get_user_by_name(Username, S) of
                OU=#user{} ->
                    U2 = apply_user_ops(OU,L,U),
                    NewU = apply_user_json(U2, Json,U),
                    Res = {struct, [{status, "ok"}]},
                    NewS = S#state{users=update_user(NewU, S#state.users)};
                false ->
                    NewS = S,
                    Res = {struct, [{error, "unknown user"}]}
            end;
        U=#user{} when U#user.username == Username ->
            U2 = apply_user_ops(U,L,U),
            NewU = apply_user_json(U2, Json,U),
            Res = {struct, [{status, "ok"}]},
            NewS = S#state{users=update_user(NewU, S#state.users)};
        #user{} ->
            NewS = S,
            Res = {struct, [{error, "only allowed for admin user"}]};
        _ ->
            NewS = S,
            Res = {struct, [{error, "unknown sid"}]}
    end,
    {reply, {Res, []}, NewS};

%% http://bastu.gt16.se/bastu/add_user?username=jb&
%% password=test&role=user&foo=bar
do_cmd("add_user", L0, Json, A, _From, S) ->
    Sid = get_sid(A, L0),
    L = merge_attrs(L0, Json),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            %% login successful
            Username = get_val("username", L, ""),
            Password = get_val("password", L, ""),
            Role = get_val("role", L, ""),
            Exists = get_user_by_name(Username, S) =/= false,
            if Username == "" ->
                    NewS = S,
                    Res = {struct, [{error, "invalid username"}]};
               Password == "" ->
                    NewS = S,
                    Res = {struct, [{error, "invalid password"}]};
               Exists ->
                    NewS = S,
                    Res = {struct, [{error, "a user with the same name "
                                     "already exists"}]};
               true ->
                    Md5Pass = ?b2l(base64:encode(crypto:hash(md5, Password))),
                    U2 = #user{username=Username,
                               password=Md5Pass,
                               sid=mk_id(S),
                               role=?l2a(Role)},
                    NewU = apply_user_json(U2, Json,U),
                    store_users([NewU|S#state.users]),
                    Res = {struct, [{status, "ok"}]},
                    NewS = S#state{users=[NewU|S#state.users]}
            end;
        #user{} ->
            %% login successful
            NewS = S,
            Res = {struct, [{error, "only admin can create users"}]};
        _ ->
            NewS = S,
            Res = {struct, [{error, "unknown sid"}]}
    end,
    ?liof("Res=~p\n", [Res]),
    {reply, {Res, []}, NewS};

%% http://bastu.gt16.se/bastu/del_user?username=jb
do_cmd("del_user", L, _Json, A, _From, S) ->
    Sid = get_sid(A,L),
    Username = get_val("username", L, ""),
    case get_user_by_id(Sid, S) of
        U=#user{} when U#user.role == admin ->
            DelUser = get_user_by_name(Username, S),
            if Username == "" ->
                    NewS = S,
                    Res = {struct, [{error, "invalid username"}]};
               DelUser == false ->
                    NewS = S,
                    Res = {struct, [{error, "the user does not exist"}]};
               true ->
                    NewUsers = lists:delete(DelUser, S#state.users),
                    store_users(NewUsers),
                    Res = {struct, [{status, "ok"}]},
                    NewS = S#state{users=NewUsers}
            end;
        U=#user{} when U#user.username == Username ->
            NewUsers = lists:delete(U, S#state.users),
            store_users(NewUsers),
            Res = {struct, [{status, "ok"}]},
            NewS = S#state{users=NewUsers};
        #user{} ->
            %% login successful
            NewS = S,
            Res = {struct, [{error, "only admin can remove users"}]};
        _ ->
            NewS = S,
            Res = {struct, [{error, "unknown sid"}]}
    end,
    {reply, {Res, []}, NewS};

do_cmd("get_status", L, _Json, Arg, From, S) ->
    proc_lib:spawn_link(
      fun() ->
              try
                  Sid = get_sid(Arg, L),
                  case get_user_by_id(Sid, S) of
                      U=#user{} ->
                          {ok, Temp}   = call_device(U, "get_temp"),
                          {ok, Status} = call_device(U, "get_status"),
                          {ok, EndsAt} = call_device(U, "get_end_time"),
                          Res = {struct, [{"temperature", Temp},
                                          {"status", Status},
                                          {"end_time", EndsAt}]};
                      _ ->
                          Res = {struct, [{error, "unknown sid"}]}
                  end,
                  gen_server:reply(From, {Res, []})
              catch
                  X:Y ->
                      error_logger:format(
                        "get_status failed: ~p:~p\n~p\n",
                        [X,Y,erlang:get_stacktrace()]),
                      ERes = {struct, [{"error", "no response"}]},
                      gen_server:reply(From, {ERes, []})
              end
      end),
    {noreply, S};

do_cmd("switch_on", L, _Json, Arg, From, S) ->
    proc_lib:spawn_link(
      fun() ->
              try
                  Sid = get_sid(Arg, L),
                  case get_user_by_id(Sid, S) of
                      U=#user{} ->
                          Res =
                              case call_device(U, "sauna_on") of
                                  {ok, Ok} ->
                                      {struct, [{"result", to_string(Ok)}]};
                                  {error, "no such device"} ->
                                      {struct, [{"error", "no such device"}]};
                                  {error, "no response"}->
                                      {struct, [{"error", "no response"}]};
                                  _ ->
                                      {struct, [{"error", "internal"}]}
                              end;
                      _ ->
                          Res = {struct, [{error, "unknown sid"}]}
                  end,
                  gen_server:reply(From, {Res,[]})
              catch
                  X:Y ->
                      error_logger:format("switch_on failed: ~p:~p\n",
                                          [X,Y]),
                      ERes = {struct, [{"error", "no response"}]},
                      gen_server:reply(From, {ERes, []})
              end
      end),
    {noreply, S};

do_cmd("switch_off", L, _Json, Arg, From, S) ->
    proc_lib:spawn_link(
      fun() ->
              try
                  Sid = get_sid(Arg, L),
                  case get_user_by_id(Sid, S) of
                      U=#user{} ->
                          Res =
                              case call_device(U, "sauna_off") of
                                  {ok, Ok} ->
                                      {struct, [{"result", to_string(Ok)}]};
                                  {error, "no such device"} ->
                                      {struct, [{"error", "no such device"}]};
                                  {error, "no response"}->
                                      {struct, [{"error", "no response"}]};
                                  _ ->
                                      {struct, [{"error", "internal"}]}
                              end;
                      _ ->
                          Res = {struct, [{error, "unknown sid"}]}
                  end,
                  gen_server:reply(From, {Res, []})
              catch
                  X:Y ->
                      error_logger:format("switch_off failed: ~p:~p\n",
                                          [X,Y]),
                      ERes = {struct, [{"error", "no response"}]},
                      gen_server:reply(From, {ERes, []})
              end
      end),
    {noreply, S};

do_cmd("unclaimed", L, _Json, Arg, _From, S) ->
    Sid = get_sid(Arg, L),
    case get_user_by_id(Sid, S) of
        #user{} ->
            Ids = gen_server:call(comet, get_ids),
            Unclaimed = [Id || Id <- Ids,
                               not(lists:keymember(Id, #user.device,
                                                   S#state.users))],
            Res = {array, Unclaimed};
        _ ->
            Res = {struct, [{error, "unknown sid"}]}
    end,
    ?liof("Res=~p\n", [Res]),
    {reply, {Res,[]}, S};

do_cmd(Unknown, _L, _Json, _Arg, _From, S) ->
    Error = lists:flatten(io_lib:format("unknown request: ~p", [Unknown])),
    error_logger:format("~s", [Error]),
    {reply, {{struct, [{error, Error}]}, []}, S}.

rpcreply({Response, ExtraHeaders}) ->
    X = json2:encode(Response),
    [{header, {cache_control, "no-cache"}},
     {header, "Access-Control-Allow-Origin: *"},
     {header, "Expires: -1"}]++ExtraHeaders++[{html, X}].

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

to_string(A) when is_atom(A) ->
    atom_to_list(A);
to_string(L) when is_list(L) ->
    L;
to_string(T) ->
    io_lib:format("~p", [T]).



get_sid(A, L) ->
    case get_val("sid", L, "") of
        "" ->
            get_cookie_val("sid", A);
        Sid ->
            Sid
    end.

get_cookie_val(Cookie, Arg) ->
    H = Arg#arg.headers,
    yaws_api:find_cookie_val(Cookie, H#headers.cookie).


get_user_by_id(User, S) ->
    case lists:keysearch(User, #user.sid, S#state.users) of
        {value, U=#user{}} ->
            U;
        false ->
            false
    end.

get_user_by_name(User, S) ->
    case lists:keysearch(User, #user.username, S#state.users) of
        {value, U=#user{}} ->
            U;
        false ->
            false
    end.

format_expires_date({{Year, Month, Date}, {Hours, Minutes, Seconds}}) ->
    lists:flatten(
      io_lib:format("~s, ~p ~s ~p ~2.2.0w:~2.2.0w:~2.2.0w GMT",
                    [weekday({Year, Month, Date}), Date,
                     month(Month), Year, Hours, Minutes, Seconds])).

weekday(Date) ->
    int_to_wd(calendar:day_of_the_week(Date)).

int_to_wd(1) ->
    "Mon";
int_to_wd(2) ->
    "Tue";
int_to_wd(3) ->
    "Wed";
int_to_wd(4) ->
    "Thu";
int_to_wd(5) ->
    "Fri";
int_to_wd(6) ->
    "Sat";
int_to_wd(7) ->
    "Sun".

month(1) ->
    "Jan";
month(2) ->
    "Feb";
month(3) ->
    "Mar";
month(4) ->
    "Apr";
month(5) ->
    "May";
month(6) ->
    "Jun";
month(7) ->
    "Jul";
month(8) ->
    "Aug";
month(9) ->
    "Sep";
month(10) ->
    "Oct";
month(11) ->
    "Nov";
month(12) ->
    "Dec".

mk_id(S) ->
    N = random:uniform(16#ffffffffffffffff), %% 64 bits
    Id = ?i2l(N),
    case lists:keysearch(Id, #user.sid, S#state.users) of
        {value, _} ->
            mk_id(S);
        _ ->
            Id
    end.

mk_rpid(S) ->
    N = random:uniform(16#ffffffffffffffff), %% 64 bits
    Id = ?i2l(N),
    case lists:keysearch(Id, #user.passwd_reset_id, S#state.users) of
        {value, _} ->
            mk_rpid(S);
        _ ->
            Id
    end.

update_user(User, Users) ->
    NewUsers = lists:keyreplace(User#user.username,
                                #user.username, Users, User),
    store_users(NewUsers),
    NewUsers.

get_user_by_rpid(PRID, S) ->
    case lists:keysearch(PRID, #user.passwd_reset_id, S#state.users) of
        {value, U=#user{}} ->
            U;
        false ->
            false
    end.

read_users() ->
    case file:read_file(?USER_DB) of
        {ok, B} ->
            {ok, {array, Users}} = json2:decode_string(?b2l(B)),
            [object2user(U) || U <- Users];
        _ ->
            file:copy(?USER_DB++".start", ?USER_DB),
            read_users()
    end.

store_users(Users) ->
    UserStructs = [user2object(U) || U <- Users],
    String = idrott_json2:encode_pretty({array, UserStructs}),
    file:write_file(?USER_DB_TMP, String),
    file:rename(?USER_DB_TMP, ?USER_DB).

user2object(U) ->
    {struct,
     [{"username", U#user.username},
      {"password", U#user.password},
      {"sid", U#user.sid},
      {"confirmed", U#user.confirmed},
      {"role", ?a2l(U#user.role)},
      {"comment", U#user.comment},
      {"device", U#user.device},
      {"passwd_reset_id", U#user.passwd_reset_id},
      {"passwd_reset_send_time", U#user.passwd_reset_send_time}|
      U#user.data]}.

object2user({struct, Props}) ->
    object2user(#user{}, Props, _Data=[]).

object2user(U, [], Data) ->
    if U#user.username == [] orelse U#user.sid ->
            throw({error, "incomplete user"});
       true ->
            U#user{data=Data}
    end;
object2user(U, [{"username", Username}|Props], Data) ->
    object2user(U#user{username=Username}, Props, Data);
object2user(U, [{"password", Password}|Props], Data) ->
    object2user(U#user{password=Password}, Props, Data);
object2user(U, [{"sid", Sid}|Props], Data) ->
    object2user(U#user{sid=Sid}, Props, Data);
object2user(U, [{"device", Id}|Props], Data) ->
    object2user(U#user{device=Id}, Props, Data);
object2user(U, [{"comment", Str}|Props], Data) ->
    object2user(U#user{comment=Str}, Props, Data);
object2user(U, [{"confirmed", Confirmed}|Props], Data) ->
    object2user(U#user{confirmed=Confirmed}, Props, Data);
object2user(U, [{"role", Role}|Props], Data) ->
    object2user(U#user{role=?l2a(Role)}, Props, Data);
object2user(U, [{"passwd_reset_id", PRI}|Props], Data) ->
    object2user(U#user{passwd_reset_id=PRI}, Props, Data);
object2user(U, [{"passwd_reset_send_time", PRST}|Props], Data) ->
    object2user(U#user{passwd_reset_send_time=PRST}, Props, Data);
object2user(U, [D={Key,_Value}|Props], Data)
  when is_list(Key) ->
    object2user(U, Props, [D|Data]);
object2user(U, [Unknown|Props], Data) ->
    error_logger:format("unknown user property ~p", [Unknown]),
    object2user(U, Props, Data).

apply_user_ops(U, [], _AUser) ->
    U;
apply_user_ops(U, [{"sid", _}|Ops], AUser) ->
    apply_user_ops(U, Ops, AUser);
apply_user_ops(U, [{"password", _}|Ops], AUser) ->
    apply_user_ops(U, Ops, AUser);
apply_user_ops(U, [{"username", _}|Ops], AUser) ->
    apply_user_ops(U, Ops, AUser);
apply_user_ops(U, [{"passwd_reset_id", _}|Ops], AUser) ->
    apply_user_ops(U, Ops, AUser);
apply_user_ops(U, [{"passwd_reset_send_time", _}|Ops], AU) ->
    apply_user_ops(U, Ops, AU);
apply_user_ops(U, [{"device", Dev}|Ops], AU) ->
    apply_user_ops(U#user{device=Dev}, Ops, AU);
apply_user_ops(U, [{"comment", Dev}|Ops], AU) ->
    apply_user_ops(U#user{comment=Dev}, Ops, AU);
apply_user_ops(U, [{"role", NewRole}|Ops], AU) ->
    if AU#user.role == admin ->
            apply_user_ops(U#user{role=?l2a(NewRole)}, Ops, AU);
       true ->
            apply_user_ops(U, Ops, AU)
    end;
apply_user_ops(U, [{Key,Value}|Ops], AU) ->
    Data = lists:keydelete(Key, 1, U#user.data),
    apply_user_ops(U#user{data=[{Key, Value}|Data]}, Ops, AU);
apply_user_ops(U, [_|Ops], AU) ->
    apply_user_ops(U, Ops, AU).

apply_user_json(U, {struct, Ops}, AU) ->
    apply_user_ops(U, Ops, AU);
apply_user_json(U, _, _AU) ->
    U.

%

call_device(U, Cmd) ->
    if U#user.device == undefined ->
            "no device";
       true ->
            gen_server:call(comet, {call, U#user.device, Cmd}, infinity)
    end.

encode_pass(Password) ->
    ?b2l(base64:encode(crypto:hash(md5, Password))).
