%    -*- Erlang -*-
%    File:      bastu.erl
%    Author:    Johan Bevemyr
%    Created:   Tue Aug  5 14:29:44 2014
%    Purpose:

-module(bastu).
-author('jb@bevemyr.com').

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(RUNTIME, 3*60*60).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-record(state, {
          hw_switch,
	  hw_timer,
	  ref,
	  temp,
	  status="off",
	  start_time=gnow()
         }).

init([]) ->
    {ok, Gpio10} = gpio:start_link(10, output),
    {ok, Gpio11} = gpio:start_link(11, output),
    gpio:write(Gpio10, 0),
    gpio:write(Gpio11, 0),
    timer:send_after(5000, temp),
    {ok, #state{hw_switch=Gpio10, hw_timer=Gpio11}}.

handle_call(sauna_on, _From, S) ->
    if S#state.hw_timer == undefined ->
	    ok;
       true ->
	    timer:cancel(S#state.hw_timer)
    end,
    gpio:write(S#state.hw_switch, 1),
    gpio:write(S#state.hw_timer, 1),
    %% run for three hours
    {ok, Ref} = timer:send_after(?RUNTIME*1000, sauna_timeout),
    {reply, ok, S#state{ref=Ref, status="on", start_time=gnow()}};
handle_call(sauna_off, _From, S) ->
    gpio:write(S#state.hw_switch, 0),
    gpio:write(S#state.hw_timer, 0),
    timer:cancel(S#state.ref),
    {reply, ok, S#state{ref=undefined, status="off"}};
handle_call(get_temp, _From, S) ->
    {reply, S#state.temp, S};
handle_call(get_status, _From, S) ->
    {reply, S#state.status, S};
handle_call(get_end_time, _From, S) ->
    %% {reply, gtostr(S#state.start_time+?RUNTIME, time), S};
    Remain = S#state.start_time+?RUNTIME-gnow(),
    {reply, Remain, S};
handle_call(Request, _From, S) ->
    error_logger:format("unknown request: ~p\n", [Request]),
    {reply, ok, S}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(temp, S) ->
    try
	Temp = string:strip(os:cmd("/usr/local/src/lightstrip/readtemp.sh"),
			    both, $\n),
	[_,TempStr] = string:tokens(Temp, "="),
	timer:send_after(5000, temp),
	{noreply, S#state{temp=TempStr}}
    catch
	X:Y ->
	    error_logger:format("failed to read temp: ~p:~p\n", [X,Y]),
            timer:send_after(5000, temp),
	    {noreply, S#state{temp="0"}}
    end;
handle_info(sauna_timeout, S) ->
    gpip:write(S#state.hw_switch, 0),
    gpio:write(S#state.hw_timer, 0),
    {noreply, S#state{ref=undefined, status="off"}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Time management

gnow() ->
     calendar:datetime_to_gregorian_seconds(calendar:local_time()).

gtostr(Secs) ->
     gtostr(Secs, date_time).

gtostr(undefined, _) ->
    "-";
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

