%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Small name registration server
%%% @end
%%% Created : 28 Feb 2018 by Tony Rogvall <tony@rogvall.se>

-module(movi_reg).

-export([register/2, unregister/1, lookup/1]).

-define(SERVER, movi_reg).

register(Name, Value) ->
    case whereis(movi_reg) of
	undefined ->
	    case start() of
		ok -> ets:insert(movi_reg, {Name,Value});
		Error -> Error
	    end;
	_Pid ->
	    ets:insert(movi_reg, {Name,Value})
    end.

lookup(Name) ->
    try ets:lookup(movi_reg, Name) of
	[{_,Value}] -> Value;
	_ -> undefined
    catch
	error:_ -> undefined
    end.

unregister(Name) ->
    catch ets:delete(movi_reg, Name),
    ok.
	    
start() ->
    CALLER = self(),
    {Pid,Mon} =
	spawn_monitor(
	  fun() ->
		  ets:new(movi_reg, [public,named_table]),
		  erlang:register(?SERVER, self()),
		  CALLER ! {self(), ok},
		  loop()
	  end),
    receive
	{Pid, ok} -> 
	    erlang:demonitor(Mon,[flush]),
	    ok;
	{'DOWN',Mon,process,Pid,_Reason} ->
	    case whereis(?SERVER) of
		undefined -> {error,nopid};
		_ -> ok
	    end
    end.

loop() ->
    receive
	stop ->
	    ok;
	Other ->
	    io:format("movi_reg loop go ~w\n", [Other]),
	    loop()
    end.


	    

