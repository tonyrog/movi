%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Import imdb files to internal term format
%%% ftp://ftp.sunet.se/pub/tv+movies/imdb/ratings.list.gz
%%% @end
%%% Created : 11 Feb 2016 by Tony Rogvall <tony@rogvall.se>

-module(imdb_ratings).

-export([load/0, load/2]).
-export([import/0, import/1]).

-include("../include/imdb.hrl").

-define(is_digit(C),
	(((C) >= $0) andalso ((C) =< $9))).

-define(is_dchar(C),
	( (((C) >= $0) andalso ((C) =< $9)) 
	  orelse ((C) =:= $.) 
	  orelse ((C) =:= $*) )).

load() ->
    Ets = ets:new(imdb, []),
    ok = load("ratings.dat", Ets),
    Ets.

import() ->
    import("ratings.list.gz").

import(File) ->
    FileName = filename:join(code:priv_dir(movi), File),
    case file:open(FileName, [raw, read, binary, compressed,
			      {read_ahead, 1024*64}]) of
	{ok,Fd} ->
	    Ets = ets:new(imdb, []),
	    io:format("loading rating table\n"),
	    T0 = erlang:system_time(milli_seconds),
	    try load_listing(Fd, Ets) of
		N -> N
	    after
		file:close(Fd)
	    end,
	    T1 = erlang:system_time(milli_seconds),
	    io:format("loaded, ~w records in ~f seconds\n", 
		      [ets:info(Ets, size), (T1-T0)/1000]),
	    save_ets("ratings.dat", Ets);
	Error ->
	    Error
    end.

%% save ets table in <size><term> format
save_ets(Name, Ets) ->
    FileName = filename:join(code:priv_dir(movi), Name),
    {ok,Fd} = file:open(FileName, [raw,write,binary]),
    ets:foldl(
      fun(Term, _Acc) ->
	      Bin = term_to_binary(Term),
	      file:write(Fd, [<<(byte_size(Bin)):32>>,Bin])
      end, [], Ets),
    file:close(Fd).

load(Name, Ets) ->
    FileName = filename:join(code:priv_dir(movi), Name),
    {ok,Fd} = file:open(FileName, [raw,read,binary,{read_ahead, 1024*64}]),
    load_ets_loop(Fd, Ets),
    file:close(Fd).

load_ets_loop(Fd, Ets) ->
    case file:read(Fd, 4) of
	{ok, <<Size:32>>} ->
	    case file:read(Fd, Size) of
		{ok, Bin} ->
		    ets:insert(Ets, binary_to_term(Bin)),
		    load_ets_loop(Fd, Ets);
		Error ->
		    Error
	    end;
	eof ->
	    ok;
	Error ->
	    Error
    end.

%% load rating file into an ets table
%% Keys are {Title, Year} or {Title, Year, Season, Episode}
%%  tv_series have attribute {episodes, [{Season,Episode}]} to
%%  link to the individual episodes
%%
load_listing(Fd,Ets) ->
    load_lst(Fd,
	    fun(Item) ->
		    Y = proplists:get_value(year,Item#item.attributes),
		    E = proplists:get_value(episode,Item#item.attributes),
		    S = proplists:get_value(season,Item#item.attributes),
		    T = Item#item.title,
		    if S =:= undefined, E =:= undefined ->
			    ets:insert(Ets, {{T,Y}, Item});
		       true ->
			    case ets:lookup(Ets, {T,Y}) of
				[] ->
				    Attr1 = delete_props([episode,season],
							 Item#item.attributes),
				    Attr2 = [{episodes,[{S,E}]}|Attr1],
				    Item1 = Item#item { attributes = Attr2 },
				    ets:insert(Ets, {{T,Y}, Item1}),
				    ets:insert(Ets, {{T,Y,S,E}, Item});
				[{_,Item1}] ->
				    %% update with episode
				    Es = proplists:get_value(episodes,
							     Item1#item.attributes,[]),
				    Attr1 = delete_props([tv_series,episodes],
							 Item1#item.attributes),
				    Attr2 = [tv_series,
					     {episodes,[{S,E}|Es]}|Attr1],
				    Item2 = Item1#item { attributes = Attr2 },
				    ets:insert(Ets, {{T,Y}, Item2}),
				    ets:insert(Ets, {{T,Y,S,E}, Item})
			    end
		    end,
		    true
	    end).

load_lst(Fd,Callback) ->
    load_lst(Fd, 0, Callback).

load_lst(Fd, Count, Callback) ->
    case file:read_line(Fd) of
	eof ->
	    Count;
	{ok,Line} ->
	    try scan_line(Line) of
		error ->
		    load_lst(Fd, Count, Callback);
		{ok,Item} ->
		    try Callback(Item) of
			false ->
%%			    io:format("~30s ~w ~w [~s]\n", 
%%				      [Item#item.title,
%%				       Item#item.rating,
%%				       Item#item.votes,
%%				       format_attr(Item#item.attributes)]),
			    load_lst(Fd, Count+1, Callback);
			true ->
			    load_lst(Fd, Count, Callback)
		    catch
			error:_Error ->
			    io:format("crash: ~p\n", [erlang:get_stacktrace()]),
			    load_lst(Fd, Count, Callback)
		    end
	    catch
		error:_ ->
		    load_lst(Fd, Count, Callback)
	    end
    end.
		       
scan_line(<<"      ",D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,Rest/binary>>) 
  when ?is_dchar(D1),?is_dchar(D2),?is_dchar(D3),?is_dchar(D4),
       ?is_dchar(D5),?is_dchar(D6),?is_dchar(D7),?is_dchar(D8),
       ?is_dchar(D9),?is_dchar(D10) ->
    {Votes, Rest1} = to_integer(Rest),
    {Rating, Rest2} = to_float(Rest1),
    {Attr,Rest3} = scan_attributes(Rest2, [], []),
    Title = trim(Rest3),
    {ok,#item{new=false,
	      dist=[D1,D2,D3,D4,D5,D6,D7,D8,D9,D10],
	      votes=Votes, 
	      rating=Rating, 
	      attributes=Attr,
	      title=Title}};
scan_line(<<"*     ",D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,Rest/binary>>) 
  when ?is_dchar(D1),?is_dchar(D2),?is_dchar(D3),?is_dchar(D4),
       ?is_dchar(D5),?is_dchar(D6),?is_dchar(D7),?is_dchar(D8),
       ?is_dchar(D9),?is_dchar(D10) ->
    {Votes, Rest1} = to_integer(Rest),
    {Rating, Rest2} = to_float(Rest1),
    {Attr,Rest3} = scan_attributes(Rest2, [], []),
    Title = trim(Rest3),
    {ok,#item{new=true,
	      dist=[D1,D2,D3,D4,D5,D6,D7,D8,D9,D10],
	      votes=Votes, 
	      rating=Rating, 
	      attributes=Attr,
	      title=Title}};
scan_line(_) ->
    error.


scan_attributes(<<"(TV)", Rest/binary>>, Acc, Attr) ->
    scan_attributes(Rest, Acc, [tv|Attr]);
scan_attributes(<<"(VG)", Rest/binary>>, Acc, Attr) ->
    scan_attributes(Rest, Acc, [video_game|Attr]);
scan_attributes(<<"(V)", Rest/binary>>, Acc, Attr) ->
    scan_attributes(Rest, Acc, [video|Attr]);

scan_attributes(<<"(",Y1,Y2,Y3,Y4,")", Rest/binary>>, Acc, Attr) when
      ?is_digit(Y1),?is_digit(Y2),?is_digit(Y3),?is_digit(Y4) ->
    Year = (Y1-$0)*1000+(Y2-$0)*100+(Y3-$0)*10+(Y4-$0),
    scan_attributes(Rest, Acc, [{year,Year}|Attr]);
scan_attributes(<<"(",Y1,Y2,Y3,Y4,"/", Rest/binary>>, Acc, Attr) when
      ?is_digit(Y1),?is_digit(Y2),?is_digit(Y3),?is_digit(Y4) ->
    Year = (Y1-$0)*1000+(Y2-$0)*100+(Y3-$0)*10+(Y4-$0),
    {Part,<<$),Rest1/binary>>} = roman_number(Rest),
    scan_attributes(Rest1, Acc, [{year,Year},{part,Part}|Attr]);
scan_attributes(<<"{",Rest/binary>>, Acc, Attr) ->
    scan_episode(Rest, "", Acc, Attr);
scan_attributes(<<C, Rest/binary>>, Acc, Attr) ->
    scan_attributes(Rest, [C|Acc], Attr);
scan_attributes(<<>>, Acc, Attr) ->
    {Attr,list_to_binary(lists:reverse(Acc))}.

scan_episode(<<"(#",Rest/binary>>, Acc1, Acc, Attr) ->
    {Season, <<$.,Rest1/binary>>} = to_integer(Rest),
    {Episode, <<$),Rest2/binary>>} = to_integer(Rest1),
    scan_episode(Rest2, Acc1, Acc, [tv_series,
				    {season,Season},{episode,Episode}|Attr]);
scan_episode(<<"}", Rest/binary>>, Acc1, Acc, Attr) ->
    Name = list_to_binary(lists:reverse(Acc1)),
    scan_attributes(Rest, Acc, [{episode_name,trim(Name)}|Attr]);
scan_episode(<<C,Rest/binary>>, Acc1, Acc, Attr) ->
    scan_episode(Rest, [C|Acc1], Acc, Attr);
scan_episode(<<>>, Acc1, Acc, Attr) ->
    Name = list_to_binary(lists:reverse(Acc1)),
    scan_attributes(<<>>, Acc, [{episode_name,trim(Name)}|Attr]).

roman_number(Bin) ->
    roman_number(Bin, 0).

roman_number(<<$I,$V,Bin/binary>>, N) -> roman_number(Bin, 4+N);
roman_number(<<$I,$X,Bin/binary>>, N) -> roman_number(Bin, 9+N);
roman_number(<<$X,$L,Bin/binary>>, N) -> roman_number(Bin, 40+N);
roman_number(<<$X,$C,Bin/binary>>, N) -> roman_number(Bin, 90+N);
roman_number(<<$C,$D,Bin/binary>>, N) -> roman_number(Bin, 400+N);
roman_number(<<$C,$M,Bin/binary>>, N) -> roman_number(Bin, 900+N);
roman_number(<<$M,Bin/binary>>, N) -> roman_number(Bin, 1000+N);
roman_number(<<$D,Bin/binary>>, N) -> roman_number(Bin, 500+N);
roman_number(<<$C,Bin/binary>>, N) -> roman_number(Bin, 100+N);
roman_number(<<$L,Bin/binary>>, N) -> roman_number(Bin, 50+N);
roman_number(<<$X,Bin/binary>>, N) -> roman_number(Bin, 10+N);
roman_number(<<$V,Bin/binary>>, N) -> roman_number(Bin, 5+N);
roman_number(<<$I,Bin/binary>>, N) -> roman_number(Bin, 1+N);
roman_number(Bin, N) ->
    {N, Bin}.


    

to_integer(<<$\s,Bin/binary>>) -> to_integer(Bin);
to_integer(<<$\t,Bin/binary>>) -> to_integer(Bin);
to_integer(<<$-,C,Bin/binary>>) when C >= $0, C =< $9 ->
    case to_integer(Bin, C-$0) of
	Err = {error,_Reason} -> Err;
	{N,Rest} -> {-N, Rest}
    end;
to_integer(<<C,Bin/binary>>) when C >= $0, C =< $9 ->
    to_integer(Bin, C-$0);
to_integer(Data) when is_binary(Data) ->
    {error, no_integer};
to_integer(_) ->
    {error, not_a_binary}.

to_integer(<<C,Bin/binary>>,N) when C >= $0, C =< $9 ->
    to_integer(Bin, N*10+(C-$0));
to_integer(Rest, N) ->
    {N, Rest}.

to_float(<<$\s,Bin/binary>>) -> to_float(Bin);
to_float(<<$\t,Bin/binary>>) -> to_float(Bin);
to_float(<<$-,C,Bin/binary>>) when C >= $0, C =< $9 ->
    case to_float(Bin, C-$0) of
	Err = {error,_Reason} -> Err;
	{F, Rest} -> {-F, Rest}
    end;
to_float(<<C,Bin/binary>>) when C >= $0, C =< $9 ->
    to_float(Bin, C-$0);
to_float(Data) when is_binary(Data) ->
    {error, no_float};
to_float(_) ->
    {error, not_a_binary}.

to_float(<<C,Bin/binary>>,N) when C >= $0, C =< $9 ->
    to_float(Bin, N*10+(C-$0));
to_float(<<$.,Bin/binary>>,N) ->
    to_frac(Bin, N, 0, 0.0);
to_float(Rest, N) ->
    {N, Rest}.

to_frac(<<C,Bin/binary>>,N,I,F) when C >= $0, C =< $9 ->
    to_frac(Bin, N,I+1,(C-$0)*10+F);
to_frac(<<Rest/binary>>,N,I,F) ->
    {N + F*math:pow(10, -(I+1)), Rest}.


trim(Bin) ->
    trim_eol(trim_bol(Bin)).

trim_bol(<<$\s,Bin/binary>>) -> trim_bol(Bin);
trim_bol(<<$\t,Bin/binary>>) -> trim_bol(Bin);
trim_bol(<<$\n,Bin/binary>>) -> trim_bol(Bin);
trim_bol(<<$\r,Bin/binary>>) -> trim_bol(Bin);
trim_bol(Bin) -> Bin.

trim_eol(<<>>) ->
    <<>>;
trim_eol(Bin) ->
    Sz = byte_size(Bin)-1,
    case Bin of
	<<Bin1:Sz/binary,$\s>> -> trim_eol(Bin1);
	<<Bin1:Sz/binary,$\t>> -> trim_eol(Bin1);
	<<Bin1:Sz/binary,$\n>> -> trim_eol(Bin1);
	<<Bin1:Sz/binary,$\r>> -> trim_eol(Bin1);
	_ -> Bin
    end.

delete_props([Prop|Props], List) ->
    delete_props(Props, proplists:delete(Prop, List));
delete_props([], List) ->
    List.
