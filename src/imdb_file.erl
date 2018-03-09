%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Load imdb file
%%% @end
%%% Created : 26 Feb 2018 by Tony Rogvall <tony@rogvall.se>

-module(imdb_file).

-export([open/1, close/1, read/1, read/2]).
-export([fold/3, fold_fd/3, fold_fd/4]).
-export([fold_fd_record/5, fold_fd_record/6]).

open(File) ->
    file:open(File, [raw, read, binary, compressed,
		     {read_ahead, 1024*128}]).

close(Fd) ->
    file:close(Fd).

read(Fd) ->
    Cp = binary:compile_pattern([<<"\t">>,<<"\n">>]),
    read(Fd, Cp).

read(Fd,Cp) ->
    case file:read_line(Fd) of
	eof -> eof;
	{ok,Line} ->
	    {ok, binary:split(Line,Cp,[global,trim])}
    end.

fold(Fun, Acc, File) ->
    Cp = binary:compile_pattern([<<"\t">>,<<"\n">>]),
    case open(File) of
	{ok,Fd} ->
	    try fold_fd(Fun, Acc, Fd, Cp) of
		Acc1 -> Acc1
	    catch
		error:Reason ->
		    {error,Reason}
	    end;
	Error ->
	    Error
    end.

fold_fd(Fun, Acc, Fd) ->
    Cp = binary:compile_pattern([<<"\t">>,<<"\n">>]),
    fold_fd(Fun, Acc, Fd, Cp).

fold_fd(Fun, Acc, Fd, Cp) ->
    case read(Fd,Cp) of
	eof -> Acc;
	{ok,Fields} -> 
	    fold_fd(Fun, Fun(Fields,Acc), Fd, Cp)
    end.

fold_fd_record(Fun, Acc, Fd, Fs, Rec) ->
    Cp = binary:compile_pattern([<<"\t">>,<<"\n">>]),
    fold_fd_record(Fun, Acc, Fd, Cp, Fs, Rec).

fold_fd_record(Fun, Acc, Fd, Cp, Fs, Rec) ->
    case read(Fd,Cp) of
	eof -> Acc;
	{ok,Fields} ->
	    Rec1 = parse_record(Fields, Fs, Rec),
	    fold_fd_record(Fun, Fun(Rec1,Acc), Fd, Cp, Fs, Rec)
    end.

parse_record([Bin|Bs],[T|Ts],Rec) ->
    Field = imdb:T(Bin),
    parse_record(Bs, Ts, [Field|Rec]);
parse_record([],[],Rs) ->
    Rec = list_to_tuple(lists:reverse(Rs)),
    case element(2,Rec) of  %% check if key is a auto structured key
	{I,J} when is_integer(I), is_integer(J) ->
	    A = element(I,Rec),
	    B = element(J,Rec),
	    setelement(2, Rec, {A,B});
	_ ->
	    Rec
    end.
