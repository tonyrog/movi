%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Load imdb tsv file
%%% @end
%%% Created : 26 Feb 2018 by Tony Rogvall <tony@rogvall.se>

-module(tsv).

-export([open/2, close/1, read/1, read/2]).
-export([fold/3, fold_fd/3, fold_fd/4]).
-export([fold_fd_record/5, fold_fd_record/6]).

-compile(export_all).

-include("../include/tsv.hrl").

open(File, read) ->
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
    case open(File, read) of
	{ok,Fd} ->
	    try fold_fd(Fun, Acc, Fd, Cp) of
		Acc1 -> Acc1
	    catch
		error:Reason:Stack ->
		    {error,Reason,Stack}
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

fold_fd_record(Fun, Acc, Fd, Fs, RecName) ->
    Cp = binary:compile_pattern([<<"\t">>,<<"\n">>]),
    fold_fd_record(Fun, Acc, Fd, Cp, Fs, RecName).

fold_fd_record(Fun, Acc, Fd, Cp, Fs, RecName) ->
    case read(Fd,Cp) of
	eof -> Acc;
	{ok,Fields} ->
	    Rec1 = parse_record(Fields, Fs, [RecName]),
	    try Fun(Rec1,Acc) of
		Acc1 ->
		    fold_fd_record(Fun, Acc1, Fd, Cp, Fs, RecName)
	    catch
		throw:stop ->
		    Acc
	    end
    end.

-define(UNDEF, <<"\\N">>).

parse_record([Bin|Bs],[Fld|Flds],Rs) ->
    Field = parse_field(Bin, Fld),
    parse_record(Bs, Flds, [Field|Rs]);
parse_record([],[],Rs) ->
    list_to_tuple(lists:reverse(Rs)).

parse_field(?UNDEF, _Type) -> undefined;
parse_field(Bin, #tsv_field{type=Type,parser=Df}) ->
    parse_field_(Bin,Type,Df).

parse_field_(Bin,Type,Df) ->
    case Type band ?TSV_STRUCT_MASK  of
	?TSV_TYPE_SCALAR ->
	    parse_scalar(Bin,Type,Df);
	?TSV_TYPE_LIST ->
	    Type1 = Type band (bnot (?TSV_STRUCT_MASK bor ?TSV_STRUCTLEN_MASK)),
	    parse_list(Bin,Type1,Df);
	?TSV_TYPE_ARRAY ->
	    Type1 = Type band (bnot (?TSV_STRUCT_MASK bor ?TSV_STRUCTLEN_MASK)),
	    parse_array(Bin,Type1,Df)
    end.

parse_scalar(Bin0,Type,Df) ->
    A = (Type bsr 24) band 16#ff,
    B = (Type bsr 16) band 16#ff,
    Bin = if A =/= 0 ->
		  case Bin0 of
		      <<A,B,Bin1/binary>> ->  Bin1
		  end;
	     true ->
		  Bin0
	  end,
    case Type band ?TSV_TYPE_MASK of
	?TSV_TYPE_BOOLEAN ->
	    case Bin of
		<<"0">> -> false;
		<<"1">> -> true
	    end;
	?TSV_TYPE_INT ->
	    try binary_to_integer(Bin) of
		I -> I
	    catch
		error:_ ->
		    parse_value(Bin, Df)
	    end;
	?TSV_TYPE_FLOAT ->
	    try binary_to_float(Bin) of
		F -> F
	    catch
		error:_ ->
		    try binary_to_integer(Bin) of
			I -> erlang:float(I)
		    catch
			error:_ ->
			    parse_value(Bin, Df)
		    end
	    end;
	?TSV_TYPE_STRING ->
	    Bin;
	?TSV_TYPE_BINARY ->
	    Bin;
	?TSV_TYPE_ATOM ->
	    binary_to_atom(string:lowercase(Bin), utf8)
    end.

parse_value(Bin,Df) when is_map(Df) ->
    case maps:find(Bin, Df) of
	{ok,Value} -> Value
    end;
parse_value(Bin,Df) when is_function(Df) ->
    Df(Bin).
    
parse_list(Bin,Type,Df) ->
    Bs = binary:split(Bin,<<",">>,[global]),
    [parse_scalar(B,Type,Df) || B <- Bs].
    
parse_array(Bin,Type,Df) ->
    {ok,Ts,_} = erl_scan:string(binary_to_list(Bin)),
    match_array(Ts,Type band ?TSV_TYPE_MASK,Df).

match_array([{'[',_},{']',_}],_T,_Df) ->
    [];
match_array([{'[',_},Elem|Ts],T,Df) ->
    [match_elem(Elem,T,Df) | match_array1(Ts,T,Df)].

match_array1([{',',_},Elem|Ts],T,Df) ->
    [match_elem(Elem,T,Df) | match_array1(Ts,T,Df)];
match_array1([{']',_}],_T,_Df) ->
    [].

match_elem({string,_,S},?TSV_TYPE_STRING,_Df) -> S;
match_elem({binary,_,B},?TSV_TYPE_BINARY,_Df) -> B;
match_elem({integer,_,I},?TSV_TYPE_INT,_Df) -> I;
match_elem({float,_,F},?TSV_TYPE_FLOAT,_Df) -> F;
match_elem({atom,_,A},?TSV_TYPE_ATOM,_Df) -> A.
