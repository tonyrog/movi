%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Load imdb tsv file
%%% @end
%%% Created : 26 Feb 2018 by Tony Rogvall <tony@rogvall.se>

-module(tsv).

-export([open/2, close/1, read/1, read/2, write/2]).
-export([fold/3, fold_fd/3, fold_fd/4]).
-export([fold_fd_record/5, fold_fd_record/6]).

-compile(export_all).

-include("../include/tsv.hrl").

open(File, read) ->
    file:open(File, [raw, read, binary, compressed, {read_ahead, 1024*128}]);
open(File, write) ->
    FileName = filename:rootname(File)++".tsv",
    file:open(FileName, [raw, write, binary, delayed_write]).

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

write(Fd, Elems) ->
    file:write(Fd, [lists:join($\t,Elems),$\n]).

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
    Bin = untag_value(Bin0, Type),
    parse_scalar_(Bin,Type,Df).

parse_scalar_(Bin,_Type,Df) when is_map(Df) ->
    case maps:find(Bin, Df) of
	{ok,Value} -> Value
    end;
parse_scalar_(Bin,_Type,Df) when is_function(Df) ->
    Df(Bin);
parse_scalar_(Bin,Type,undefined) ->
    case Type band ?TSV_TYPE_MASK of
	?TSV_TYPE_BOOLEAN ->
	    case Bin of
		<<"0">> -> false;
		<<"1">> -> true
	    end;
	?TSV_TYPE_INT -> binary_to_integer(Bin);
	?TSV_TYPE_FLOAT ->
	    try binary_to_float(Bin) of
		F -> F
	    catch
		error:_ ->
		    erlang:float(binary_to_integer(Bin))
	    end;
	?TSV_TYPE_STRING -> Bin;
	?TSV_TYPE_BINARY -> Bin;
	?TSV_TYPE_ATOM -> binary_to_atom(string:lowercase(Bin), utf8)
    end.

untag_value(Bin, Type) ->
    A = (Type bsr 24) band 16#ff,
    if A =/= 0 ->
	    B = (Type bsr 16) band 16#ff,
	    case Bin of
		<<A,B,Bin1/binary>> ->  Bin1
	    end;
       true ->
	    Bin
    end.

parse_list(Bin,Type,Df) ->
    Bs = binary:split(Bin,<<",">>,[global]),
    [parse_scalar(B,Type,Df) || B <- Bs].
    
parse_array(Bin,Type,Df) ->
    {ok,Ts,_} = erl_scan:string(binary_to_list(Bin)),
    list_to_tuple(match_array(Ts,Type band ?TSV_TYPE_MASK,Df)).

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


format_record(Record, Fields) ->
    Elems = tl(tuple_to_list(Record)),
    format_fields_(Elems, Fields).

format_fields_([E|Elems], [Fld|Flds]) ->
    [format_field(E, Fld) | format_fields_(Elems,Flds)];
format_fields_([], []) ->
    [].

format_field(undefined, _) -> ?UNDEF;
format_field(Elem, #tsv_field{type=Type,formatter=Ff}) ->
    format_field_(Elem, Type, Ff).

format_field_(Elem, Type, Ff) ->
    case Type band ?TSV_STRUCT_MASK  of
	?TSV_TYPE_SCALAR -> format_scalar(Elem,Type,Ff);
	?TSV_TYPE_LIST ->
	    Type1 = Type band (bnot (?TSV_STRUCT_MASK bor ?TSV_STRUCTLEN_MASK)),
	    format_list(Elem,Type1,Ff);
	?TSV_TYPE_ARRAY ->
	    Type1 = Type band (bnot (?TSV_STRUCT_MASK bor ?TSV_STRUCTLEN_MASK)),
	    format_array(Elem,Type1,Ff)
    end.

format_scalar(Elem,Type,Ff) ->
    A = (Type bsr 24) band 16#ff,
    B = (Type bsr 16) band 16#ff,
    if A =/= 0 ->
	    B = (Type bsr 16) band 16#ff,
	    Bin = format_scalar_(Elem, Type, Ff, true),
	    <<A,B,Bin/binary>>;
       true ->
	    format_scalar_(Elem, Type, Ff, false)
    end.

format_scalar_(Elem,_Type,Ff,_Tagged) when is_map(Ff) ->
    case maps:find(Elem, Ff) of
	{ok,Bin} -> Bin
    end;
format_scalar_(Elem,_Type,Ff,_Tagged) when is_function(Ff) ->
    Ff(Elem);
format_scalar_(Elem,Type,undefined,Tagged) ->
    case Type band ?TSV_TYPE_MASK of
	?TSV_TYPE_BOOLEAN ->
	    case Elem of
		false -> <<"0">>;
		true  -> <<"1">>
	    end;
	?TSV_TYPE_INT ->
	    Int = integer_to_binary(Elem),
	    if Tagged ->
		    N = byte_size(Int),
		    if N < 7 ->  %% pad with zeros
			    Zs = list_to_binary(lists:duplicate(7-N, $0)),
			    <<Zs/binary, Int/binary>>;
		       true ->
			    Int
		    end;
	       true ->
		    Int
	    end;
	?TSV_TYPE_FLOAT -> 
	    %% fixme, how to cope with 32/64 bit float conv
	    %% io_lib_format:fwrite_g(Elem)
	    list_to_binary(io_lib:format("~.1f", [Elem]));
	?TSV_TYPE_STRING when is_binary(Elem) -> Elem;
	?TSV_TYPE_BINARY when is_binary(Elem) -> Elem;
	?TSV_TYPE_ATOM -> atom_to_binary(Elem,utf8)
    end.

format_list(Elems,Type,Df) ->
    Es = [format_scalar(E,Type,Df) || E <- Elems],
    lists:join($,, Es).
    
format_array(Elems,Type,Df) ->
    Es = [format_scalar(E,Type,Df) || E <- tuple_to_list(Elems)],
    [$[,lists:join($,, Es),$]].
