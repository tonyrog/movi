%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    read/write tsv style database
%%% @end
%%% Created : 20 Jan 2019 by Tony Rogvall <tony@rogvall.se>

-module(tsvdb).

-include("../include/tsv.hrl").

-export([open_rec/2, open_pri/2]).
-export([close/1]).

-export([read_record/2]).
-export([write_record/3]).

-export([write_table_header/3]).
-export([write_field_header/3]).
-export([encode_field_header/2]).

-export([type_bytes/1, record_bytes/1, is_fixed/1]).
-export([encode_type/1, decode_type/1]).
-export([decode_record/2, decode_record_fields/2,
	 decode_field/2, decode_typed_field/2, decode_scalar/2]).
-export([encode_record/2, encode_record_fields/2,
	 encode_field/2, encode_typed_field/2, encode_scalar/2]).

-export([test_type_codec/0]).
-compile(export_all).

%% open a .rec, .pri or .sec for reading

open_rec(File, read) ->
    FileName = filename:rootname(File)++".rec",
    file:open(FileName, [raw, read, binary, {read_ahead,64*1024}]);
open_rec(File, write) ->
    FileName = filename:rootname(File)++".rec",
    file:open(FileName, [raw, write, {delayed_write,1024*1024,20000}]).

open_pri(File, read) ->
    FileName = filename:rootname(File)++".pri",
    file:open(FileName, [raw, read, binary, {read_ahead,64*1024}]);
open_pri(File, write) ->
    FileName = filename:rootname(File)++".pri",
    file:open(FileName, [raw, write, binary, delayed_write]).

%% using "." in filenames present an interesting problem
rootname(Filename) ->
    case filename:dirname(Filename) of
	"." when hd(Filename) =/= $. ->
	    rootname_(Filename);
	Dir ->
	    Base = filename:basename(Filename),
	    filename:join(Dir, rootname_(Base))
    end.

%% maybe strip both tsv and tsv.gz?
rootname_(Filename) ->
    case filename:extension(Filename) of
	".tsv" -> filename:basename(Filename, ".tsv");
	".rec" -> filename:basename(Filename, ".rec");
	".pri" -> filename:basename(Filename, ".pri");
	Ext ->
	    N = length(Ext),
	    if N =< 4 -> filename:basename(Filename, Ext);
	       true -> Filename
	    end
    end.

close(Fd) ->
    file:close(Fd).

write_table_header(Fd, Table, OffsetList) when is_record(Table, tsv_table) ->
    write_table_header_(Fd, Table#tsv_table.fields, OffsetList);
write_table_header(Fd, Fields, OffsetList) when is_list(Fields) ->
    write_table_header_(Fd, Fields, OffsetList).

write_table_header_(Fd,[F|Fs],[Offs|OffsList]) ->
    write_field_header(Fd,F,Offs),
    write_table_header_(Fd,Fs,OffsList);
write_table_header_(_Fd,[],[]) ->
    ok.

%% emit binary (readonly?) database 
write_field_header(Fd,Field,IndexOffset) ->
    file:write(Fd, encode_field_header(Field,IndexOffset)).

%% <<Size:32, Type:32, IndexType:32, IndexOffset:64, Name/binary>>
%% NameSize = DescSize - (4+4+8)
encode_field_header(#tsv_field{name=Name,type=Type,index_type=IndexType},
		    IndexOffset) ->
    Size = byte_size(Name) + (4+4+8),
    <<Size:32, Type:32, IndexType:32, IndexOffset:64, Name/binary>>.

%% field must be binary encoded according to type
    %% using file position slow down writing 3 times!
write_record(Fd, Table, Record) ->
    Data = encode_record(Table,Record),
    Len = byte_size(Data),
    CRC = erlang:crc32([<<Len:32>>, Data]),
    ok = file:write(Fd,<<Len:32, Data/binary, CRC:32>>),
    {ok,byte_size(Data)+8}.

read_record(Fd, Table) ->
    {ok,L=(<<Len:32>>)} = file:read(Fd, 4),
    {ok,Data} = file:read(Fd, Len),
    CRC = erlang:crc32([L,Data]),
    case file:read(Fd, 4) of
	{ok,<<CRC:32>>} ->
	    Fs = decode_record(Table#tsv_table.fields,Data),
	    {ok,list_to_tuple([Table#tsv_table.name|Fs])};
	{ok,_} ->
	    {error, bad_crc};
	Error ->
	    Error
    end.

encode_record(Table, Record) when is_record(Table, tsv_table) ->
    iolist_to_binary(encode_record_fields(Table#tsv_table.fields,
					  tl(tuple_to_list(Record))));
encode_record(Fields, Record) when is_list(Fields) ->
    iolist_to_binary(encode_record_fields(Fields,
					  tl(tuple_to_list(Record)))).


decode_record(Table, Buffer) when is_record(Table, tsv_table) ->
    decode_record_fields(Table#tsv_table.fields, Buffer);
decode_record(Fields, Buffer) when is_list(Fields) ->
    decode_record_fields(Fields, Buffer).

decode_record_fields([Tsv|TsvL], Bin) ->
    {V,Bin1} = decode_field(Tsv, Bin),
    [V | decode_record_fields(TsvL, Bin1)];
decode_record_fields([], <<>>) ->
    [].

decode_field(#tsv_field{type=T,default=D}, Bin) ->
    case decode_typed_field(T, Bin) of
	{D,Bin1} -> {undefined,Bin1};
	Res={_,_Bin1} -> Res
    end.

decode_typed_field(T, Bin)  when is_integer(T) ->
    case T band ?TSV_STRUCT_MASK  of
	?TSV_TYPE_SCALAR ->
	    decode_scalar(T, Bin);
	?TSV_TYPE_LIST ->
	    case T band ?TSV_STRUCTLEN_MASK of
		?TSV_LENGTH8  -> decode_list(8,T,Bin);
		?TSV_LENGTH16 -> decode_list(16,T,Bin);
		?TSV_LENGTH32 -> decode_list(32,T,Bin)
	    end;
	?TSV_TYPE_ARRAY ->
	    case T band ?TSV_STRUCTLEN_MASK of
		?TSV_LENGTH8 -> decode_array(8,T,Bin);
		?TSV_LENGTH16 -> decode_array(16,T,Bin);
		?TSV_LENGTH32 -> decode_array(32,T,Bin)
	    end
    end;
decode_typed_field(TypeTerm, Bin) -> 
    decode_typed_field(encode_type(TypeTerm),Bin).


decode_list(Size, T, Bin) ->
    <<N:Size,Bin1/binary>> = Bin,
    decode_elems_(N, T, Bin1, []).

decode_array(Size, T, Bin) ->
    <<N:Size,Bin1/binary>> = Bin,
    {List,Bin1} = decode_elems_(N, T, Bin1, []),
    {list_to_tuple(List), Bin1}.

decode_elems_(0, _T, Bin, Acc) ->
    {lists:reverse(Acc),Bin};
decode_elems_(I, T, Bin, Acc) ->
    {E, Bin1} = decode_scalar(T, Bin),
    decode_elems_(I-1, T, Bin1, [E|Acc]).

decode_scalar(T, D) ->
    case T band ?TSV_SCALAR_MASK of
	?TSV_TYPE_BOOLEAN -> 
	    case D of
		<<1:8, D1/binary>> -> {true,D1};
		<<0:8, D1/binary>> -> {false,D1};
		<<2:8, D1/binary>> -> {undefined,D1}
	    end;
	?TSV_TYPE_INT8 ->
	    case D of
		<<X:8/signed, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_INT16 ->
	    case D of
		<<X:16/signed, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_INT32 ->
	    case D of
		<<X:32/signed, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_INT64 ->
	    case D of
		<<X:64/signed, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_FLOAT32 ->
	    case D of
		<<X:32/float, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_FLOAT64 ->
	    case D of
		<<X:64/float, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_STRING8 ->
	    case D of
		<<L:8, X:L/binary, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_STRING16 ->
	    case D of
		<<L:16, X:L/binary, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_STRING32 ->
	    case D of
		<<L:32, X:L/binary, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_BINARY8 ->
	    case D of
		<<L:8, X:L/binary, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_BINARY16 ->
	    case D of
		<<L:16, X:L/binary, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_BINARY32 ->
	    case D of
		<<L:32, X:L/binary, D1/binary>> -> {X,D1}
	    end;
	?TSV_TYPE_ATOM8 ->
	    case D of
		<<L:8, X:L/binary, D1/binary>> -> 
		    {erlang:binary_to_atom(X,utf8),D1}
	    end;
	?TSV_TYPE_ATOM16 ->
	    case D of
		<<L:16, X:L/binary, D1/binary>> ->
		    {erlang:binary_to_atom(X,utf8),D1}
	    end
    end.

encode_record_fields([Tsv|TsvL],[V|Vs]) ->
    [encode_field(Tsv, V) | encode_record_fields(TsvL, Vs)];
encode_record_fields([],[]) ->
    [].


encode_field(F=#tsv_field{type=T,default=D},undefined) 
  when D =/= undefined; ?TSV_IS_ATOM(T) ->
    encode_field(F, D);
encode_field(#tsv_field{type=T}, Value) ->
    encode_typed_field(T, Value).

encode_typed_field(T, Value) when is_integer(T) ->
    case T band ?TSV_STRUCT_MASK  of
	?TSV_TYPE_SCALAR ->
	    encode_scalar(T, Value);
	?TSV_TYPE_LIST ->
	    N = length(Value),
	    case T band ?TSV_STRUCTLEN_MASK of
		?TSV_LENGTH8 ->
		    [<<N:8>> | [encode_scalar(T,E) || E <- Value]];
		?TSV_LENGTH16 ->
		    [<<N:16>> | [encode_scalar(T,E) || E <- Value]];
		?TSV_LENGTH32 ->
		    [<<N:32>> | [encode_scalar(T,E) || E <- Value]]
	    end;
	?TSV_TYPE_ARRAY ->
	    N = tuple_size(Value),
	    Vs = tuple_to_list(Value),
	    case T band ?TSV_STRUCTLEN_MASK of
		?TSV_LENGTH8 ->
		    [<<N:8>> | [encode_scalar(T,E) || E <- Vs]];
		?TSV_LENGTH16 ->
		    [<<N:16>> | [encode_scalar(T,E) || E <- Vs]];
		?TSV_LENGTH32 ->
		    [<<N:32>> | [encode_scalar(T,E) || E <- Vs]]
	    end
    end;
encode_typed_field(TypeTerm, Value) ->
    encode_typed_field(encode_type(TypeTerm), Value).


encode_scalar(T, Data) ->
    case T band ?TSV_SCALAR_MASK of
	?TSV_TYPE_BOOLEAN -> 
	    case Data of
		true -> <<1:8>>;
		false -> <<0:8>>;
		undefined -> <<2:8>>
	    end;
	?TSV_TYPE_INT8 -> <<Data:8>>;
	?TSV_TYPE_INT16 -> <<Data:16>>;
	?TSV_TYPE_INT32 -> <<Data:32>>;
	?TSV_TYPE_INT64 -> <<Data:64>>;
	?TSV_TYPE_FLOAT32  -> <<Data:32/float>>;
	?TSV_TYPE_FLOAT64  -> <<Data:64/float>>;
	?TSV_TYPE_STRING8  -> encode_len8(string_to_binary(Data,utf8));
	?TSV_TYPE_STRING16 -> encode_len16(string_to_binary(Data,utf8));
	?TSV_TYPE_STRING32 -> encode_len32(string_to_binary(Data,utf8));
	?TSV_TYPE_BINARY8  -> encode_len8(Data);
	?TSV_TYPE_BINARY16 -> encode_len16(Data);
	?TSV_TYPE_BINARY32 -> encode_len32(Data);
	?TSV_TYPE_ATOM8 -> encode_len8(atom_to_binary(Data, utf8));
	?TSV_TYPE_ATOM16 -> encode_len16(atom_to_binary(Data, utf8))
    end.

string_to_binary(Bin, _Encoding) when is_binary(Bin) ->
    Bin;
string_to_binary(String, Encoding) when is_list(String) ->
    unicode:characters_to_binary(String, Encoding).


encode_len8(Data) -> <<(byte_size(Data)):8, Data/binary>>.
encode_len16(Data) -> <<(byte_size(Data)):16, Data/binary>>.
encode_len32(Data) -> <<(byte_size(Data)):32, Data/binary>>.

%% calculate byte size of record
%% return 0 if contains dynamic field Size otherwise

is_fixed(FieldsOrTable) ->
    record_bytes(FieldsOrTable) > 0.

record_bytes(Table) when is_record(Table, tsv_table) ->
    record_bytes_(Table#tsv_table.fields, 0);
record_bytes(Fields) when is_list(Fields) ->
    record_bytes_(Fields, 0).

record_bytes_([F|Fs],Sum) ->
    case type_bytes(F#tsv_field.type) of
	0 -> 0;  %% dynamic
	N -> record_bytes_(Fs,Sum+N)
    end;
record_bytes_([],Sum) -> Sum.

%% return number of bytes to encde a type, either in
%% integer or symbolic form
type_bytes(T) when is_integer(T) ->
    if T band ?TSV_STRUCT_MASK =:= ?TSV_TYPE_SCALAR ->
	    case T band ?TSV_SCALAR_MASK of
		?TSV_TYPE_BOOLEAN -> 1;
		?TSV_TYPE_INT8  -> 1;
		?TSV_TYPE_INT16 -> 2;
		?TSV_TYPE_INT32 -> 4;
		?TSV_TYPE_INT64 -> 8;
		?TSV_TYPE_FLOAT32 -> 4;
		?TSV_TYPE_FLOAT64 -> 8;
		_ -> 0
	    end;
       true -> 0
    end;
type_bytes(T) ->
    type_bytes(encode_type(T)).


%% encode symbolic type to uint32 
encode_type(boolean) -> ?TSV_TYPE_BOOLEAN;
encode_type(int) ->    ?TSV_TYPE_INT;
encode_type(float) ->  ?TSV_TYPE_FLOAT;
encode_type(string) -> ?TSV_TYPE_STRING;
encode_type(binary) -> ?TSV_TYPE_BINARY;
encode_type(atom) ->  ?TSV_TYPE_ATOM;
encode_type({int,Bits}) ->
    Len = (trunc(math:log2(Bits)) band 16#f),
    ?TSV_TYPE_INT bor (Len bsl 8);
encode_type({float,Bits}) ->
    Len = (trunc(math:log2(Bits)) band 16#f),
    ?TSV_TYPE_FLOAT bor (Len bsl 8);
encode_type({string,Bits}) ->  %% size of length (max 64)
    Len = (trunc(math:log2(Bits)) band 16#f),
    ?TSV_TYPE_STRING bor (Len bsl 8);
encode_type({binary,Bits}) ->  %% size of length (max 64)
    Len = (trunc(math:log2(Bits)) band 16#f),
    ?TSV_TYPE_BINARY bor (Len bsl 8);
encode_type({atom,Bits}) ->  %% size of length field (max 16)
    Len = (trunc(math:log2(Bits)) band 16#f),
    ?TSV_TYPE_ATOM bor (Len bsl 8);
encode_type({list,Type}) ->
    ?TSV_TYPE_LIST bor encode_type(Type);
encode_type({list8,Type}) ->
    ?TSV_TYPE_LIST bor ?TSV_LENGTH8 bor encode_type(Type);
encode_type({list16,Type}) ->
    ?TSV_TYPE_LIST bor ?TSV_LENGTH16 bor encode_type(Type);
encode_type({list32,Type}) ->
    ?TSV_TYPE_LIST bor ?TSV_LENGTH32 bor encode_type(Type);
encode_type({array,Type}) ->
    ?TSV_TYPE_ARRAY bor encode_type(Type);
encode_type({array8,Type}) ->
    ?TSV_TYPE_ARRAY bor ?TSV_LENGTH8 bor encode_type(Type);
encode_type({array16,Type}) ->
    ?TSV_TYPE_ARRAY bor ?TSV_LENGTH16 bor encode_type(Type);
encode_type({array32,Type}) ->
    ?TSV_TYPE_ARRAY bor ?TSV_LENGTH32 bor encode_type(Type);
encode_type({tag,{[A,B],Type}}) ->
    ?TSV_TYPE_TAGGED(A,B,encode_type(Type)).

decode_type(Type) ->
    case (Type bsr 16) of
	0 -> decode_type_(Type);
	Tag ->
	    A = (Tag bsr 8) band 16#ff,
	    B = (Tag bsr 0) band 16#ff,
	    {tag,{[A,B],decode_type_(Type)}}
    end.

decode_type_(Type) ->
    case Type band ?TSV_STRUCT_MASK  of
	?TSV_TYPE_SCALAR ->
	    decode_scalar_type(Type);
	?TSV_TYPE_LIST ->
	    case (Type band ?TSV_STRUCTLEN_MASK) bsr 14 of
		0 -> {list, decode_scalar_type(Type)};
		1 -> {list8, decode_scalar_type(Type)};
		2 -> {list16, decode_scalar_type(Type)};
		3 -> {list32, decode_scalar_type(Type)}
	    end;
	?TSV_TYPE_ARRAY ->
	    case (Type band ?TSV_STRUCTLEN_MASK) bsr 14 of
		0 -> {array,   decode_scalar_type(Type)};
		1 -> {array8,  decode_scalar_type(Type)};
		2 -> {array16, decode_scalar_type(Type)};
		3 -> {array32, decode_scalar_type(Type)}
	    end
    end.    

decode_scalar_type(Type) ->
    BitLen = ((Type band ?TSV_SCALARLEN_MASK) bsr 8),
    Size = if BitLen =:= 0 -> 0; true -> (1 bsl BitLen) end,
    case Type band ?TSV_TYPE_MASK of
	?TSV_TYPE_BOOLEAN -> boolean;
	?TSV_TYPE_INT   -> if Size =:= 0 -> int; true -> {int,Size} end;
	?TSV_TYPE_FLOAT -> if Size =:= 0 -> float; true -> {float,Size} end;
	?TSV_TYPE_STRING -> if Size =:= 0 -> string; true -> {string,Size} end;
	?TSV_TYPE_BINARY -> if Size =:= 0 -> binary; true -> {binary,Size} end;
	?TSV_TYPE_ATOM -> if Size =:= 0 -> atom; true -> {atom,Size} end
    end.

test_type_codec() ->
    NumericTypes = [int,float],
    StringTypes  = [string,binary,atom],
    BasicTypes = [boolean] ++ NumericTypes ++ StringTypes,
    BasicTypes1 = 
	[{int,Size} || Size <- [8,16,32,64]] ++
	[{float,Size} || Size <- [8,16,32,64]] ++
	[{string,Size} || Size <- [8,16,32]] ++
	[{binary,Size} || Size <- [8,16,32]] ++
	[{atom,Size} || Size <- [8,16]],
	
    [test_type(T) || T <- BasicTypes],
    [test_type({list,T}) || T <- BasicTypes],
    [test_type({list8,T}) || T <- BasicTypes],
    [test_type({list16,T}) || T <- BasicTypes],
    [test_type({list32,T}) || T <- BasicTypes],
    [test_type({array,T}) || T <- BasicTypes],
    [test_type({array8,T}) || T <- BasicTypes],
    [test_type({array16,T}) || T <- BasicTypes],
    [test_type({array32,T}) || T <- BasicTypes],

    [test_type(T) || T <- BasicTypes1],
    [test_type({list,T}) || T <- BasicTypes1],
    [test_type({list8,T}) || T <- BasicTypes1],
    [test_type({list16,T}) || T <- BasicTypes1],
    [test_type({list32,T}) || T <- BasicTypes1],
    [test_type({array,T}) || T <- BasicTypes1],
    [test_type({array8,T}) || T <- BasicTypes1],
    [test_type({array16,T}) || T <- BasicTypes1],
    [test_type({array32,T}) || T <- BasicTypes1],

    [test_type({tag,{Tag,{int,64}}}) || 
	Tag <- ["tt","nm","uu","gg"]],
    [test_type({tag,{Tag,{list16,{atom,8}}}}) || 
	Tag <- ["tt","nm","uu","gg"]],

    ok.

test_type(Type) ->
    E = encode_type(Type),
    Type = decode_type(E).
