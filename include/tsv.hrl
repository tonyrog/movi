-ifndef(__TSVDB_HRL__).
-define(__TSVDB_HRL__, true).

-define(TSVDB_MAGIC, 16#718EC2F1).

-define(TSV_INDEX_FIXED,   1).
-define(TSV_INDEX_DYNAMIC, 2).
-define(TSV_INDEX_TREE,    3).
-define(TSV_INDEX_HASH,    4).

%% type  <match>:8 <tag>:8 <struct type>:4 <size bits>:4 <basic type>:8
%% basic types
-define(TSV_TYPE_BOOLEAN,  16#00000001).
-define(TSV_TYPE_INT,      16#00000002).
-define(TSV_TYPE_FLOAT,    16#00000003).
-define(TSV_TYPE_STRING,   16#00000004).
-define(TSV_TYPE_BINARY,   16#00000005).
-define(TSV_TYPE_ATOM,     16#00000006).
-define(TSV_TYPE_SCALAR,   16#00000000).
-define(TSV_TYPE_LIST,     16#00001000).
-define(TSV_TYPE_ARRAY,    16#00002000).
-define(TSV_SCALAR8,       16#00000300).
-define(TSV_SCALAR16,      16#00000400).
-define(TSV_SCALAR32,      16#00000500).
-define(TSV_SCALAR64,      16#00000600).
-define(TSV_LENGTH8,       16#00004000).
-define(TSV_LENGTH16,      16#00008000).
-define(TSV_LENGTH32,      16#0000C000).
-define(TSV_TYPE_MASK,     16#000000ff).
-define(TSV_SCALAR_MASK,   16#00000fff).
-define(TSV_SCALARLEN_MASK,16#00000f00).
-define(TSV_STRUCT_MASK,   16#00003000).  %% scalar=0,list=1,array=2
-define(TSV_STRUCTLEN_MASK,16#0000C000).  %% 4=8, 8=16, c=32

%% combined types
-define(TSV_TYPE_INT8,     (?TSV_TYPE_INT bor ?TSV_SCALAR8)).
-define(TSV_TYPE_INT16,    (?TSV_TYPE_INT bor ?TSV_SCALAR16)).
-define(TSV_TYPE_INT32,    (?TSV_TYPE_INT bor ?TSV_SCALAR32)).
-define(TSV_TYPE_INT64,    (?TSV_TYPE_INT bor ?TSV_SCALAR64)).
-define(TSV_TYPE_FLOAT32,  (?TSV_TYPE_FLOAT bor ?TSV_SCALAR32)).
-define(TSV_TYPE_FLOAT64,  (?TSV_TYPE_FLOAT bor ?TSV_SCALAR64)).
-define(TSV_TYPE_STRING8,  (?TSV_TYPE_STRING bor ?TSV_SCALAR8)).
-define(TSV_TYPE_STRING16, (?TSV_TYPE_STRING bor ?TSV_SCALAR16)).
-define(TSV_TYPE_STRING32, (?TSV_TYPE_STRING bor ?TSV_SCALAR32)).
-define(TSV_TYPE_BINARY8,  (?TSV_TYPE_BINARY bor ?TSV_SCALAR8)).
-define(TSV_TYPE_BINARY16, (?TSV_TYPE_BINARY bor ?TSV_SCALAR16)).
-define(TSV_TYPE_BINARY32, (?TSV_TYPE_BINARY bor ?TSV_SCALAR32)).
-define(TSV_TYPE_ATOM8,    (?TSV_TYPE_ATOM bor ?TSV_SCALAR8)).
-define(TSV_TYPE_ATOM16,   (?TSV_TYPE_ATOM bor ?TSV_SCALAR16)).
-define(TSV_TYPE_LIST8,    (?TSV_TYPE_LIST bor ?TSV_LENGTH8)).
-define(TSV_TYPE_LIST16,   (?TSV_TYPE_LIST bor ?TSV_LENGTH16)).
-define(TSV_TYPE_LIST32,   (?TSV_TYPE_LIST bor ?TSV_LENGTH32)).
-define(TSV_TYPE_ARRAY8,   (?TSV_TYPE_ARRAY bor ?TSV_LENGTH8)).
-define(TSV_TYPE_ARRAY16,  (?TSV_TYPE_ARRAY bor ?TSV_LENGTH16)).
-define(TSV_TYPE_ARRAY32,  (?TSV_TYPE_ARRAY bor ?TSV_LENGTH32)).
-define(TSV_TYPE_TAGGED(A,B,T), (((A) bsl 24) bor ((B) bsl 16) bor (T))).

%% return base type and struct, strip all lengths
-define(TSV_STRUCT_TYPE(T), (T band (?TSV_TYPE_MASK bor ?TSV_STRUCT_MASK))).
-define(TSV_IS_ATOM(T), (?TSV_STRUCT_TYPE(T) =:= ?TSV_TYPE_ATOM)).
-define(TSV_IS_INT(T), (?TSV_STRUCT_TYPE(T) =:= ?TSV_TYPE_INT)).
-define(TSV_IS_FLOAT(T), (?TSV_STRUCT_TYPE(T) =:= ?TSV_TYPE_FLOAT)).

-type uint8()  :: 0..16#ff.
-type uint16() :: 0..16#ffff.
-type uint32() :: 0..16#ffffffff.
-type uint64() :: 0..16#ffffffffffffffff.

-record(tsv_field,
	{
	 name :: binary(),
	 type :: uint32(),   %% :uint32  ?TSV_TYPE_xxxx
	 default :: term(),  %% default value to encode (when undefined)
	 parser  :: map() | function(),  %% value parser
	 index_type :: uint32(),     %% ?TSV_INDEX_xxxx
	 index_offset :: uint64()    %%
	}).

-record(tsv_table,
	{
	 name :: atom(),
	 %% what field(s) is used as primare index
	 primary ::  integer() | [integer()],
	 fields = [] :: #tsv_field{}
	}).

-endif.
