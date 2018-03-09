%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    basic record element decode / encode
%%% @end
%%% Created : 27 Feb 2018 by Tony Rogvall <tony@rogvall.se>

-module(imdb).
-compile(export_all).

-include("../include/imdb.hrl").

%% Table attributes and settings

%% #name_basics{} db information
name_basics(db)   -> bdb;
name_basics(record) -> [name_basics];
name_basics(tsv_fields) ->
    [<<"nconst">>,<<"primaryName">>,<<"birthYear">>,
     <<"deathYear">>,<<"primaryProfession">>,
     <<"knownForTitles">>];
name_basics(types) ->
    [nconst,string,year,year,atom_list,tconst_list];
name_basics(tsv_file) ->
    filename:join(code:priv_dir(movi), "name.basics.tsv.gz");
name_basics(db_file) ->
    %% filename:join(code:priv_dir(movi), "name.basics.dets").
    "name.basics.bdb".


title_basics(db)   -> dets;
title_basics(record) -> [title_basics];
title_basics(tsv_fields) ->
    [<<"tconst">>,<<"titleType">>,<<"primaryTitle">>,
     <<"originalTitle">>,<<"isAdult">>,<<"startYear">>,
     <<"endYear">>,<<"runtimeMinutes">>,<<"genres">>];
title_basics(types) ->
    [tconst, atom, string, string, boolean,
     year, year, integer, atom_list];
title_basics(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.basics.tsv.gz");
title_basics(db_file) ->
    filename:join(code:priv_dir(movi), "title.basics.dets").

title_episode(db) -> dets;
title_episode(record) -> [title_episode];
title_episode(tsv_fields) ->
    [<<"tconst">>,<<"parentTconst">>,<<"seasonNumber">>,<<"episodeNumber">>];
title_episode(types) ->
    [tconst, tconst, integer, integer];
title_episode(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.episode.tsv.gz");
title_episode(db_file) ->
    filename:join(code:priv_dir(movi), "title.episode.dets").

title_crew(db) -> dets;
title_crew(record) -> [title_crew];
title_crew(tsv_fields) ->
    [<<"tconst">>,<<"directors">>,<<"writers">>];
title_crew(types) ->
    [tconst,nconst_list,nconst_list];
title_crew(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.crew.tsv.gz");
title_crew(db_file) ->
    filename:join(code:priv_dir(movi), "title.crew.dets").

title_ratings(db) -> ets;
title_ratings(record) -> [title_ratings];
title_ratings(tsv_fields) ->
    [<<"tconst">>,<<"averageRating">>,<<"numVotes">>];
title_ratings(types) ->
    [tconst, float, integer];
title_ratings(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.ratings.tsv.gz");
title_ratings(db_file) ->
    filename:join(code:priv_dir(movi), "title.ratings.dets").

title_principals(db) -> bdb;
title_principals(record) ->
    %% principals.key = {tconst,ordering}
    [{#title_principals.tconst,#title_principals.ordering},title_principals];
title_principals(tsv_fields) ->
    [<<"tconst">>,<<"ordering">>,<<"nconst">>,
     <<"category">>,<<"job">>,<<"characters">>];
title_principals(types) ->
    [tconst, integer, nconst, category, job, string_array];
title_principals(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.principals.tsv.gz");
title_principals(db_file) ->
    %% filename:join(code:priv_dir(movi), "title.principals.dets").
    "title.principals.bdb".

title_akas(db) -> dets;
title_akas(record) ->
    %% akas.key = {tconst,ordering}
    [{#title_akas.tconst,#title_akas.ordering},title_akas];
title_akas(tsv_fields) ->
    [<<"titleId">>,<<"ordering">>,<<"title">>,
     <<"region">>,<<"language">>,
     <<"types">>,<<"attributes">>,<<"isOriginalTitle">>];
title_akas(types) ->
    [tconst,integer,string,lang,lang,
     string,string,boolean];
title_akas(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.akas.tsv.gz");
title_akas(db_file) ->
    filename:join(code:priv_dir(movi), "title.akas.dets").

%% re-build dets file from the tsv file
import(Name) when is_atom(Name) ->
    open(Name, read_write),
    Count = fold(Name,
		 fun(Rec,N) ->
			 insert(Name, Rec),
			 N+1
		 end, 0),
    close(Name),
    Count.

export(_Name) ->
    %% should write ets/dets to a tsv file!
    ok.

%% setup ets/dets tables
setup(Name) ->
    open(Name, read).

load(Name) ->
    case ?MODULE:Name(db) of
	dets -> dets;
	bdb -> open(Name, read);
	ets ->
	    fold(Name,
		 fun(Rec,N) ->
			 ets:insert(Name, Rec),
			 N+1
		 end, 0)
    end.

repair(Name) ->
    case ?MODULE:Name(db) of
	dets ->
	    DbFile = ?MODULE:Name(db_file),
	    R = dets:open_file(Name, [{file,DbFile},{keypos,2},
				      {repair,true}]),
	    dets:close(Name),
	    R;
	bdb ->
	    {error,fixme};
	ets ->
	    ok
    end.

open(Name, Access) ->
    DbFile = ?MODULE:Name(db_file),
    case ?MODULE:Name(db) of
	bdb when Access =:= read ->
	    set_db_home(),
	    {ok,Ref} = bdb:open(DbFile, hash, []), 
	    put({bdb,Name}, Ref), %% movi_reg:register(Name, Ref),
	    Name;
	bdb when Access =:= read_write ->
	    set_db_home(),
	    {ok,Ref} = bdb:open(DbFile, hash, [create]), 
	    put({bdb,Name}, Ref), %% movi_reg:register(Name, Ref),
	    Name;
	dets ->
	    {ok,_} = dets:open_file(Name, [{access,Access},{file,DbFile},
					   {keypos,2}]),
	    Name;
	ets ->
	    ets:new(Name,[named_table,public,{keypos,2}])
    end.

close(Name) ->
    case ?MODULE:Name(db) of
	dets ->
	    dets:close(Name);
	ets ->
	    ets:delete(Name);
	bdb ->
	    Ref = get({bdb,Name}), %% movi_reg:lookup(Name),
	    bdb:close(Ref)
    end.

set_db_home() ->
    os:putenv("DB_HOME", filename:join(code:priv_dir(movi), "bdb")).

%% perhaps store data as:
%%   binary-key:  <<"tt1234567">> | <<"nm1234567">> |
%%                <<"tt1234567,1">>  (sub index)
%%   value is term_to_binary([Value1,Value2,....])
%%
lookup(Name, Key) ->
    case ?MODULE:Name(db) of
	dets ->
	    dets:lookup(Name, Key);
	ets ->
	    ets:lookup(Name, Key);
	bdb ->
	    Ref = get({bdb,Name}), %% movi_reg:lookup(Name),
	    case bdb:get(Ref, term_to_binary(Key)) of
		{ok,FsBin} ->
		    Record = list_to_tuple([Name,Key|binary_to_term(FsBin)]),
		    [Record];
		_ ->
		    []
	    end
    end.

insert(Name, Record) ->
    case ?MODULE:Name(db) of
	dets ->
	    dets:insert(Name, Record);
	ets ->
	    ets:insert(Name, Record);
	bdb ->
	    Ref = get({bdb,Name}), %% movi_reg:lookup(Name),
	    [_,Key|Values] = tuple_to_list(Record),
	    bdb:put(Ref, term_to_binary(Key), term_to_binary(Values))
    end.

fold(Name,Fun,Acc) when is_atom(Name) ->
    FileName = ?MODULE:Name(tsv_file),
    fold(Name,Fun,Acc,FileName).

fold(Name,Fun,Acc,File) ->
    case imdb_file:open(File) of
	{ok,Fd} ->
	    Fields = ?MODULE:Name(tsv_fields),
	    %% check header
	    case imdb_file:read(Fd) of
		{ok,Fields} ->
		    Fs = ?MODULE:Name(types),
		    R0 = ?MODULE:Name(record),
		    try imdb_file:fold_fd_record(Fun,Acc,Fd,Fs,R0) of
			Acc1 -> Acc1
		    catch
			error:Reason ->
			    {error,Reason}
		    after
			imdb_file:close(Fd)
		    end;
		{ok,_Header} -> {error,bad_header};
		eof -> {error,empty};
		{error,_} = Error -> Error
	    end;
	Error ->
	    Error
    end.

%% title record number
tconst(<<$t,$t,Const/binary>>) ->
    binary_to_integer(Const).

%% name record number
nconst(<<$n,$m,Const/binary>>) ->
    binary_to_integer(Const).

string(?UNDEFINED) -> undefined;
string(String) -> String.

year(?UNDEFINED) -> undefined;
year(Y) -> binary_to_integer(Y).

%% fixme translate to clear text atom
lang(?UNDEFINED) -> undefined;
lang(LA= <<_,_>>) -> LA;
lang(LAA= <<_,_,_>>) -> LAA;
lang(LAAA= <<_,_,_,_>>) -> LAAA.

integer(?UNDEFINED) -> undefined;
integer(Bin) ->  binary_to_integer(Bin).

float(?UNDEFINED) -> undefined;
float(Bin) ->
    try binary_to_float(Bin) of
	F -> F
    catch
	error:_ ->
	    erlang:float(binary_to_integer(Bin))
    end.

number(?UNDEFINED) -> undefined;
number(Bin) -> 
    try binary_to_float(Bin) of
	F -> F
    catch
	error:_ ->
	    binary_to_integer(Bin)
    end.

boolean(?UNDEFINED) -> undefined;
boolean(<<"0">>) -> false;
boolean(<<"1">>) -> true.

atom_list(?UNDEFINED) -> undefined;
atom_list(Bin) -> 
    String = string:to_lower(binary_to_list(Bin)),
    [list_to_atom(A) || A <- string:split(String, ",", all)].

atom(?UNDEFINED) -> undefined;
atom(A) -> binary_to_atom(A, latin1).

tconst_list(?UNDEFINED) -> undefined;
tconst_list(Bin) -> 
    Ts = binary:split(Bin,<<",">>,[global]),
    [binary_to_integer(TConst) || <<$t,$t,TConst/binary>> <- Ts].

nconst_list(?UNDEFINED) -> undefined;
nconst_list(Bin) -> 
    Ns = binary:split(Bin,<<",">>,[global]),
    [binary_to_integer(NConst) || <<$n,$m,NConst/binary>> <- Ns].

string_array(?UNDEFINED) -> undefined;
string_array(Bin) ->
    {ok,Ts,_} = erl_scan:string(binary_to_list(Bin)),
    [S || {string,_,S} <- Ts].

%% category
category(?UNDEFINED) -> #category.undefined;
category(<<"self">>) ->  #category.self;
category(<<"director">>) ->  #category.director;
category(<<"composer">>) -> #category.composer;
category(<<"writer">>) -> #category.writer;
category(<<"editor">>) -> #category.editor;
category(<<"actor">>) -> #category.actor;
category(<<"actress">>) -> #category.actress;
category(<<"producer">>) -> #category.producer;
category(<<"production_designer">>) -> #category.production_designer;
category(<<"cinematographer">>) -> #category.cinematographer;
category(<<"archive_footage">>) -> #category.archive_footage;
category(<<"archive_sound">>) -> #category.archive_sound.
				     
%% jobs
job(?UNDEFINED) -> #job.undefined;
job(<<"self">>) -> #job.self;
job(<<"director">>) -> #job.director;
job(<<"writer">>) -> #job.writer;
job(<<"co-writer">>) -> #job.co_writer;
job(<<"creator">>) -> #job.creator;
job(<<"co-director">>) -> #job.co_director;
job(<<"producer">>) -> #job.producer;
job(<<"executive producer">>) -> #job.executive_producer;
job(<<"co-composer">>) -> #job.co_composer;
job(<<"director of photography">>) -> #job.director_of_photography;
job(<<"novel">>) -> #job.novel;
job(<<"story">>) -> #job.story;
job(<<"original story">>) -> #job.original_story;
job(<<"short story">>) -> #job.short_story;
job(<<"original story and screen play">>) -> 
    [#job.original_story,#job.original_screen_play];
job(<<"poem">>) -> #job.poem;
job(<<"play">>) -> #job.play;
job(<<"plays">>) -> #job.plays;
job(<<"tale">>) -> #job.tale;
job(<<"opera">>) -> #job.opera;
job(<<"titles">>) -> #job.titles;
job(<<"teleplay">>) -> #job.teleplay;
job(<<"teleplay by">>) -> #job.teleplay;
job(<<"idea">>) -> #job.idea;
job(<<"poems">>) -> #job.poems;
job(<<"memoirs">>) -> #job.memoirs;
job(<<"dialogue">>) -> #job.dialogue;
job(<<"commentary">>) -> #job.commentary;
job(<<"screenplay">>) -> #job.screen_play;
job(<<"screen play">>) -> #job.screen_play;
job(<<"screen play by">>) -> #job.screen_play;
job(<<"screenplay by">>) -> #job.screen_play;
job(<<"original screenplay">>) -> #job.original_screen_play;
job(<<"comic strip">>)  -> #job.comic_strip;
job(<<"comedy">>)  -> #job.comedy;
job(<<"scenario">>) -> #job.scenario;
job(<<"novella">>)  -> #job.novella;
job(<<"adaptation">>)  -> #job.adaptation;
job(<<"book">>) -> #job.book;
job(<<"fable">>) -> #job.fable;
job(<<"translation">>) -> #job.translation;
job(<<"main characters">>)  -> #job.main_characters;
job(<<"based on a play by">>) -> #job.based_on_a_play_by;
job(<<"based on the play by">>) -> #job.based_on_a_play_by;
job(<<"based on a story by">>) -> #job.based_on_a_story_by;
job(<<"based on a characters created by">>) -> #job.based_on_characters_by;
job(<<"based on a novel by">>) -> #job.based_on_a_novel_by;
job(<<"from a story by">>) -> #job.based_on_a_story_by;
job(<<"adapted from the play by">>) -> #job.adapted_from_the_play_by;
job(<<"written by">>) -> #job.written_by;
job(<<"contributing writer">>) -> #job.contributing_writer;
job(<<"staff writer">>) -> #job.staff_writer;
job(<<"unused screenplay">>) -> #job.unused_screenplay;
job(<<"quotation">>) -> #job.quotation;
job(<<"manuscript">>) -> #job.manuscript;
job(<<"unfinished novel">>) -> #job.unfinished_novel;
job(<<"unconfirmed">>) -> #job.unconfirmed;
job(<<"libretto">>) -> #job.libertto;
job(<<"photoplay">>) -> #job.photoplay;
job(<<"by">>) -> #job.by;
job(<<"video editor">>) -> #job.video_editor;
job(<<"opera libretto">>) -> #job.opera_libretto;
job(<<"adaptation and scenario">>) -> [#job.adaptation,#job.scenario];
%% one argument
job(<<"story ", Name/binary>>) -> {#job.story,Name};
job(<<"short story ", Name/binary>>) -> {#job.short_story,Name};
job(<<"play ", Name/binary>>)  -> {#job.play,Name};
job(<<"poem ", Name/binary>>)  -> {#job.poem,Name};
job(<<"comic strip ", Name/binary>>)  -> {#job.comic_strip,Name};
job(<<"novel ", Name/binary>>) -> {#job.novel,Name};
job(<<"novels ", Name/binary>>) -> {#job.novels,Name};
job(<<"stories ", Name/binary>>) -> {#job.stories,Name};
job(<<"operetta",Name/binary>>) -> {#job.operetta,Name};
job(<<"opera ",Name/binary>>) -> {#job.opera,Name};
job(<<"book ", Name/binary>>) -> {#job.book,Name};
job(<<"character ",Name/binary>>) -> {#job.character,Name};
job(<<"after: ", Name/binary>>) -> {#job.'after',Name};
job(<<"poems ",Name/binary>>) -> {#job.poems, Name};
job(<<"adapted from his novel: ",Name/binary>>) ->
    {#job.adapted_from_his_novel,Name};
job(What) -> 
    %% io:format("job: ~p\n", [What]),
    What.
