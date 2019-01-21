%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    basic record element decode / encode
%%% @end
%%% Created : 27 Feb 2018 by Tony Rogvall <tony@rogvall.se>

-module(imdb).
-compile(export_all).

-include("../include/tsv.hrl").
-include("../include/imdb.hrl").

-define(dbg(Fmt,As), io:format((Fmt),(As))).
%% -define(dbg(Fmt,As), ok).
 
-define(BOOLEAN, ?TSV_TYPE_BOOLEAN).
-define(FLOAT,   ?TSV_TYPE_FLOAT32).
-define(INT,     ?TSV_TYPE_INT64).
-define(INT8,    ?TSV_TYPE_INT8).
-define(ATOM,    ?TSV_TYPE_ATOM8).
-define(STRING,  ?TSV_TYPE_STRING8).

-define(TCONST,   ?TSV_TYPE_TAGGED($t,$t,?TSV_TYPE_INT64)).
-define(NCONST,   ?TSV_TYPE_TAGGED($n,$m,?TSV_TYPE_INT64)).
-define(UCONST,   ?TSV_TYPE_TAGGED($u,$u,?TSV_TYPE_INT64)).
-define(GCONST,   ?TSV_TYPE_TAGGED($g,$g,?TSV_TYPE_INT64)).
-define(YEAR,     ?TSV_TYPE_TAGGED(0,$Y,?TSV_TYPE_INT16)).
-define(JOB,      ?TSV_TYPE_TAGGED(0,$J,?TSV_TYPE_BINARY8)).
-define(CATEGORY, ?TSV_TYPE_TAGGED(0,$C,?TSV_TYPE_INT8)).
-define(LANG,     ?TSV_TYPE_TAGGED(0,$L,?TSV_TYPE_ATOM8)).

-define(LIST(T), (?TSV_TYPE_LIST8 bor (T))).
-define(ARRAY(T), (?TSV_TYPE_ARRAY8 bor (T))).

-define(CATEGORY_LIST,
	?CMAP("self",     ?CATEGORY_self),
	?CMAP("director", ?CATEGORY_director),
	?CMAP("composer", ?CATEGORY_composer),
	?CMAP("writer",   ?CATEGORY_writer),
	?CMAP("editor",   ?CATEGORY_editor),
	?CMAP("actor",    ?CATEGORY_actor),
	?CMAP("actress",  ?CATEGORY_actress),
	?CMAP("producer",  ?CATEGORY_producer),
	?CMAP("production_designer", ?CATEGORY_production_designer),
	?CMAP("cinematographer",  ?CATEGORY_cinematographer),
	?CMAP("archive_footage",  ?CATEGORY_archive_footage),
	?CMAP("archive_sound",    ?CATEGORY_archive_sound)
	).

-define(CMAP(K,V), <<K>> => V).
category_map(Value) -> maps:get(Value, #{ ?CATEGORY_LIST }).
-undef(CMAP).

-define(CMAP(K,V), V => <<K>>).
category_revmap(Value) -> maps:get(Value, #{ ?CATEGORY_LIST }).
-undef(CMAP).


-define(JOB_LIST,
	?JMAP("self", ?JOB_self);
	?JMAP("director", ?JOB_director);
	?JMAP("co-director", ?JOB_co_director);
	?JMAP("writer", ?JOB_writer);
	?JMAP("co-writer", ?JOB_co_writer);
	?JMAP("creator", ?JOB_creator);
        ?JMAP("co-creator", ?JOB_co_creator);
	?JMAP("producer", ?JOB_producer);
	?JMAP("co-producer", ?JOB_co_producer);
	?JMAP("co-composer", ?JOB_co_composer);
	?JMAP("executive producer", ?JOB_executive_producer);
	?JMAP("composer", ?JOB_composer);
	?JMAP("co-composer", ?JOB_co_composer);
	?JMAP("director of photography", ?JOB_director_of_photography);
	?JMAP("novel", ?JOB_novel);
	?JMAP("story", ?JOB_story);
	?JMAP("original story", ?JOB_original_story);
	?JMAP("short story", ?JOB_short_story);
	?JMAP("poem", ?JOB_poem);
	?JMAP("poems", ?JOB_poems);
	?JMAP("memoirs", ?JOB_memoirs);
	?JMAP("play", ?JOB_play);
	?JMAP("plays", ?JOB_plays);
	?JMAP("tale", ?JOB_tale);
	?JMAP("opera", ?JOB_opera);
	?JMAP("titles", ?JOB_titles);
	?JMAP("teleplay", ?JOB_teleplay);
	?JMAP("teleplay by", ?JOB_teleplay);
	?JMAP("idea", ?JOB_idea);
	?JMAP("dialogue", ?JOB_dialogue);
	?JMAP("commentary", ?JOB_commentary);
	?JMAP("screenplay", ?JOB_screen_play);
	?JMAP("screen play", ?JOB_screen_play);
	?JMAP("screen play by", ?JOB_screen_play);
	?JMAP("screenplay by", ?JOB_screen_play);

	?JMAP("original screenplay", ?JOB_original_screen_play);
	?JMAP("comic strip", ?JOB_comic_strip);
	?JMAP("comedy", ?JOB_comedy);
	?JMAP("scenario", ?JOB_scenario);
	?JMAP("novella", ?JOB_novella);
	?JMAP("adaptation", ?JOB_adaptation);
	?JMAP("book", ?JOB_book);
	?JMAP("fable", ?JOB_fable);
	?JMAP("translation", ?JOB_translation);

	?JMAP("main characters", ?JOB_main_characters);
	?JMAP("based on a play by", ?JOB_based_on_a_play_by);
	?JMAP("based on the play by", ?JOB_based_on_a_play_by);
	?JMAP("based on a story by", ?JOB_based_on_a_story_by);
	?JMAP("based on a characters created by",
	     ?JOB_based_on_characters_by);
	?JMAP("based on a novel by", ?JOB_based_on_a_novel_by);
	?JMAP("from a story by", ?JOB_based_on_a_story_by);
	?JMAP("adapted from the play by", ?JOB_adapted_from_the_play_by);
	?JMAP("written by", ?JOB_written_by);
	?JMAP("contributing writer", ?JOB_contributing_writer);
	?JMAP("staff writer", ?JOB_staff_writer);

	?JMAP("unused screenplay", ?JOB_unused_screenplay);
	?JMAP("quotation", ?JOB_quotation);
	?JMAP("stories", ?JOB_stories);
	?JMAP("manuscript", ?JOB_manuscript);
	?JMAP("unfinished novel", ?JOB_unfinished_novel);
	?JMAP("unconfirmed", ?JOB_unconfirmed);
	?JMAP("libretto", ?JOB_libertto);
	?JMAP("photoplay", ?JOB_photoplay);
	?JMAP("by", ?JOB_by);

	?JMAP("after", ?JOB_after);
	?JMAP("video editor", ?JOB_video_editor);
	?JMAP("opera libretto", ?JOB_opera_libretto);
	?JMAP("adapted from his novel", ?JOB_adapted_from_his_novel);

        %% and combined
	?JMAP_AND("adaptation and scenario",
		  ?JOB_adaptation, ?JOB_scenario);
	?JMAP_AND("original story and screen play",
		  ?JOB_original_story, ?JOB_screen_play);
	%% one argument blank mark continuation
	?JMAPA("story", ?JOB_story);
	?JMAPA("short story", ?JOB_short_story);
	?JMAPA("play", ?JOB_play);
	?JMAPA("poem", ?JOB_poem);
	?JMAPA("comic strip", ?JOB_comic_strip);
	?JMAPA("novel", ?JOB_novel);
	?JMAPA("novels", ?JOB_novels);
	?JMAPA("stories", ?JOB_stories);
	?JMAPA("operetta", ?JOB_operetta);
	?JMAPA("opera", ?JOB_opera);
	?JMAPA("book", ?JOB_book);
	?JMAPA("character", ?JOB_character);
	?JMAPA("after", ?JOB_after);
	?JMAPA("poems", ?JOB_poems);
	?JMAPA("adapted from his novel:", ?JOB_adapted_from_his_novel)
       ).

-define(JMAP(String,C1),	(<<String>>) -> <<(C1)>>).
-define(JMAP_AND(String,C1,C2),	(<<String>>) -> <<(C1),(C2)>>).
-define(JMAPA(String,C1), 
	(<<String" ",Arg/binary>>) -> <<(C1),$\~,Arg/binary>>).
job_map(Value) ->
    case Value of
	?JOB_LIST;
	(Any) -> <<$\~,Any/binary>>
    end.
-undef(JMAP).
-undef(JMAP_AND).
-undef(JMAPA).

-define(JMAP(String,C1),        C1 -> <<String>>).
-define(JMAP_AND(String,C1,C2), 0 -> <<>>).
-define(JMAPA(String,C1),       C1 -> <<String>>).

job_lookup(C) ->
    case C of
	?JOB_LIST;
	_ -> error({C, not_a_job})
    end.

job_revmap(Value) ->
    case Value of
	<<$\~,Arg/binary>> -> Arg;
	<<C1,$\~,Arg/binary>> ->
	    S1 = job_lookup(C1),
	    <<S1/binary," ",Arg/binary>>;
	<<C1,C2>> ->
	    S1 = job_lookup(C1),
	    S2 = job_lookup(C2),
	    <<S1/binary," and ",S2/binary>>;
	<<C1>> ->
	    job_lookup(C1);
	<<>> ->
	    <<>>
    end.
    
-define(TSV_FIELD(Name,Type,Default),
	#tsv_field{name=(<<Name>>),type=(Type),default=(Default)}).
-define(TSV_FIELD(Name,Type,Parser,Default),
	#tsv_field{name=(<<Name>>),type=(Type),parser=(Parser),
		   default=(Default)}).
-define(TSV_FIELD(Name,Type,Parser,Formatter,Default),
	#tsv_field{name=(<<Name>>),type=(Type),parser=(Parser),
		   formatter=(Formatter),
		   default=(Default)}).
	
%% Table attributes and settings

%% #name_basics{} db information
name_basics(db)   -> bdb;
name_basics(record) -> name_basics;
name_basics(table) ->
    #tsv_table {
       name = ?FUNCTION_NAME,
       primary = #name_basics.nconst,
       fields = [?TSV_FIELD("nconst",?NCONST,0),
		 ?TSV_FIELD("primaryName",?STRING,""),
		 ?TSV_FIELD("birthYear",?YEAR,0),
		 ?TSV_FIELD("deathYear", ?YEAR, 0),
		 ?TSV_FIELD("primaryProfession",?LIST(?ATOM),[]),
		 ?TSV_FIELD("knownForTitles", ?LIST(?TCONST), [])]
      };
name_basics(tsv_file) ->
    filename:join(code:priv_dir(movi), "name.basics.tsv.gz");
name_basics(db_file) ->
    filename:join(code:priv_dir(movi), "name.basics.rec").

title_basics(record) -> title_basics;
title_basics(table) ->
    #tsv_table {
       name = ?FUNCTION_NAME,
       primary = #title_basics.tconst,
       fields = [?TSV_FIELD("tconst", ?TCONST, 0),
		 ?TSV_FIELD("titleType", ?ATOM, ''),
		 ?TSV_FIELD("primaryTitle", ?STRING, ""),
		 ?TSV_FIELD("originalTitle", ?STRING, ""),
		 ?TSV_FIELD("isAdult", ?BOOLEAN, undefined),
		 ?TSV_FIELD("startYear", ?YEAR, 0),
		 ?TSV_FIELD("endYear", ?YEAR, 0),
		 ?TSV_FIELD("runtimeMinutes", ?INT, 0),
		 ?TSV_FIELD("genres", ?LIST(?ATOM), [])]
      };
title_basics(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.basics.tsv.gz");
title_basics(db_file) ->
    filename:join(code:priv_dir(movi), "title.basics.rec").

title_episode(record) -> title_episode;
title_episode(table) ->
    #tsv_table {
       name = ?FUNCTION_NAME,
       primary = #title_episode.tconst,
       fields = 
	   [?TSV_FIELD("tconst", ?TCONST, 0),
	    ?TSV_FIELD("parentTconst",?TCONST, 0),
	    ?TSV_FIELD("seasonNumber", ?INT, 0),
	    ?TSV_FIELD("episodeNumber", ?INT, 0)]
      };
title_episode(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.episode.tsv.gz");
title_episode(db_file) ->
    filename:join(code:priv_dir(movi), "title.episode.rec").

title_crew(record) -> title_crew;
title_crew(table) ->
    #tsv_table {
       name = ?FUNCTION_NAME,
       primary = #title_crew.tconst,
       fields = 
	   [?TSV_FIELD("tconst", ?TCONST, 0),
	    ?TSV_FIELD("directors", ?LIST(?NCONST), []),
	    ?TSV_FIELD("writers", ?LIST(?NCONST), [])]
      };
title_crew(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.crew.tsv.gz");
title_crew(db_file) ->
    filename:join(code:priv_dir(movi), "title.crew.rec").

title_ratings(db) -> ets;
title_ratings(record) -> title_ratings;
title_ratings(table) ->
    #tsv_table {
       name = ?FUNCTION_NAME,
       primary = #title_ratings.tconst,
       fields = 
	   [?TSV_FIELD("tconst",?TCONST, 0),
	    ?TSV_FIELD("averageRating",?FLOAT, 0.0),
	    ?TSV_FIELD("numVotes", ?INT, 0)]
      };
title_ratings(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.ratings.tsv.gz");
title_ratings(db_file) ->
    filename:join(code:priv_dir(movi), "title.ratings.rec").

title_principals(db) -> bdb;
title_principals(record) -> title_principals;
title_principals(table) ->
    #tsv_table {
       name = ?FUNCTION_NAME,
       primary = [#title_principals.tconst,#title_principals.ordering],
       fields = 
	   [?TSV_FIELD("tconst",?TCONST,[]),
	    ?TSV_FIELD("ordering",?INT8,[]),
	    ?TSV_FIELD("nconst",?NCONST,[]),
	    ?TSV_FIELD("category",?CATEGORY, fun category_map/1, 
		       fun category_revmap/1, 0),
	    ?TSV_FIELD("job", ?JOB, fun job_map/1, <<>>),
	    ?TSV_FIELD("characters", ?ARRAY(?STRING),{})]
      };
title_principals(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.principals.tsv.gz");
title_principals(db_file) ->
    filename:join(code:priv_dir(movi), "title.principals.rec").

title_akas(record) -> title_akas;
title_akas(table) ->
    #tsv_table {
       name = ?FUNCTION_NAME,
       primary = [#title_akas.tconst,#title_akas.ordering],
       fields = 
	   [?TSV_FIELD("titleId",?TCONST,0),
	    ?TSV_FIELD("ordering",?INT8,0),
	    ?TSV_FIELD("title",?STRING,""),
	    ?TSV_FIELD("region",?LANG,''),
	    ?TSV_FIELD("language",?LANG,''),
	    ?TSV_FIELD("types",?STRING,""),
	    ?TSV_FIELD("attributes",?STRING,""),
	    ?TSV_FIELD("isOriginalTitle",?BOOLEAN, undefined)]
      };
title_akas(tsv_file) ->
    filename:join(code:priv_dir(movi), "title.akas.tsv.gz");
title_akas(db_file) ->
    filename:join(code:priv_dir(movi), "title.akas.rec").

user(db) -> ets;
user(record) -> user;
user(table) ->
    #tsv_table {
       name = ?FUNCTION_NAME,
       primary = 2,  %% record position = 1
       fields = 
	   [?TSV_FIELD("uconst",?UCONST,0),
	    ?TSV_FIELD("name",?STRING,""),
	    ?TSV_FIELD("email",?STRING,""),
	    ?TSV_FIELD("fullName",?STRING,"")]
      };
user(tsv_file) ->
    filename:join(code:priv_dir(movi), "user.tsv");
user(db_file) ->
    filename:join(code:priv_dir(movi), "user.rec").

group(db) -> ets;
group(record) -> group;
group(table) ->
    #tsv_table {
       name = ?FUNCTION_NAME,
       primary = 2,  %% record position = 1
       fields = 
	   [?TSV_FIELD("gconst",?GCONST,0),
	    ?TSV_FIELD("name",?STRING,""),
	    ?TSV_FIELD("users",?LIST(?UCONST),[])]
      };
group(tsv_file) ->
    filename:join(code:priv_dir(movi), "group.tsv");
group(db_file) ->
    filename:join(code:priv_dir(movi), "group.rec").
    
%% Generate database file from tsv file

import_tsv_file(Name, BuildIndex) ->
    Table = ?MODULE:Name(table),
    Fixed = tsvdb:record_bytes(Table#tsv_table.fields) > 0,
    DbFile = ?MODULE:Name(db_file),
    ?dbg("import ~s.tsv to ~s fixed=~w\n", [Name,DbFile,Fixed]),
    case tsvdb:open_rec(DbFile, write) of
	{ok,Fd} ->
	    Index = ets:new(tsv_index, [ordered_set]),
	    T0 = erlang:monotonic_time(),
	    {Count,_Last} =
		fold(Name,
		     fun(Rec,{I,Offs}) ->
			     Size = try tsvdb:write_record(Fd,Table,Rec) of
					{ok,S}  -> S
				    catch
					error:Reason:Stack ->
					    io:format("Failed to write ~p\n",[Rec]),
					    io:format("~p:\n~p\n", [Reason,Stack]),
					    error(fail)
				    end,
			     if BuildIndex ->
				     Key = primary_key(Rec,Table),
				     ets:insert(Index, {Key,Offs});
				true ->
				     ok
			     end,
			     if (I+1) rem 10000 =:= 0 ->
				     io:put_chars(".");
				true -> ok
			     end,
			     if (I+1) rem 500000 =:= 0 ->
				     T1 = erlang:monotonic_time(),
				     Time = erlang:convert_time_unit(T1 - T0, native, microsecond),
				     io:format(" ~.2f\n", 
					       [1000000*((I+1)/Time)]);
				true -> ok
			     end,
			     {I+1,Offs+Size}
		     end,{0,0}),
	    io:put_chars("\n"),
	    T1 = erlang:monotonic_time(),
	    Time = erlang:convert_time_unit(T1-T0,native,microsecond),
	    io:format("wrote ~w record in ~.2f seconds, from ~s.tsv to ~s ~.2f recs/sec\n", 
		      [Count, Time/1000000, Name, DbFile,
		       1000000*(Count / Time)]),
	    tsvdb:close(Fd),
	    %% write sorted offsets into file or last in rec file
	    %% to be used for binary search
	    if BuildIndex ->
		    case tsvdb:open_pri(DbFile, write) of
			{ok,Fd1} ->
			    ets:foldl(
			      fun({_Key,Offset},I) ->
				      ok = file:write(Fd1, <<Offset:64>>),
				      I+1
			      end, 1, Index),
			    tsvdb:close(Fd1),
			    ets:delete(Index),
			    {ok,Count};
			Error={error,Reason} ->
			    io:format("fail build, index table ~p\n", [Reason]),
			    Error
		    end;
	       true ->
		    ets:delete(Index),
		    ok
	    end;

	Error = {error,Reason} ->
	    ?dbg("fail import ~p\n", [Reason]),
	    Error
    end.

primary_key(Rec, Table) ->
    case Table#tsv_table.primary of
	I when is_integer(I) -> element(I, Rec);
	[I] -> element(I, Rec);
	[I,J] -> {element(I,Rec),element(J,Rec)}
    end.

export_tsv_file(Name) ->
    export_tsv_file(-1,Name).

export_tsv_file(MaxRecords,Name) ->
    Table = ?MODULE:Name(table),
    DbFile = ?MODULE:Name(db_file),
    Fields = Table#tsv_table.fields,
    Names = [N || #tsv_field{name=N} <- Fields],
    case tsvdb:open_rec(DbFile, read) of
	{ok,Fd} ->
	    case tsv:open(DbFile, write) of
		{ok,Fd1} ->
		    tsv:write(Fd1, Names),
		    try export_tsv_records(MaxRecords,Fd,Fd1,Table) of
			ok -> ok
		    catch
			error:Reason:Stack ->
			    io:format("export_tsv_records crash\n~p\n", 
				      [Stack]),
			    {error,Reason}
		    after
			tsv:close(Fd1),
			tsvdb:close(Fd)
		    end;
		Error ->
		    tsvdb:close(Fd),
		    Error
	    end;
	Error -> Error
    end.

export_tsv_records(0, _Fd, _Fd1, _Table) ->
    ok;
export_tsv_records(I, Fd, Fd1, Table) ->
    case tsvdb:read_record(Fd, Table) of
	{ok,Record} ->
	    Fs = tsv:format_record(Record, Table#tsv_table.fields),
	    tsv:write(Fd1, Fs),
	    export_tsv_records(I-1,Fd, Fd1, Table);
	Error = {error,_} ->
	    Error;
	eof ->
	    ok
    end.

import_all() ->
    import_tsv_file(name_basics, false),
    import_tsv_file(title_basics, false),
    import_tsv_file(title_episode, false),
    import_tsv_file(title_crew, false),
    import_tsv_file(title_ratings, true),
    import_tsv_file(title_principals, false),
    import_tsv_file(title_akas, false).

import_local_all() ->
    import_tsv_file(user, true),
    import_tsv_file(group, true).

%% read rec files and produce tsv files
export_all() ->
    export_tsv_file(title_ratings),
    ok.

%% setup ets/dets tables

list(Name) ->
    list(Name, (1 bsl 64)).

list(Name,Max) ->
    Table  = ?MODULE:Name(table),
    Fixed  = tsvdb:record_bytes(Table#tsv_table.fields),
    fold(Name,
	 fun(Rec,I) ->
		 ?dbg("~w: record=~p\n", [I,Rec]),
		 Bin = tsvdb:encode_record(Table, Rec),
		 Static = byte_size(Bin),
		 io:format("~w:[fixed=~w,sta=~w] ~p\n", [I,Fixed,Static,Rec]),
		 if I >= Max ->
			 throw(stop);
		    true ->
                         ok
		 end,
		 I+1
	 end, 1).

test() ->
    list(name_basics, 4),
    list(title_basics, 4),
    list(title_episode, 4),
    list(title_crew, 4),
    list(title_ratings, 4),
    list(title_principals, 4),
    list(title_akas, 4),
    list(user, 4),
    list(group, 4).

lookup(Name, Key) ->
    Table = ?MODULE:Name(table),
    DbFile = ?MODULE:Name(db_file),
    {ok,Fd} = tsvdb:open_rec(DbFile, read),
    {ok,Fi} = tsvdb:open_pri(DbFile, read),
    %% binary search using Offset in Fi
    {ok,Len0} = file:position(Fi, eof),
    Len = Len0 div 8,
    %% Fixed = tsvdb:record_bytes(Table#tsv_table.fields) > 0,
    Result = binary_search(Table, Fd, Fi, 0, Len-1, Key),
    tsvdb:close(Fd),
    tsvdb:close(Fi),
    Result.

binary_search(Table, Fd, Fi, L, R, Key) when L =< R ->
    I = (L + R) div 2,
    {ok,<<Offset:64>>} = file:pread(Fi,I*8,8),
    file:position(Fd, Offset),
    {ok,Rec} = tsvdb:read_record(Fd, Table),
    V = primary_key(Rec,Table),
    if V < Key ->
	    binary_search(Table, Fd, Fi, I+1, R, Key);
       V > Key ->
	    binary_search(Table, Fd, Fi, L, I-1, Key);
       true ->
	    Rec
    end;
binary_search(_Table, _Fd, _Fi, _L, _R, _Key) ->
    false.

fold(Name,Fun,Acc) when is_atom(Name) ->
    FileName = ?MODULE:Name(tsv_file),
    fold(Name,Fun,Acc,FileName).

fold(Name,Fun,Acc,File) ->
    case tsv:open(File,read) of
	{ok,Fd} ->
	    Table  = ?MODULE:Name(table),
	    Fields = Table#tsv_table.fields,
	    Names = [N || #tsv_field{name=N} <- Fields],
	    %% check header
	    case tsv:read(Fd) of
		{ok,Names} ->
		    R0 = ?MODULE:Name(record),
		    try tsv:fold_fd_record(Fun,Acc,Fd,Fields,R0) of
			Acc1 -> Acc1
		    catch
			error:Reason:Stack ->
			    {error,Reason,Stack}
		    after
			tsv:close(Fd)
		    end;
		{ok,_Header} ->
		    ?dbg("got bad_header ~p\n",[_Header]),
		    ?dbg("names = ~p\n",[Names]),
		    {error,{Name,bad_header}};
		eof -> {error,empty};
		{error,_} = Error -> Error
	    end;
	Error ->
	    Error
    end.
