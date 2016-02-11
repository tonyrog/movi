%%
%% ftp://ftp.sunet.se/pub/tv+movies/imdb/ratings.list.gz
%%
-module(imdb).

-export([good_movies/0, good_new_movies/0, good_classic_movies/0]).

-compile(export_all).
-include("../include/imdb.hrl").

%%
%% attribues in exclude include: 
%%   videon_game,video,tv, {part,N}
%%   tv_series, {episode,E}, {season,S}, {episode_name,Name}
%%

load_ratings() ->
    search_db_ratings(
		 #filter { min_votes = 10000,
			   min_rating = 8.5
			 }).

best_movies() ->
     search_db_ratings(
		       #filter { min_votes = 10000,
				 min_rating = 8.0,
				 exclude = [tv,tv_series,video,video_game]
			       }).

good_movies() ->
     search_db_ratings(
		       #filter { min_votes = 10000,
				 min_rating = 7.0,
				 exclude = [tv,tv_series,video,video_game]
			       }).

best_new_movies() ->
    search_db_ratings(
		      #filter { min_votes = 10000,
				min_rating = 8.0,
				exclude = [tv,tv_series,video,video_game],
				min_year = 2013
			      }).
good_new_movies() ->
    search_db_ratings(
		      #filter { min_votes = 10000,
				min_rating = 7.0,
				exclude = [tv,tv_series,video,video_game],
				min_year = 2013
			      }).


best_classic_movies() ->
    search_db_ratings(
		      #filter { min_votes = 10000,
				min_rating = 7.0,
				exclude = [tv,tv_series,video,video_game],
				max_year = 1980
			     }).

good_classic_movies() ->
    search_db_ratings(
		      #filter { min_votes = 10000,
				min_rating = 7.0,
				exclude = [tv,tv_series,video,video_game],
				max_year = 1980
			     }).

do_filter(Item, Filter) ->
    Year = proplists:get_value(year,Item#item.attributes),
    if Item#item.votes < Filter#filter.min_votes -> true;
       Item#item.votes > Filter#filter.max_votes -> true;
       Item#item.rating < Filter#filter.min_rating -> true;
       Item#item.rating > Filter#filter.max_rating -> true;
       is_integer(Year), Year < Filter#filter.min_year -> true;
       is_integer(Year), Year > Filter#filter.max_year -> true;
       true ->
	    case Filter#filter.include -- Item#item.attributes of
		[] -> %% mandatory attributes are all present
		    Exclude = Filter#filter.exclude,
		    case Exclude -- Item#item.attributes of
			Exclude ->
			    if Filter#filter.title =:= ".*";
			       Filter#filter.title =:= "" ->
				    false;
			       true ->
				    case re:run(Item#item.title,
						Filter#filter.title_compiled) of
					nomatch -> true;
					{match,_} -> false
				    end
			    end;
			_ -> true
		    end;
		_ -> true
	    end
    end.

filter_options([Opt|Opts], Filter) ->
    case Opt of
	{min_votes,X} when is_integer(X), X >= 0 ->
	    filter_options(Opts, Filter#filter { min_votes=X});
	{max_votes,X} when is_integer(X), X >= 0 ->
	    filter_options(Opts, Filter#filter { max_votes=X});
	{min_rating,X} when is_number(X), X >= 0 ->
	    filter_options(Opts, Filter#filter { min_rating=X});
	{max_rating,X} when is_number(X), X >= 0 ->
	    filter_options(Opts, Filter#filter { max_rating=X});
	{min_year,X} when is_integer(X), X >= 0, X =< 9999 -> 
	    filter_options(Opts, Filter#filter { min_year=X});
	{max_year,X} when is_integer(X), X >= 0, X =< 9999 -> 
	    filter_options(Opts, Filter#filter { max_year=X});
	{include,X} when is_list(X) -> 
	    filter_options(Opts, Filter#filter { include=X});
	{exclude,X} when is_list(X) -> 
	    filter_options(Opts, Filter#filter { exclude=X});
	{title,X} when is_list(X) ->
	    case re:compile(X) of
		{ok,Y} ->
		    filter_options(Opts, Filter#filter { title=X, 
							 title_compiled=Y });
		_ ->
		    error(badarg)
	    end;
	_ ->
	    error(badarg)
    end;
filter_options([], Filter) ->
    Filter.

search_db_ratings(Cond) when is_list(Cond) ->
    search_db_ratings(filter_options(Cond,#filter{}));
search_db_ratings(Cond) when is_record(Cond, filter) ->
    Ets = imdb_ratings:load(),
    ets:foldl(
      fun({_Key,Item}, Acc) ->
	      case do_filter(Item, Cond) of
		  true -> 
		      ok;
		  false ->
		      io:format("~30s ~w ~w [~s]\n",
				[Item#item.title,
				 Item#item.rating,
				 Item#item.votes,
				 format_attr(Item#item.attributes)]),
		      [Item|Acc]
	      end
      end, [], Ets).

format_attr([{year,YYYY}|Attrs]) ->
    ["(",tl(integer_to_list(10000+YYYY)),")" |
     format_attr(Attrs)];
format_attr([{part,N}|Attrs]) ->
    ["(",format_roman(N),")" | format_attr(Attrs)];
format_attr([video_game|Attrs]) ->
    [" Game " | format_attr(Attrs)];
format_attr([video|Attrs]) ->
    [" Video " | format_attr(Attrs)];
format_attr([tv|Attrs]) ->
    [" TV " | format_attr(Attrs)];
format_attr([tv_series|Attrs]) -> %% formated by SxxEyy
    format_attr(Attrs);
format_attr([{episodes,_}|Attrs]) -> %% not formated
    format_attr(Attrs);    
format_attr([{episode,E}|Attrs]) ->
    ["E",tl(integer_to_list(100+E))," " | format_attr(Attrs)];
format_attr([{season,S}|Attrs]) ->
    ["S",tl(integer_to_list(100+S)) | format_attr(Attrs)];
format_attr([{episode_name,_N}|Attrs]) ->
    format_attr(Attrs);
format_attr([]) ->
    [].

format_roman(N) ->
    if
	N >= 1000 -> [$M | format_roman(N-1000)];
	N >= 900  -> [$C,$M | format_roman(N-900)];
	N >= 500 -> [$D | format_roman(N-500)];
	N >= 400  -> [$C,$D | format_roman(N-400)];
	N >= 100 -> [$C | format_roman(N-100)];
	N >= 90 -> [$X,$C | format_roman(N-90)];
	N >= 50 -> [$L | format_roman(N-50)];
	N >= 40 -> [$X,$L | format_roman(N-40)];
	N >= 10 -> [$X | format_roman(N-10)];
	N >= 9  -> [$I,$X | format_roman(N-9)];
	N >= 5 -> [$V | format_roman(N-5)];
	N >= 4 -> [$I,$V | format_roman(N-4)];
	N >= 1 -> [$I | format_roman(N-1)];
	N =:= 0 -> []
    end.
