%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Wrapper to start movi
%%% @end
%%% Created : 12 Feb 2016 by Tony Rogvall <tony@rogvall.se>

-module(movi).
-compile(export_all).

-export([good_movies/0, good_new_movies/0, good_classic_movies/0]).

-export([start/0]).

-include("../include/imdb.hrl").

start() ->
    application:start(movi),
    load().

%% test load
load() ->
    io:format("loading ratings...\n", []),
    R1 = imdb:load(title_ratings),
    io:format("loaded ~w ratings\n", [R1]),

    io:format("loading basics ...\n", []),
    R2 = imdb:load(title_basics),
    io:format("loaded ~w basics\n", [R2]),

    io:format("loading principals ...\n", []),
    R3 = imdb:load(title_principals),
    io:format("loaded ~w principals\n", [R3]),

    io:format("loading names ...\n", []),
    R4 = imdb:load(name_basics),
    io:format("loaded ~w names\n", [R4]),

    ok.


best_movies() -> best_movies([]).
best_movies(Opts) ->
    search([{min_votes,10000},{min_rating,8.0},
	    {exclude,[tvEpisode,tvSeries,video,videoGame]}]++
	       Opts).

good_movies() -> good_movies([]).
good_movies(Opts) ->
    search([{min_votes,10000},
	    {min_rating,7.0},
	    {exclude,[tvEpisode,tvSeries,video,videoGame]}]++
	       Opts).
action_movies() -> action_movies([]).
action_movies(Opts) ->
    search([{min_votes,10000},
	    {min_rating,8.5},
	    {exclude,[tvEpisode,tvSeries,video,videoGame]},
	    {genres,[action]}]++
	       Opts).

funny_movies() -> funny_movies([]).
funny_movies(Opts) -> 
    search([{min_votes,10000},
	    {min_rating,8.5},
	    {exclude,[tvEpisode,tvSeries,video,videoGame]},
	    {genres,[comedy]}]++
	       Opts).

%% not more than 2 years old
best_new_movies() -> best_new_movies([]).
best_new_movies(Opts) ->
    {Year,_,_} = date(),
    search([{min_votes,10000},
	    {min_rating,8.0},
	    {exclude,[tvEpisode,tvSeries,video,videoGame]},
	    {min_year,Year-2}]++
	       Opts).

good_new_movies() -> good_new_movies([]).
good_new_movies(Opts) ->
    {Year,_,_} = date(),
    search([{min_votes,10000},
	    {min_rating,7.0},
	    {exclude,[tvEpisode,tvSeries,video,videoGame]},
	    {min_year,Year-2}]++
	       Opts).

best_classic_movies() -> best_classic_movies([]).
best_classic_movies(Opts) ->
    search([{min_votes,10000},
	    {min_rating,7.0},
	    {exclude,[tvEpisode,tvSeries,video,videoGame]},
	    {max_year,1980}]++
	       Opts).

good_classic_movies() -> good_classic_movies([]).
good_classic_movies(Opts) ->
    search([{min_votes,10000},
	    {min_rating,7.0},
	    {exclude,[tvEpisode,tvSeries,video,videoGame]},
	    {max_year,1980}]++
	       Opts).

%% include filter return true if item is filtered out
%% return false if not filtered out
%%  titleType and genres are check agains filter.include
%%
include_filter(#title_basics { titleType = undefined }, _F) ->
    true;  %% no title type are filtered out
include_filter(#title_basics { titleType = T }, F) ->
    if F#filter.include =:= [] -> false;
       true -> not lists:member(T, F#filter.include)
    end.

exclude_filter(#title_basics { titleType = undefined }, _F) ->
    true;  %% no title type are filtered out
exclude_filter(#title_basics { titleType = T }, F) ->
    if F#filter.exclude =:= [] -> false;
       true -> lists:member(T, F#filter.exclude)
    end.

%% all of the genrese in the filter must be present
genres_filter(#title_basics { genres = undefined}, _F) ->
    false;
genres_filter(#title_basics { genres = Gs}, F) ->
    Fs = F#filter.genres,
    if Fs =:= [] -> false;
       true ->
	    case Fs -- Gs of
		[] -> false;
		_ -> true
	    end
    end.

title_filter(Title, Filter) ->
    if Title =:= undefined, Filter#filter.title =:= "" -> false;
       Filter#filter.title =:= ".*"; Filter#filter.title =:= "" ->
	    false;
       true ->
	    case re:run(Title,Filter#filter.title_compiled) of
		nomatch -> true;
		{match,_} -> false
	    end
    end.

name_filter(Name, Filter) ->
    if Name =:= undefined, Filter#filter.name =:= "" -> false;
       Filter#filter.name =:= ".*"; Filter#filter.name =:= "" ->
	    false;
       true ->
	    case re:run(Name,Filter#filter.name_compiled) of
		nomatch -> true;
		{match,_} -> false
	    end
    end.

options() ->
    [min_votes,  max_votes,
     min_rating, max_rating,
     min_year,   
     max_year,
     include,  %% include titleType(s)
     exclude,  %% exclude titleType(s)
     genres,   %% action,comedy,drama...
     title,    %% regexp
     name      %% regexp
    ].

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
	{genres,X} when is_list(X) ->
	    filter_options(Opts, Filter#filter { genres=X });
	{title,X} when is_list(X) ->
	    case re:compile(X) of
		{ok,Y} ->
		    filter_options(Opts, Filter#filter { title=X, 
							 title_compiled=Y });
		_ ->
		    error(badarg)
	    end;
	{category,X} when is_list(X) ->
	    filter_options(Opts, Filter#filter { category=X});
	{name,X} when is_list(X) ->
	    case re:compile(X) of
		{ok,Y} ->
		    filter_options(Opts, Filter#filter { name=X, 
							 name_compiled=Y });
		_ ->
		    error(badarg)
	    end;
	_ ->
	    error(badarg)
    end;
filter_options([], Filter) ->
    Filter.

search(Cond) when is_list(Cond) ->
    search(filter_options(Cond,#filter{}));
search(Cond) when is_record(Cond, filter) ->
    ets:foldl(
      fun(R, Acc) ->
	      case ratings_filter(R, Cond) of
		  true -> 
		      Acc;
		  false ->
		      ID = R#title_ratings.tconst,
		      [B] = imdb:lookup(title_basics, ID),
		      case basics_filter(B, Cond) of
			  true -> Acc;
			  false ->
			      case principal_filter(ID, Cond) of
				  true -> Acc;
				  false ->
				      io:format("tt~7..0w: ~-30ts ~w ~w ~w ~w\n",
						[ID,
						 B#title_basics.originalTitle,
						 B#title_basics.startYear,
						 R#title_ratings.averageRating,
						 R#title_ratings.numVotes,
						 B#title_basics.genres]),
				      [ID|Acc]
			      end
		      end
	      end
      end, [], title_ratings).

principal_filter(ID, F) ->
    if F#filter.name =:= "" -> false;
       true -> 
	    case principal_match({ID,1}, F) of
		true -> false;
		false -> true
	    end
    end.

principal_match(Key={ID,Order}, F) ->
    case imdb:lookup(title_principals, Key) of
	[] -> false;
	[P] ->
	    case principal_match_(P, F) of
		true -> true;
		false -> principal_match({ID,Order+1},F)
	    end
    end.

principal_match_(P, F) ->
    if F#filter.category =/= [] ->
	    case lists:member(P#title_principals.category,F#filter.category) of
		true ->
		    case imdb:lookup(name_basics,P#title_principals.nconst) of
			[N] ->
			    not name_filter(N#name_basics.primaryName, F);
			[] ->
			    io:format("missing name nm~07w\n", 
				      [P#title_principals.nconst]),
			    false
		    end;
			    
		false ->
		    false
	    end;
       true -> %% any category
	    case imdb:lookup(name_basics, P#title_principals.nconst) of
		[N] ->
		    not name_filter(N#name_basics.primaryName, F);
		[] ->
		    io:format("missing name nm~07w\n", 
			      [P#title_principals.nconst]),
		    false
	    end
    end.

basics_filter(B, Cond) ->
    if B#title_basics.startYear < Cond#filter.min_year -> true;
       B#title_basics.startYear > Cond#filter.max_year -> true;
       true ->
	    case include_filter(B, Cond) of
	    true -> true;
	    false ->
		case exclude_filter(B, Cond) of
		true -> true;
		false ->
		    case genres_filter(B, Cond) of
		    true -> true;
		    false ->
		       title_filter(B#title_basics.primaryTitle, Cond)
		    end
		end
	    end
    end.

ratings_filter(R, Cond) ->
    if R#title_ratings.numVotes < Cond#filter.min_votes -> true;
       R#title_ratings.numVotes > Cond#filter.max_votes -> true;
       R#title_ratings.averageRating < Cond#filter.min_rating -> true;
       R#title_ratings.averageRating > Cond#filter.max_rating -> true;
       true -> false
    end.

ratings_factor() ->
    [put(Y,{0,0}) || Y <- lists:seq(1990, 2017)],
    ets:foldl(
      fun(R, Acc) ->
	      ID = R#title_ratings.tconst,
	      [B] = imdb:lookup(title_basics, ID),
	      Y = B#title_basics.startYear,
	      if B#title_basics.titleType =:= movie ->
		      case get(Y) of
			  undefined -> 
			      Acc;
			  {SumRatings,NVotes} ->
			      N = R#title_ratings.numVotes,
			      NVotes1 = NVotes+N,
			      SumRatings1 = SumRatings+
				  R#title_ratings.averageRating*N,
			      put(Y, {SumRatings1,NVotes1});
			  _ ->
			      Acc
		      end;
		 true ->
		      Acc
	      end
      end, [], title_ratings),
    [{Y,get(Y)} || Y <- lists:seq(1990, 2017)].

