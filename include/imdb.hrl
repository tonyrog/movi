-ifndef(__IMDB_HRL__).
-define(__IMDB_HRL__, true).

%%
%% attribues in exclude include: 
%%   videon_game,video,tv, {part,N}
%%   tv_series, {episode,E}, {season,S}, {episode_name,Name}
%%

-record(item,
	{
	  title,
	  attributes,
	  votes,
	  rating,
	  dist,
	  new
	}).

-record(filter,
	{
	  min_votes = 0,
	  max_votes = 10000000000,
	  min_rating = 0.0,
	  max_rating = 10.0,
	  min_year = 0000,
	  max_year = 9999,
	  include = [],   %% attributes that must be present
	  exclude = [],   %% attributes that must not be present
	  title   = "",   %% match title
	  title_compiled  %% compiled pattern
	  }).

-endif.
