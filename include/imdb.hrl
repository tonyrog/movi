-ifndef(__IMDB_HRL__).
-define(__IMDB_HRL__, true).

%% actually used as ENUM
-record(category,
	{
	  self,
	  director,
	  composer,
	  writer,
	  editor,
	  actor,
	  actress,
	  producer,
	  production_designer,
	  cinematographer,
	  archive_footage,
	  archive_sound
	}).

%% this is an enum!
-record(job,
	{
	  self,
	  director,
	  co_director,
	  writer,
	  co_writer,
	  creator,
	  co_creator,
	  producer,
	  co_producer,
	  executive_producer,
	  composer,
	  co_composer,
	  director_of_photography,
	  novel,
	  story,
	  original_story,
	  short_story,
	  poem,
	  poems,
	  memoirs,
	  play,
	  plays,
	  tale,
	  opera,
	  titles,
	  teleplay,
	  idea,
	  dialogue,
	  commentary,
	  screen_play,
	  original_screen_play,
	  comic_strip,
	  comedy,
	  scenario,
	  novella,
	  adaptation,
	  book,
	  fable,
	  translation,
	  main_characters,
	  novels,
	  based_on_a_play_by,
	  based_on_a_story_by,
	  based_on_characters_by,
	  based_on_a_novel_by,
	  adapted_from_the_play_by,
	  written_by,
	  contributing_writer,
	  staff_writer,
	  unused_screenplay,
	  quotation,
	  stories,
	  manuscript,
	  unfinished_novel,
	  unconfirmed,
	  character,
	  libertto,
	  photoplay,
	  operetta,
	  by,
	  'after',
	  video_editor,
	  opera_libretto,
	  adapted_from_his_novel
	}).


%% name.basics.tsv.gz
-record(name_basics, {
	  nconst,            %% nconst
	  primaryName,       %% string
	  birthYear,         %% year
	  deathYear,         %% year
	  primaryProfession, %% [atom]
	  knownForTitles     %% [tconst]
	 }).

%% title.akas.tsv.gz
-record(title_akas, {
	  tconst,            %% tconst
	  ordering,          %% integer  --  secondary index?
	  title,             %% string
	  region,            %% lang
	  language,          %% lang
	  types,             %% string  -- imdbDisplay|original..
	  attributes,        %% string
	  isOriginalTitle    %% boolean
	  }).

%% title.basics.tsv.gz
-record(title_basics, {
	  tconst,         %% tconst
	  titleType,      %% atom   -- movie|short|...
	  primaryTitle,   %% string
	  originalTitle,  %% string
	  isAdult,        %% boolean
	  startYear,      %% year
	  endYear,        %% year
	  runtimeMinutes, %% integer
	  genres          %% [atom]
	 }).

%% title.crew.tsv.gz
-record(title_crew, {
	  tconst,       %% tconst
	  directors,    %% [nconst]
	  writers       %% [nconst]
	 }).

%% title.episode.tsv.gz
-record(title_episode, {
	  tconst,        %% tconst
	  parentTconst,  %% tconst
	  seasonNumber,  %% integer
	  episodeNumber  %% integer
	 }).

-record(title_principals,
	{
	  tconst,        %% tconst
	  ordering,      %% integer    -- secondary key 1..k
	  nconst,        %% nconst
	  category,      %% enum|string  -- self|director|composer...
	  job,           %% enum|string  -- more like relation (fuzzy)
	  characters     %% string_array -- fishy json array?
	}).

%% title.ratings.tsv.gz
-record(title_ratings, {
	  tconst,        %% tconst
	  averageRating, %% float
	  numVotes       %% integer()
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
	  genres  = [],   %% genres that must be present
	  title   = "",   %% match title
	  title_compiled, %% compiled pattern
	  name    = "",   %% match name
	  name_compiled,  %% compiled name pattern
	  category = []   %% type of name
	}).

-endif.
