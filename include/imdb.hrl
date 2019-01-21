-ifndef(__IMDB_HRL__).
-define(__IMDB_HRL__, true).


-define(CATEGORY_undefined,  0).
-define(CATEGORY_self,       1).
-define(CATEGORY_director,   2).
-define(CATEGORY_composer,   3).
-define(CATEGORY_writer,     4).
-define(CATEGORY_editor,     5).
-define(CATEGORY_actor,      6).
-define(CATEGORY_actress,    7).
-define(CATEGORY_producer,   8).
-define(CATEGORY_production_designer, 9).
-define(CATEGORY_cinematographer, 10).
-define(CATEGORY_archive_footage, 11).
-define(CATEGORY_archive_sound, 12).

-define(JOB_undefined, 0).
-define(JOB_self, 1).
-define(JOB_director, 2).
-define(JOB_co_director, 3).
-define(JOB_writer, 4).
-define(JOB_co_writer, 5).
-define(JOB_creator, 6).
-define(JOB_co_creator, 7).
-define(JOB_producer, 8).
-define(JOB_co_producer, 9).
-define(JOB_executive_producer, 10).

-define(JOB_composer, 11).
-define(JOB_co_composer, 12).
-define(JOB_director_of_photography, 13).
-define(JOB_novel, 14).
-define(JOB_story, 15).
-define(JOB_original_story, 16).
-define(JOB_short_story, 17).
-define(JOB_poem, 18).
-define(JOB_poems, 19).
-define(JOB_memoirs, 20).

-define(JOB_play, 21).
-define(JOB_plays, 22).
-define(JOB_tale, 23).
-define(JOB_opera, 24).
-define(JOB_titles, 25).
-define(JOB_teleplay, 26).
-define(JOB_idea, 27).
-define(JOB_dialogue, 28).
-define(JOB_commentary, 29).
-define(JOB_screen_play, 30).

-define(JOB_original_screen_play, 31).
-define(JOB_comic_strip, 32).
-define(JOB_comedy, 33).
-define(JOB_scenario, 34).
-define(JOB_novella, 35).
-define(JOB_adaptation, 36).
-define(JOB_book, 37).
-define(JOB_fable, 38).
-define(JOB_translation, 39).

-define(JOB_main_characters, 40).
-define(JOB_novels, 41).
-define(JOB_based_on_a_play_by, 42).
-define(JOB_based_on_a_story_by, 43).
-define(JOB_based_on_characters_by, 44).
-define(JOB_based_on_a_novel_by, 45).
-define(JOB_adapted_from_the_play_by, 46).
-define(JOB_written_by, 47).
-define(JOB_contributing_writer, 48).
-define(JOB_staff_writer, 49).

-define(JOB_unused_screenplay, 50).
-define(JOB_quotation, 51).
-define(JOB_stories, 52).
-define(JOB_manuscript, 53).
-define(JOB_unfinished_novel, 54).
-define(JOB_unconfirmed, 55).
-define(JOB_character, 56).
-define(JOB_libertto, 57).
-define(JOB_photoplay, 58).
-define(JOB_operetta, 59).

-define(JOB_by, 60).
-define(JOB_after, 61).
-define(JOB_video_editor, 62).
-define(JOB_opera_libretto, 63).
-define(JOB_adapted_from_his_novel, 64).

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
