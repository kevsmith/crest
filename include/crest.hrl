-compile([{parse_transform, lager_transform}]).

-type crest_entity_ref() :: pid() | binary().
-type crest_entity_name() :: binary().

-define(CREST_VALUE(Name), list_to_binary([<<"/value/">>, Name])).
-define(CREST_BARRIER(Name), list_to_binary([<<"/barrier/">>, Name])).
