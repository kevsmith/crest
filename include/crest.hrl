-compile([{parse_transform, lager_transform}]).

-include("crest_types.hrl").

-define(CREST_VALUE(Name), list_to_binary([<<"/value/">>, Name])).
-define(CREST_BARRIER(Name), list_to_binary([<<"/barrier/">>, Name])).

-define(EXPECTS_HEADER, "X-Crest-Expects").
