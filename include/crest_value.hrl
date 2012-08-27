-type crest_value() :: binary() | number().
-type crest_value_type() :: binary | integer | float.

-type locked_error() :: {error, locked}.
-type type_error()   :: {error, wrong_type}.
-type precond_error()  :: {error, precondition_failed, {crest_value(), crest_value()}}.
-type write_error()  :: locked_error() | type_error() | precond_error().

-type write_term() :: {write, crest_value()} | {write, crest_value(), crest_value()} | {incr, number()} | {decr, number()}.
