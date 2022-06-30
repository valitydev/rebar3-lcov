-module(rebar3_lcov).

-ignore_xref([init/1]).
-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_lcov_prv:init(State),
    {ok, State1}.
