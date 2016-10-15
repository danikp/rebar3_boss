-module(rebar3_boss).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_boss_prv:init(State),
    {ok, State1}.
