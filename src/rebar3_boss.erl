-module(rebar3_boss).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    lists:foldl(fun provider_init/2, {ok, State}, [
        rebar3_boss_compile
        ,rebar3_boss_start
        ,rebar3_boss_start_dev
        ,rebar3_boss_stop
        %,rebar3_boss_reload
        %,rebar3_boss_attach
    ]).

provider_init(Module, {ok, State}) ->
    Module:init(State).
