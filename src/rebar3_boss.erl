-module(rebar3_boss).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    inets:start(httpc, [{profile, hex}]),
    rebar3_hex_http:maybe_setup_proxy(),
    lists:foldl(fun provider_init/2, {ok, State}, [rebar3_boss_compile
                                                  ,rebar3_boss_start
                                                  ,rebar3_boss_start_dev
                                                  ,rebar3_boss_stop
                                                  %,rebar3_boss_reload
                                                  %,rebar3_boss_attach
                                                  ]).

provider_init(Module, {ok, State}) ->
Module:init(State).
