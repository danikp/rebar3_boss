-module(rebar3_boss_start_dev).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, start_dev).
-define(NAMESPACE, boss).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================


-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {namespace, ?NAMESPACE},
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 CB start_dev"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "Start ChicagoBoss project in development mode."},
        {desc, "Start ChicagoBoss project in development mode."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_api:info("Generating dynamic start-dev command~n", []),

  % AppName    = app_name(AppFile),
  % NameArg    = vm_name_arg(BossConf, AppFile),
  % ErlCmd    = erl_command(),
  % EbinDirs    = all_ebin_dirs(BossConf, AppFile),
  % CookieOpt    = cookie_option(BossConf),
  % VmArgs    = vm_args(BossConf),
   io:format("~s -pa ~s -boss developing_app ~s -boot start_sasl -config boss ~s -s reloader -s lager -s boss ~s~s~n",
     [ErlCmd, string:join(EbinDirs, " -pa "), AppName, CookieOpt, NameArg, VmArgs]),
    {ok, State}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================

