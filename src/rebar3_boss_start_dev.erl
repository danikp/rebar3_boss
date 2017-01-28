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

  AppName    = app_name(AppFile),
  NameArg    = vm_name_arg(BossConf, AppFile),
  ErlCmd    = erl_command(),
  EbinDirs    = all_ebin_dirs(BossConf, AppFile),
  CookieOpt    = cookie_option(BossConf),
  VmArgs    = vm_args(BossConf),
  io:format("~s -pa ~s -boss developing_app ~s -boot start_sasl -config boss ~s -s reloader -s lager -s boss ~s~s~n",
    [ErlCmd, string:join(EbinDirs, " -pa "), AppName, CookieOpt, NameArg, VmArgs]),
    {ok, State}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================


option(Opt, BossDbOpts) ->
    proplists:get_value(Opt, BossDbOpts, option_default(Opt)).


option_default(model_dir) -> "src/controller";
option_default(out_dir)  -> "ebin";
option_default(source_ext) -> ".erl";
option_default(recursive) -> false;
option_default(compiler_options) -> [verbose, return_errors].


compiler_options(ErlOpts, BossDbOpts) ->
    set_debug_info_option(proplists:get_value(debug_info, ErlOpts), option(compiler_options, BossDbOpts)).


set_debug_info_option(true, BossCompilerOptions) ->
    [debug_info | BossCompilerOptions];
set_debug_info_option(undefined, BossCompilerOptions) ->
    BossCompilerOptions.


compile_model(Source, Target, BossDbOpts, RebarConfig) ->
    ErlOpts = proplists:unfold(rebar_opts:get(RebarConfig, erl_opts, [])),

    RecordCompilerOpts = [
        {out_dir, filename:dirname(Target)},
        {compiler_options, compiler_options(ErlOpts, BossDbOpts)}
    ],

    rebar_api:debug("Compiling boss controller \"~s\" -> \"~s\" with options:~n    ~s",
                    [Source, Target, io_lib:format("~p", [BossDbOpts])]),

    case boss_record_compiler:compile(Source, RecordCompilerOpts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Source, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, RecordCompilerOpts)
    end.
