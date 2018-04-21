-module(rebar3_boss_compile).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, boss).
-define(DEPS, [compile]).

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
        {example, "rebar3 CB compile"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "Compile ChicagoBoss projects."},
        {desc, "Compile ChicagoBoss projects."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running boss...", []),
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
        Opts = rebar_app_info:opts(AppInfo),
        OutDir = rebar_app_info:ebin_dir(AppInfo),

        filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),

        BossOpts = proplists:unfold(rebar_opts:get(Opts, boss_opts, [])),

        SourceDir = option(source_dir, BossOpts),
        SourceExt = option(source_ext, BossOpts),
        TargetExt = ".beam",
        rebar_base_compiler:run(Opts, [],
            SourceDir,
            SourceExt,
            OutDir,
            TargetExt,
            fun(S, T, _C) ->
                compile_model(S, T, BossOpts, Opts)
            end,
            [{check_last_mod, true}, {recursive, option(recursive, BossOpts)}])
     end || AppInfo <- Apps],
    {ok, State}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================


option(Opt, BossOpts) ->
    proplists:get_value(Opt, BossOpts, option_default(Opt)).


option_default(source_dir) -> "src/controller";
option_default(out_dir)  -> "ebin";
option_default(source_ext) -> ".erl";
option_default(recursive) -> true;
option_default(compiler_options) -> [verbose, return_errors].


compiler_options(ErlOpts, BossOpts) ->
    set_debug_info_option(proplists:get_value(debug_info, ErlOpts), option(compiler_options, BossOpts)).


set_debug_info_option(true, BossCompilerOptions) ->
    [debug_info | BossCompilerOptions];
set_debug_info_option(undefined, BossCompilerOptions) ->
    BossCompilerOptions.


compile_model(Source, Target, BossOpts, RebarConfig) ->
    ErlOpts = proplists:unfold(rebar_opts:get(RebarConfig, erl_opts, [])),

    ControllerCompilerOpts = [
        {out_dir, filename:dirname(Target)},
        {compiler_options, compiler_options(ErlOpts, BossOpts)}
    ],

    rebar_api:debug("Compiling boss controller \"~s\" -> \"~s\" with options:~n    ~s",
                    [Source, Target, io_lib:format("~p", [BossOpts])]),

    case boss_controller_compiler:compile(Source, ControllerCompilerOpts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Source, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, ControllerCompilerOpts)
    end.
