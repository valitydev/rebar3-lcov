-module(rebar3_lcov_prv).

-ignore_xref([do/1]).
-ignore_xref([format_error/1]).

-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-define(DEFAULT_COVERAGE_FILES, ["eunit.coverdata", "ct.coverdata"]).
-define(DEFAULT_LCOV_FILENAME, "lcov.info").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {module, ?MODULE},
        {namespace, lcov},
        {name, convert},
        {deps, [{default, app_discovery}]},
        {example, "rebar3 lcov convert"},
        {short_desc, "Converts erlang .coverdata to lcov tracefile format"},
        {desc,
            "Coverts the rebar3 cover generated .coverdata files, "
            "into lcov tracefile format files, and saves the results "
            "to _build/test/lcov/lcov.info file (by default)."},
        {opts, lcov_opts(State)},
        {profiles, [test]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, term()}.
do(State) ->
    OutputFile = output_file(State),
    InputFiles = input_files(State),
    Apps = get_apps(State),
    try generate(State, OutputFile, InputFiles, Apps) of
        ok ->
            {ok, State}
    catch
        throw:Error ->
            {error, {?MODULE, Error}}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%

generate(State, OutputFilePath, InputFiles, Apps) ->
    ok = ensure_output_path(OutputFilePath),
    ok = import_app_coverage(InputFiles),
    CoverageByModule = analyze_app_coverage(Apps, State),
    {ok, OutputFile} = file:open(OutputFilePath, [write]),
    ok = write_app_coverage(CoverageByModule, OutputFile),
    ok = file:close(OutputFile),
    ok.

ensure_output_path(OutputFilePath) ->
    _ = file:delete(OutputFilePath),
    _ = filelib:ensure_dir(filename:join(filename:dirname(OutputFilePath), "temp")),
    ok.

import_app_coverage([]) ->
    ok;
import_app_coverage([File | Rest]) ->
    _ = rebar_api:info("Importing file ~s", [File]),
    case cover:import(File) of
        ok -> import_app_coverage(Rest);
        Error -> Error
    end.

analyze_app_coverage(Apps, State) ->
    Modules = cover:imported_modules(),
    _ = rebar_api:debug("Analyzing coverage of ~p", [Modules]),
    {result, Result, _} = cover:analyse(Modules, calls, line),
    convert_app_coverage(get_ebin_paths(Apps, State), Result).

get_ebin_paths(Apps, State) ->
    lists:map(
        fun(App) ->
            EBin = rebar_app_info:ebin_dir(App),
            rebar_dir:make_relative_path(EBin, rebar_dir:root_dir(State))
        end,
        Apps
    ).

write_app_coverage([], _OutputFile) ->
    ok;
write_app_coverage([{Filename, CoverageArray} | Rest], OutputFile) ->
    _ = rebar_api:info("Writing coverage data for ~s", [Filename]),
    CoverageDict = array:to_orddict(CoverageArray),
    InstrumentedLines = [{N, C} || {N, C} <- CoverageDict, C =/= null],
    LinesWithCoverage = [{N, C} || {N, C} <- InstrumentedLines, C > 0],
    ok = write_coverage_record(
        Filename,
        InstrumentedLines,
        length(LinesWithCoverage),
        length(InstrumentedLines),
        OutputFile
    ),
    write_app_coverage(Rest, OutputFile).

write_coverage_record(SF, DAList, LH, LF, OutputFile) ->
    %% https://manpages.debian.org/bullseye/lcov/geninfo.1.en.html#FILES
    _ = io:format(OutputFile, "SF:~s~n", [SF]),
    _ = lists:foreach(
        fun({LineNumber, Count}) ->
            io:format(OutputFile, "DA:~p,~p~n", [LineNumber, Count])
        end,
        DAList
    ),
    _ = io:format(OutputFile, "LH:~p~n", [LH]),
    _ = io:format(OutputFile, "LF:~p~n", [LF]),
    _ = io:format(OutputFile, "end_of_record~n", []),
    ok.

convert_app_coverage(EbinPaths, CoverageData) ->
    CoverageByModule = get_coverage_by_module(CoverageData),
    CoverageByFilename = map_source_paths(EbinPaths, CoverageByModule),
    maps:to_list(CoverageByFilename).

get_coverage_by_module(CoverageData) ->
    lists:foldl(fun add_cover_line/2, #{}, CoverageData).

add_cover_line({{Module, LineNumber}, CallAmount}, Acc) ->
    CallsPerLine = maps:get(Module, Acc, array:new({default, null})),
    Acc#{Module => array:set(LineNumber, CallAmount, CallsPerLine)}.

map_source_paths(EbinPaths, CoverageByModule) ->
    _ = rebar_api:debug("Mapping source paths...", []),
    maps:fold(
        fun(ModuleName, CoverageData, AccIn) ->
            SourcePath = get_source_path(EbinPaths, ModuleName),
            case AccIn of
                #{SourcePath := OtherCoverageData} ->
                    _ = rebar_api:error(
                        "Duplicate source path! Path: ~p, Existing coverage: ~n~p",
                        [SourcePath, OtherCoverageData]
                    ),
                    throw({duplicate_source_path, {SourcePath, OtherCoverageData}});
                _ ->
                    AccIn#{SourcePath => CoverageData}
            end
        end,
        #{},
        CoverageByModule
    ).

get_source_path([Dir | Rest], ModuleName) ->
    Beam = io_lib:format("~s/~s.beam", [Dir, ModuleName]),
    case beam_lib:chunks(Beam, [compile_info]) of
        {ok, {ModuleName, [{compile_info, CompileInfo}]}} ->
            case proplists:get_value(source, CompileInfo) of
                undefined ->
                    _ = rebar_api:error(
                        "Could not find source path in beam file ~s for module: ~p",
                        [Beam, ModuleName]
                    ),
                    throw({source_path_not_found, {in_ebin, Beam, ModuleName}});
                Path ->
                    Path
            end;
        _ ->
            get_source_path(Rest, ModuleName)
    end;
get_source_path(_, ModuleName) ->
    _ = rebar_api:error(
        "Could not find source path for module: ~p",
        [ModuleName]
    ),
    throw({source_path_not_found, {anywhere, ModuleName}}).

%%

lcov_opts(_State) ->
    [].

input_files(State) ->
    CoverDataFiles = coverdata_files(State),
    CoverDir = filename:join([rebar_dir:base_dir(State), "cover"]),
    ok = filelib:ensure_dir(filename:join([CoverDir, "dummy.log"])),
    {ok, Files} = rebar_utils:list_dir(CoverDir),
    rebar_utils:filtermap(
        fun(FileName) ->
            case lists:member(FileName, CoverDataFiles) of
                true -> {true, filename:join([CoverDir, FileName])};
                false -> false
            end
        end,
        Files
    ).

output_file(State) ->
    Config = config_opts(State),
    proplists:get_value(output_file, Config, default_output_file(State)).

default_output_file(State) ->
    ProfileDir = rebar_dir:base_dir(State),
    filename:join([ProfileDir, "lcov", ?DEFAULT_LCOV_FILENAME]).

get_apps(State) ->
    rebar_state:project_apps(State).

coverdata_files(State) ->
    Config = config_opts(State),
    proplists:get_value(coverdata_files, Config, ?DEFAULT_COVERAGE_FILES).

config_opts(State) ->
    rebar_state:get(State, lcov, []).
