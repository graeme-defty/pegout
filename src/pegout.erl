-module(pegout).
-author("Graeme Defty <graeme.defty@gmail.com>").
-export([file/1, file/2]).

%% @doc Generates a parser from the specified file.
%% @equiv file(Filename, [])
%% @spec file(Filename::string()) -> ok
file(help) ->
    io:format("Useful help text~n",[]);

file(InputGrammar) ->
    file(InputGrammar, []).

file(InputGrammar, Options) ->
    io:format("-- Pegout --   version: ~p~n",[proplists:get_value(time,module_info(compile))]),
    Basename = filename:basename(InputGrammar, ".peg"),
    InputDir = filename:dirname(InputGrammar),
    OutputDir = proplists:get_value(output, Options, InputDir),
    OutputFilename = filename:join(OutputDir, Basename ++ ".html"),
    validate_params(filename:absname(InputGrammar),
                    filename:absname(OutputFilename)),
    Rslt = parse_grammar(InputGrammar),
    file:write_file(OutputFilename, Rslt).

validate_params(InputGrammar, OutputFile) when InputGrammar =:= OutputFile ->
    throw({badarg, "Input and output file are the same!"});
validate_params(_, _) -> ok.

parse_grammar(InputFile) ->
    case pegout_parse:file(InputFile) of
        {fail, Index} ->
            throw({grammar_error, {fail, Index}});
        {Parsed, Remainder, Index} ->
            io:format("WARNING: Grammar parse ended unexpectedly at ~p, generated parser may be incorrect.~nRemainder:~n~p",
                      [Index, Remainder]),
            Parsed;
        L when is_list(L) -> L;
        _ -> throw({error, {unknown, grammar, InputFile}})
    end.

