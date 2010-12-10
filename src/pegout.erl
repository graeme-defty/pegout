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
    file:write_file(OutputFilename, prefix(InputGrammar) ++ Rslt ++ suffix()).

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

prefix(Name) ->
  [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n",
    "<html>\n",
    "<head>\n",
    "<title>Pegout for " ++ Name ++ "</title>\n",
    "<script type=\"text/javascript\">\n",
    "  function toggle(id){\n",
    "    if(document.getElementById){\n",
    "    var el = document.getElementById(id);\n",
    "    el.style.display = (el.style.display == 'none') ? 'block' : 'none';\n",
    "  }}\n",
    "</script>\n",
    "</head>\n<body>\n"].
    
suffix() ->
  [ "</body>\n</html>\n"].

