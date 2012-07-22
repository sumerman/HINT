-module(hs_biftab_plt).
-export([tabs_to_plt/0, 
         tabs_to_plt/1, 
         tab_to_plt/1]).

defult_tab_ext() -> ".tab".

tab_dir(App) when is_atom(App) ->
  code:priv_dir(App);
tab_dir(Path) -> Path.

tab_paths(Srcs) ->
  FilesInTabDirs = lists:concat(lists:map(
        fun(S) ->
            Dir = tab_dir(S),
            {ok, Files} = file:list_dir(Dir), 
            [filename:join(Dir, F) || F <- Files]
        end, Srcs)),
  [FName || FName <- FilesInTabDirs, 
            filename:extension(FName) == defult_tab_ext()].

tabs_to_plt() ->
  {ok, App} = application:get_application(),
  tabs_to_plt([App]).

tabs_to_plt(Srcs) ->
  dialyzer_plt:merge_plts([tab_to_plt(T) || T <- tab_paths(Srcs)]).

tab_to_plt(Path) ->
  MFAs = parse_tab(Path),
  NPLT = dialyzer_plt:new(),
  lists:foldl(
    fun({M,F,A}=MFA, PLT) ->
        case erl_bif_types:is_known(M,F,A) of
          true ->
            RetT = erl_bif_types:type(M,F,A),
            ArgT = erl_bif_types:arg_types(M,F,A),
            dialyzer_plt:insert_list(PLT, [{MFA, {RetT, ArgT}}]);
          false ->
            PLT
        end
    end, NPLT, MFAs).

parse_tab(Path) ->
  {ok, Lines} = readlines(Path),
  {ok, Rex}   = line_regex(),
  Parsed = lists:map(
      fun(L) ->
          parse_line(Rex, L)
      end, Lines),
  lists:filter(fun defined_only/1, Parsed).

parse_line(Rex, Line) ->
  case re:run(Line, Rex, [{capture, [1,2,3], binary}]) of
    {match, [M,F,A]} -> 
      {binary_to_atom(M, utf8), 
       binary_to_atom(F, utf8),
       list_to_integer(binary_to_list(A))};
    _Else -> undefined
  end.

line_regex() ->
  re:compile("^u?bif '?([^']+)'?:(.+)/(\\d).*$", [unicode]).

defined_only(undefined) -> false;
defined_only(_)         -> true.

-define(AHEAD_BUF_SZ, 1024*1024).
-define(OPEN_OPTS, [read, binary, raw,
                    {read_ahead, ?AHEAD_BUF_SZ}]).

readlines(FName) ->
  {ok, F} = file:open(FName, ?OPEN_OPTS),
  R  = readlines(F, []),
  ok = file:close(F),
  {ok, R}.
readlines(F, R) ->
  case file:read_line(F) of
    {ok, Line} -> 
      readlines(F, [Line|R]);
    eof ->
      lists:reverse(R)
  end.

