-module(saloon).

-export([start/0]).

start() ->
  compile_views(code:priv_dir(site)),
  saloon_app:start().

compile_views(Path) ->
  AbsPath = filename:absname_join(filename:absname(Path), "templates"),
  EbinDir = filename:absname_join(filename:absname(Path), "../ebin"),
  Templates = filelib:wildcard(AbsPath ++ "/*.dtl"),
  lists:foldl(fun(File, _) ->
                  FileName = filename:basename(File, ".dtl"),
                  ViewName = list_to_atom(FileName),
                  erlydtl:compile(
                    File,
                    ViewName,
                    [ {out_dir, EbinDir}
                    , {custom_tags_modules, [saloon_lang]}]
                   )
              end, ok, Templates).
