WHAT IS IT?
===========

Have you ever tried [Hoogle](http://www.haskell.org/hoogle/)?
 
If you haven't, here's a brief description. 

Quite often you don't know the name of a function, or the name of the module it resides in, but you know what types you'd like this function to accept and what types this function will output.

Hoogle let's you search based on type signatures of functions. One small problem though... It's Haskell only, no Erlang.

Wouldn't it be nice to type in something like

       proplists:?(any(), list()) -> any()

and get back

       proplists:get_value

Or type in 

       "(erlang:timestamp(),erlang:timestamp())->integer()"

and get back

       erlang:now_diff

?

That's the goal of HINT: to provide you with a way to search for function definitions and documentation by function type signatures.

DEMO
====

1. Start web.sh
2. Go to http://127.0.0.1/52012

Give some time for web.sh to start completely. If it doesn't work, clean && compile && run.

3. Demo URLs:  
- http://booktu.com
- http://dmitriid.com:52012


HOW TOs
=======

Supply PLT
----------------

By defaut PLT from `$HOME` dir will be used. 
It's also possible to specify path in OTP's `app.config` file.

### Generate PLT

In order to generate a tiny PLT, run the following in your shell:
       `dialyzer --build_plt --apps kernel stdlib erts crypto sasl --output_plt ~/.dialyzer_plt`


See some results on the command line
------------------------------------

In `$PROJECT_ROOT`:

       bash> erl -pa apps/hint_search/ebin
       > hint_search:start().
       > hint_search:q("(erlang:timestamp(),erlang:timestamp())->integer()").
       {ok,[{{timer,now_diff,2},1.5},
            {{dets,init,2},0.9},
            {{crypto,dh_generate_parameters,2},0.9},
            {{dets,next,2},0.9},
       ...

Configuration
=============

TODO 

Technical details
=================

Frontend:

- Cowboy web-server
- Custom controllers
- Regular web stuff

Backend:

- derive Module, Funciton, Arity from user request
- create possible rotations of these
- obtain list of known modules and funcitons from provided bif.tab and PLT
- filter out funcitons with wrong arity
- call Dialyzer to compare types
- create a ranking of Module-Function pairs based on Dialyzer output
- Fuzzy search on provided names
- return these to the user
