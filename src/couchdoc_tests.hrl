-include_lib("eunit/include/eunit.hrl").

strip_couch_obj_test() ->
    Strip = fun strip_couch_obj/1,

    ?assertEqual([], Strip([])),
    ?assertEqual(1, Strip(1)),
    ?assertEqual(undefined, Strip(undefined)),
    ?assertEqual("hello", Strip("hello")),

    ?assertEqual(foo, Strip({foo})),
    ?assertEqual("hello", Strip({"hello"})),
    ?assertEqual([{foo, bar}], Strip({[{foo, bar}]})),
    ?assertEqual([{<<"foo">>, <<"bar">>}], Strip({[{<<"foo">>, <<"bar">>}]})),

    ?assertEqual([{"foo", "bar"},
                  {"baz", [{"name", "Baz"}, 
                           {"meaning", "Notta"}]}],
                 Strip({[{"foo", "bar"},
                         {"baz", {[{"name", "Baz"}, 
                                   {"meaning", "Notta"}]}}]})),

    ?assertEqual([{<<"foo">>, <<"bar">>},
                  {<<"baz">>, [{<<"name">>, <<"Baz">>}, 
                               {<<"meaning">>, <<"Notta">>}]}],
                 Strip({[{<<"foo">>, <<"bar">>},
                         {<<"baz">>, {[{<<"name">>, <<"Baz">>}, 
                                       {<<"meaning">>, <<"Notta">>}]}}]})),
    
    ?assertEqual([{foo, [{bar, [{baz, [{bam, "Bam!"}]}]}]}],
                 Strip({[{foo, {[{bar, {[{baz, {[{bam, "Bam!"}]}}]}}]}}]})),
    ok.

wrap_couch_obj_test() ->
    W = fun wrap_couch_obj/1,

    ?assertEqual(1, W(1)),
    ?assertEqual(foo, W(foo)),
    ?assertEqual("hello", W("hello")),
    ?assertEqual([foo, bar, {baz, bam}], W([foo, bar, {baz, bam}])),

    ?assertEqual({[]}, W([])),
    ?assertEqual({[{name, "George"}, {height,72}]}, 
                 W([{name, "George"}, {height, 72}])),

    ?assertEqual({[{foo, {[{bar, {[{baz, {[{bam, "Bam!"}]}}]}}]}}]}, 
                 W([{foo, [{bar, [{baz, [{bam, "Bam!"}]}]}]}])),

    ok.
