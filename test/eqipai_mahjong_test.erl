-module(eqipai_mahjong_test).
-include_lib("eunit/include/eunit.hrl").

mahjong_test_() ->
    [?_assert(eqipai_mahjong:is_hu(gen_mahjongs1()) == true),
     ?_assert(eqipai_mahjong:is_hu(gen_mahjongs2()) == true),
     ?_assert(eqipai_mahjong:is_hu(gen_mahjongs3()) == false),
     ?_assert(eqipai_mahjong:is_hu(gen_mahjongs4()) == false)
    ].

%% true 一万，二万，三万，两条，两条
gen_mahjongs1() ->
    [#{type => wang, index => 1},
     #{type => wang, index => 2},
     #{type => tiao, index => 2},
     #{type => wang, index => 3},
     #{type => tiao, index => 2}
    ].
%% true 11123455678999万
gen_mahjongs2() ->
    [#{type => wang, index => 1},
     #{type => wang, index => 1},
     #{type => wang, index => 1},
     #{type => wang, index => 2},
     #{type => wang, index => 3},
     #{type => wang, index => 4},
     #{type => wang, index => 5},
     #{type => wang, index => 5},
     #{type => wang, index => 9},
     #{type => wang, index => 6},
     #{type => wang, index => 7},
     #{type => wang, index => 8},
     #{type => wang, index => 9},
     #{type => wang, index => 9}
    ].

%% false 123万，东南西，中中
gen_mahjongs3() ->
    [#{type => wang, index => 1},
     #{type => wang, index => 2},
     #{type => wang, index => 3},
     #{type => zi, index => 1},
     #{type => zi, index => 2},
     #{type => zi, index => 3},
     #{type => zi, index => 5},
     #{type => zi, index => 5}
    ].

%% false 12333万，1223条，中中
gen_mahjongs4() ->
    [#{type => wang, index => 1},
     #{type => zi, index => 5},
     #{type => wang, index => 2},
     #{type => wang, index => 3},
     #{type => wang, index => 3},
     #{type => wang, index => 3},
     #{type => tiao, index => 1},
     #{type => tiao, index => 2},
     #{type => tiao, index => 2},
     #{type => tiao, index => 3},
     #{type => zi, index => 5}
    ].
