-module(eqipai_mahjong_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

mahjong_test_() ->
    [?_assert(element(1, eqipai_mahjong:is_hu(gen_mahjongs1())) == true),
     ?_assert(element(1, eqipai_mahjong:is_hu(gen_mahjongs2())) == true),
     ?_assert(eqipai_mahjong:is_hu(gen_mahjongs3()) == false),
     ?_assert(eqipai_mahjong:is_hu(gen_mahjongs4()) == false),
     ?_assert(element(1, eqipai_mahjong:is_hu(gen_mahjongs5())) == true)
    ].

%% true 一万，二万，三万，两条，两条
gen_mahjongs1() ->
    Info = #{wang => [2, 1, 3],
             tiao => [2, 2]
            },
    gen_mahjongs(Info).

%% true 11123455678999万
gen_mahjongs2() ->
    Info = #{wang => [1,1,1,2,3,4,5,5,6,7,8,9,9,9]},
    gen_mahjongs(Info).

%% false 123万，东南西，中中
gen_mahjongs3() ->
    Info = #{wang => [1,2,3],
             zi => [1,2,3,5,5]
            },
    gen_mahjongs(Info).

%% false 12333万，1223条，中中
gen_mahjongs4() ->
    Info = #{wang => [1,2,3,3,3],
             tiao => [1,2,2,3],
             zi => [5,5]
            },
    gen_mahjongs(Info).

gen_mahjongs5() ->
    Info = #{wang => [1,2,2,3,3,4,5,5]},
    gen_mahjongs(Info).

gen_mahjongs(MahjongInfos) ->
    TypeMahjongs = [format_mahjongs(Type, MahjongIndexs) || {Type, MahjongIndexs} <- maps:to_list(MahjongInfos)],
    lists:concat(TypeMahjongs).

format_mahjongs(Type, MahjongIndexs) ->
    [#{type => Type, index => Index} || Index <- MahjongIndexs].
