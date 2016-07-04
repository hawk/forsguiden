#!/usr/bin/env escript

%% Copyright 2015-2016 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%% Generates an forsguiden.xml file from the .csv files
%%
%%   csv2xml [-a [OutFile]]
%%
%% Options
%%   -a      - Emits all data fields

-module(csv2xml).
-mode(compile).

-define(ROW_DELIM, <<"§">>).
-define(COL_DELIM, <<"^">>).

-record(opts, {file = "forsguiden.xml",
               emit_all = false,
               indent = []}).

main(Args) ->
    Opts = parse_args(Args, #opts{}),
    OutFile = Opts#opts.file,
    {RiverHead, Rivers} = split_csv_file("river.csv"),
    Rapids = split_csv_file("rapid.csv"),
    Spots = split_csv_file("spot.csv"),
    Districts0 = sort(<<"district">>, RiverHead, Rivers),
    Districts = organize(<<"district">>, RiverHead, Districts0),
    io:format("Generate file ~s...", [OutFile]),
    Opts2 = indent(Opts),
    Xml =
        [
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n",
         start_tag(Opts, "districts"),
         [emit_district(Opts2, District, RiverHead, Rapids, Spots) ||
             District <- Districts],
         end_tag(Opts, "districts")
        ],
    file:write_file(OutFile, Xml),
    io:format("done.\n", []).

parse_args([H | T], Opts) ->
    case H of
        "-a" ->
            parse_args(T, Opts#opts{emit_all = true});
        "-" ++ _ ->
            io:format("Illegal option: ~s\n", [H]),
            halt(1);
        File ->
            parse_args(T, Opts#opts{file = File})
    end;
parse_args([], Opts) ->
    Opts.

split_csv_file(File0) ->
    File = filename:join(["csv", File0]),
    io:format("Reading file ~s...\n", [File]),
    case file:read_file(File) of
        {ok, Bin} ->
            ok;
        {error, Reason} ->
            io:format("error: ~s: ~s\n", [File, file:format_error(Reason)]),
            erlang:halt(3),
            Bin = <<>>
    end,
    [Mixed|Rest] = binary:split(Bin, ?ROW_DELIM, [global]),
    [Head0,First] = binary:split(Mixed, <<"\n">>, []),
    Head = binary:split(Head0, ?COL_DELIM, [global]),
    Body = [binary:split(Row, ?COL_DELIM, [global]) || Row <- [First|Rest]],
    Len = length(Head),
    Body2 = [Row || Row <- Body,
                    length(Row) =:= Len],
    {Head, Body2}.

emit_district(Opts, {DistrictName, LocalRivers}, RiverHead, Rapids, Spots) ->
    Opts2 = indent(Opts),
    Opts3 = indent(Opts2),
    [
     start_tag(Opts, "district"),
     tag(Opts2, "districtname", DistrictName),
     start_tag(Opts2, "rivers"),
     [emit_river(Opts3, RiverHead, River, Rapids, Spots) ||
         River <- LocalRivers],
     end_tag(Opts2, "rivers"),
     end_tag(Opts, "district")
    ].

emit_river(Opts, RiverHead, River, {RapidHead, Rapids}, Spots) ->
    RiverId = pick(<<"id">>, RiverHead, River),
    LocalRapids =
        [Rapid || Rapid <- Rapids,
                  is_equal(RiverId, <<"riverid">>, RapidHead, Rapid),
                  not is_empty_rapid(RapidHead, Rapid)],
    Sorted = sort(<<"seqnumber">>, RapidHead, LocalRapids),
    Opts2 = indent(Opts),
    Opts3 = indent(Opts2),
    Context = {<<"rivername">>, RiverHead, RiverHead},
    GradeSpan = fun(T, V) -> {replace, T, gradespan(Context, V)} end,
    Transformers =
        [
         {<<"id">>,              skip},
         {<<"district">>,        skip},
         {<<"gradespan">>,       GradeSpan},
         {<<"miscinfo">>,        skip},
         {<<"maptofind">>,       skip},
         {<<"eniro">>,           skip},
         {<<"hitta">>,           skip},
         {<<"aproved">>,         skip},
         {<<"nbrofvotes">>,      skip},
         {<<"bw">>,          fun(T, V) -> activity(T,<<"bigwater">>,    V) end},
         {<<"cr">>,          fun(T, V) -> activity(T,<<"creeking">>,    V) end},
         {<<"fr">>,          fun(T, V) -> activity(T,<<"freestyle">>,   V) end},
         {<<"ny">>,          fun(T, V) -> activity(T,<<"rookie">>,      V) end},
         {<<"sl">>,          fun(T, V) -> activity(T,<<"slalom">>,      V) end},
         {<<"tu">>,          fun(T, V) -> activity(T,<<"riverrunning">>,V) end},
         {<<"lastupdated">>, fun(T, V) -> {replace,T,swe_time(V)}          end}
        ],
    [
     start_tag(Opts, "river"),
     emit_body(Opts2, RiverHead, River, Transformers),
     start_tag(Opts2, "rapids"),
     [emit_rapid(Opts3, RapidHead, Rapid, Spots) || Rapid <- Sorted],
     end_tag(Opts2, "rapids"),
     end_tag(Opts, "river")
    ].

activity(_Tag, AltTag, <<"1">>) ->
    {replace, AltTag, <<"Ja">>};
activity(_Tag, AltTag, <<"0">>) ->
    {replace, AltTag, <<"Nej">>}.

is_empty_rapid(RapidHead, Rapid) ->
    is_equal(<<>>, <<"rapidname">>, RapidHead, Rapid) andalso
    is_equal(<<>>, <<"description">>, RapidHead, Rapid).

emit_rapid(Opts, RapidHead, Rapid, {SpotHead, Spots}) ->
    RapidId = pick(<<"id">>, RapidHead, Rapid),
    LocalSpots =
        [Spot || Spot <- Spots,
                 is_equal(RapidId, <<"rapidid">>, SpotHead, Spot),
                 not is_empty_spot(SpotHead, Spot)],
    Opts2 = indent(Opts),
    Opts3 = indent(Opts2),
    Context = {<<"rapidname">>, RapidHead, Rapid},
    GradeSpan = fun(T, V) -> {replace, T, gradespan(Context, V)} end,
    Transformers =
        [
         {<<"id">>,              skip},
         {<<"riverid">>,         skip},
         {<<"grade">>,           GradeSpan},
         {<<"nbrofvotes">>,      skip},
         {<<"seqnumber">>,       skip},
         {<<"eniro">>,           skip},
         {<<"hitta">>,           skip},
         {<<"rt90_x">>,          skip},
         {<<"rt90_y">>,          skip},
         {<<"wgs84_lat_grad">>,  skip},
         {<<"wgs84_lat_min">>,   skip},
         {<<"wgs84_lat_sek">>,   skip},
         {<<"wgs84_long_grad">>, skip},
         {<<"wgs84_long_min">>,  skip},
         {<<"wgs84_long_sek">>,  skip}
        ],
    [
     start_tag(Opts, "rapid"),
     emit_body(Opts2, RapidHead, Rapid, Transformers),
     start_tag(Opts2, "spots"),
     [emit_spot(Opts3, SpotHead, Spot) || Spot <- LocalSpots],
     end_tag(Opts2, "spots"),
     end_tag(Opts, "rapid")
    ].

is_empty_spot(SpotHead, Spot) ->
    is_equal(<<>>, <<"spotname">>, SpotHead, Spot) andalso
    is_equal(<<>>, <<"description">>, SpotHead, Spot).

emit_spot(Opts, SpotHead, Spot) ->
    Opts2 = indent(Opts),
    Transformers =
        [
         {<<"id">>,              skip},
         {<<"rapidid">>,         skip},
         {<<"nbrofvotes">>,      skip},
         {<<"rating">>,          skip},
         {<<"eniro">>,           skip},
         {<<"hitta">>,           skip},
         {<<"xcoord">>,          skip},
         {<<"ycoord">>,          skip},
         {<<"mapnumber">>,       skip},
         {<<"rt90_x">>,          skip},
         {<<"rt90_x">>,          skip},
         {<<"rt90_y">>,          skip},
         {<<"wgs84_lat_grad">>,  skip},
         {<<"wgs84_lat_min">>,   skip},
         {<<"wgs84_lat_sek">>,   skip},
         {<<"wgs84_long_grad">>, skip},
         {<<"wgs84_long_min">>,  skip},
         {<<"wgs84_long_sek">>,  skip}
        ],
    [
     start_tag(Opts, "spot"),
     emit_body(Opts2, SpotHead, Spot, Transformers),
     end_tag(Opts, "spot")
    ].

emit_body(Opts, [Tag | Head], [Val0 | Vals], Transformers) ->
    Val = sanitize(Tag, Val0),
    Transform = lookup_transform(Tag, Val, Transformers),
    NewVal = eval_transform(Opts, Tag, Val, Transform),
    [
     NewVal,
     emit_body(Opts, Head, Vals, Transformers)
    ];
emit_body(_Opts, [], [], _Transformers) ->
    [].

lookup_transform(Tag, Val, Transformers) ->
    case lists:keyfind(Tag, 1, Transformers) of
        false                             -> keep;
        {_, skip}                         -> skip;
        {_, keep}                         -> keep;
        {_, {replace,T,V}}                -> {replace, T, V};
        {_, Fun} when is_function(Fun, 2) -> Fun(Tag, Val)
    end.

eval_transform(Opts, Tag, Val, Transform) ->
    case Transform of
        skip when Opts#opts.emit_all ->
            eval_transform(Opts, Tag, Val, keep);
        skip ->
            [];
        keep ->
            [tag(Opts, Tag, Val)];
        {replace,NewTag,NewVal} ->
            [tag(Opts, NewTag, sanitize(Tag, NewVal))]
    end.

indent(#opts{indent = Indent} = Opts) ->
    Opts#opts{indent = ["  " | Indent]}.

tag(#opts{indent = Indent}, Tag, IoList) ->
    case iolist_to_binary(IoList) of
        <<>> ->
            [Indent, "<", Tag, "/>\n"];
        Bin ->
            [Indent, "<", Tag, ">", Bin, "</", Tag, ">", "\n"]
    end.

start_tag(#opts{indent = Indent}, Tag) ->
    [Indent, "<", Tag, ">\n"].

end_tag(#opts{indent = Indent}, Tag) ->
    [Indent, "</", Tag, ">\n"].

sort(Key, Head, List) ->
    Pos = pos(Key, Head),
    Tuples = [list_to_tuple(Elem) || Elem <- List],
    Sorted = lists:keysort(Pos, Tuples),
    [tuple_to_list(Tuple) || Tuple <- Sorted].

organize(Tag, Head, List) ->
    Pos = pos(Tag, Head),
    do_organize(Pos, List, undefined, []).

do_organize(Pos, [H|T], PrevKey, Acc) ->
    Key = lists:nth(Pos, H),
    Acc3 =
        if
            Key =/= PrevKey ->
                Acc2 =
                    case Acc of
                        [] ->
                            Acc;
                        [{PrevKey, Prev} | Rest] ->
                            [{PrevKey, lists:reverse(Prev)} | Rest]
                    end,
                [{Key, [H]} | Acc2];
            Key =:= PrevKey ->
                [{Key, Prev}| Rest] = Acc,
                [{Key, [H|Prev]} | Rest]
        end,
    do_organize(Pos, T, Key, Acc3);
do_organize(_Pos, [], _PrevKey, Acc) ->
    lists:reverse(Acc).

is_equal(Key, Tag, Head, Row) ->
    Val = pick(Tag, Head, Row),
    Key =:= Val.

%% pick({Tag, Head, Row} = _Context) ->
%%     pick(Tag, Head, Row).

pick(Tag, Head, Row) ->
    Pos = pos(Tag, Head),
    lists:nth(Pos, Row).

pos(Key, List) ->
    Fun =
        fun(K, Acc) when K =:= Key -> throw({pos, Acc+1});
           (_, Acc)                -> Acc+1
        end,
    try
        lists:foldl(Fun, 0, List),
        io:format("ERROR: Missing ~p in ~p\n", [Key, List]),
        erlang:halt(4)
    catch
        throw:{pos, Val} ->
            Val
    end.

sanitize(_Tag, IoList) ->
    Map =
        [
         {<<"allowfullscreen>">>, <<"allowfullscreen=\"true\">">>},
         {<<"berg&dalbana">>, <<"berg- och dalbana">>},
         {<<"&ndash;">>, <<"-">>},
         {<<"& ">>, <<"och ">>},
         {<<"&hl=en">>, <<"">>},
         {<<"&hl=sv">>, <<"">>},
         {<<"&fs=1">>, <<"">>},
         fun unobjectify/1,
         fun extract_href/1,
         fun extract_img/1,
         fun extract_iframe/1,
         {<<"^\\-+$">>, <<"">>},
         {<<"^\\?+$">>, <<"">>},
         {<<"<p>">>, <<"\n">>},
         {<<"<p/>">>, <<"\n">>},
         {<<"</p>">>, <<"\n">>},
         {<<"<br>">>, <<"\n">>},
         {<<"<BR>">>, <<"">>},
         {<<"<i>">>, <<"">>},
         {<<"</i>">>, <<"">>},
         {<<"<I>">>, <<"">>},
         {<<"</I>">>, <<"">>},
         {<<"<b>">>, <<"">>},
         {<<"</b>">>, <<"">>},
         {<<"<B>">>, <<"">>},
         {<<"</B>">>, <<"">>},
         {<<"\r\n">>, <<"\n">>},
         {<<"\n\n+">>, <<"\n\n">>},
         {<<"^[ \t\n]+">>, <<"">>},
         {<<"[ \t\n]+$">>, <<"">>},
         {<<"[ \t]+">>, <<" ">>},
         fun capitalize/1
        ],
    Opts = [global, {return, binary}],
    Fun = fun({RegExp, With}, From) -> re:replace(From, RegExp, With, Opts);
             (F, From) when is_function(F, 1) -> F(From)
          end,
    Bin = lists:foldl(Fun, iolist_to_binary(IoList), Map),
    case re:run(Bin, <<"[\\<&]">>, [unicode]) of
        nomatch    ->
            Bin;
        {match, _} ->
            %% io:format("CDATA ~tp\n\n", [Bin]),
            <<"<![CDATA[", Bin/binary, "]]>">>
    end.

swe_time(<<Day:2/binary,"/",Mon:2/binary,"/",Year:2/binary,Rest/binary>>) ->
    swe_time(<<Year/binary,"-",Mon/binary,"-",Day/binary,Rest/binary>>);
swe_time(<<Year:2/binary,"-",Mon:2/binary,"-",Day:2/binary," 00:00:00">>) ->
    LongYear = swe_year(Year),
    <<LongYear/binary,"-",Mon/binary,"-",Day/binary>>;
swe_time(DateTime) ->
    DateTime.

swe_year(Year) ->
    IntYear = binary_to_integer(Year),
    if
        IntYear > 50 -> <<"19", Year/binary>>;
        true         -> <<"20", Year/binary>>
    end.

gradespan(Context, GradeSpan) when is_binary(GradeSpan) ->
    gradespan(Context, binary_to_list(GradeSpan));
gradespan(Context, GradeSpan) ->
    case string:tokens([G || G <- GradeSpan, G =/= $\ ], "-") of
        [From, To] ->
            [grade(Context, From), "-", grade(Context, To)];
        [Grade] ->
            grade(Context, Grade);
        [] ->
            grade(Context, "")
    end.

grade(Context, Grade) when is_binary(Grade) ->
    grade(Context, binary_to_list(Grade));
grade(Context, Grade) ->
    case string:tokens([G || G <- Grade, G =/= $\ ], ".") of
        [Difficulty, Danger, Sos] ->
            emit_grade(grade(Context, difficulty, Difficulty),
                       grade(Context, danger, Danger),
                       grade(Context, sos, Sos));
        [Difficulty, DangerSos] ->
            IsInt = fun(C) -> C >= $0 andalso C =< $9 end,
            {Danger, Sos} = lists:splitwith(IsInt, DangerSos),
            emit_grade(grade(Context, difficulty, Difficulty),
                       grade(Context, danger, Danger),
                       grade(Context, sos, Sos));
        [Difficulty] ->
            emit_grade(grade(Context, difficulty, Difficulty),
                       grade(Context, danger, ""),
                       grade(Context, sos, ""));
        [] ->
            emit_grade(grade(Context, difficulty, ""),
                       grade(Context, danger, ""),
                       grade(Context, sos, ""))
    end.

grade(_Context, _, "") ->
    %% ContextName = pick(Context),
    %% io:format("MISSING GRADE ~p\n", [ContextName]),
    "";
grade(Context, difficulty, Grade) ->
    grade(Context, ["1","2","3","4","5","6"], "6", Grade);
grade(Context, danger, Grade) ->
    grade(Context, ["1","2","3","4","5","6"], "6", Grade);
grade(Context, sos, Grade) ->
    grade(Context, ["A","B","C"], "", to_upper(Grade)).

grade(_Context, ValidVals, Default, Val) when is_list(Val) ->
    case lists:member(Val, ValidVals) of
        true ->
            Val;
        false ->
            %% ContextName = pick(Context),
            %% io:format("BAD GRADE ~p ~s\n", [ContextName, Val]),
            Default
    end.

emit_grade(Difficulty, Danger, Sos) ->
    if
        Difficulty =:= "" ->
            "";
        Danger =:= "" ->
            Difficulty;
        Sos =:= "" ->
            [Difficulty, ".", Danger];
        true ->
            [Difficulty, ".", Danger, Sos]
    end.

to_upper(Bin) when is_binary(Bin) ->
    list_to_binary(to_upper(binary_to_list(Bin)));
to_upper(Char) when is_integer(Char) ->
    if
        Char >= $a, Char =< $z ->
            Char - ($a-$A);
        Char >= $å, Char =< $ö ->
            Char - ($å-$Å);
        true ->
            Char
    end;
to_upper(List) when is_list(List) ->
    lists:map(fun to_upper/1, List).

capitalize(Bin) when is_binary(Bin) ->
    list_to_binary(capitalize(binary_to_list(Bin)));
capitalize("http" ++_ = List) ->
    List;
capitalize([H|T]) ->
    [to_upper(H) | T];
capitalize([]) ->
    [].

unobjectify(Bin) ->
    case binary:split(Bin, <<"<object">>, [global]) of
        [_] ->
            Bin;
        [First | Objs] ->
            iolist_to_binary([First | lists:map(fun obj/1, Objs)])
    end.

obj(Obj) ->
    case binary:split(Obj, <<"</object>">>, []) of
        [_] ->
            <<"<object", Obj/binary>>;
        [Before, After] ->
            case binary:split(Before, <<"http">>, []) of
                [_] ->
                    <<"<object", Obj/binary>>;
                [_, RawUrl] ->
                    case binary:split(RawUrl, <<"\"">>, []) of
                        [_] ->
                            <<"<object", Obj/binary>>;
                        [StrippedUrl, _] ->
                            ["\nVideo: http", StrippedUrl, After, "\n"]
                    end
            end
    end.

extract_href(Bin) ->
    case binary:split(Bin, <<"<a href=\"">>, [global]) of
        [_] ->
            Bin;
        [First | Hrefs] ->
            iolist_to_binary([First | lists:map(fun href/1, Hrefs)])
    end.

href(Href) ->
    case binary:split(Href, <<"\"">>, []) of
        [_] ->
            <<"<a href=\"", Href/binary>>;
        [Link, After] ->
            case binary:split(After, <<">">>, []) of
                [_] ->
                    <<"<a href=\"", Href/binary>>;
                [_XX, RawName] ->
                    case binary:split(RawName, <<"</a>">>, []) of
                        [_] ->
                            <<"<a href=\"", Href/binary>>;
                        [Name, _] ->
                             ["\nLink: ", Name, ": ", Link, "\n"]
                    end
            end
    end.

extract_img(Bin) ->
    case binary:split(Bin, <<"<img src=\"">>, [global]) of
        [_] ->
            Bin;
        [First | Imgs] ->
            iolist_to_binary([First | lists:map(fun img/1, Imgs)])
    end.

img(Img) ->
    case binary:split(Img, <<"\"">>, []) of
        [_] ->
            <<"<img src=\"", Img/binary>>;
        [Link, _After] ->
            ["\nBild: ", Link, "\n"]
    end.

extract_iframe(Bin) ->
    case binary:split(Bin, <<"<iframe src=\"">>, [global]) of
        [_] ->
            Bin;
        [First | Iframes] ->
            iolist_to_binary([First | lists:map(fun iframe/1, Iframes)])
    end.

iframe(Iframe) ->
    case binary:split(Iframe, <<"\"">>, []) of
        [_] ->
            <<"<iframe src=\"", Iframe/binary>>;
        [Link, _After] ->
            ["\nBild: ", Link, "\n"]
    end.
