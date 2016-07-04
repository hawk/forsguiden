#!/usr/bin/env escript
%% -*- coding: utf-8 -*-
%% Copyright 2015-2016 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%% Generates an forsguiden.html file from forsguiden.xml
%%
%% xml2html [-x -v -nw] [ [InFile] OutFile]
%%
%%   -x  - generate a kanotguiden.xml file instead
%%   -v  - verbose
%%   -nw - no warnings

-module(xml2html).
-mode(compile).
-include_lib("xmerl/include/xmerl.hrl").

-define(UNICODE(Chars), unicode:characters_to_binary(Chars, unicode)).
-define(PICK(Key, List, Context), pick(Key, List, Context, ?LINE)).
-define(NAME(Opts, Key, List, Context),
        name(Opts, ?PICK(Key, List, Context), Context)).
-define(is_html(Opts), Opts#opts.outformat =:= html).
-define(is_xml(Opts), Opts#opts.outformat =:= xml).

-record(opts,
        {is_verbose = false  :: boolean(),
         no_warnings = false :: boolean(),
         outformat = html    :: xml | html,
         indent = ""         :: string(),
         infile              :: string(),
         outfile             :: string()}).

main(Args) ->
    Opts = #opts{infile = InFile, outfile = OutFile} =
        parse_args(Args, #opts{}),
    io:format("Reading file ~s...\n", [InFile]),
    try
        {Internal, _Rest} = xmerl_scan:file(InFile, [{quiet,true}]),
        Districts = to_simple(Internal),
        FileContext = [InFile, file],
        IoList =
            [
             emit_header(Opts, <<"Forsguiden Sverige">>),
             emit_district_table(Opts, Districts, FileContext),
             emit_river_table(Opts, Districts, FileContext),
             emit_districts(Opts, Districts, FileContext),
             emit_footer(Opts)
            ],
        io:format("Generating file://~s\n", [filename:absname(OutFile)]),
        Enc = {encoding, unicode},
        {ok, OutFile} = {file:write_file(OutFile, IoList, [Enc]), OutFile}
    catch
        exit:{fatal, {Reason, {file,BadFile}, {line,L}, {col,C}}} ->
            io:format("ERROR ~s(~p:~p): ~p\n", [BadFile, L, C, Reason]),
            halt(2)
    end.

parse_args([H|T], Opts) ->
    case H of
        "-x" ->
            parse_args(T, Opts#opts{outformat = xml});
        "-v" ->
            parse_args(T, Opts#opts{is_verbose = true});
        "-nw" ->
            parse_args(T, Opts#opts{no_warnings = true});
        File when Opts#opts.infile =:= undefined ->
            parse_args(T, Opts#opts{infile = File});
        File when Opts#opts.outfile =:= undefined ->
            parse_args(T, Opts#opts{outfile = File})
    end;
parse_args([], Opts) when Opts#opts.infile =:= undefined ->
    parse_args([], Opts#opts{infile = "forsguiden.xml"});
parse_args([], Opts) when Opts#opts.outfile =:= undefined,
                          ?is_html(Opts) ->
    Base = filename:basename(Opts#opts.infile, ".xml"),
    parse_args([], Opts#opts{outfile = Base++".html"});
parse_args([], Opts) when Opts#opts.outfile =:= undefined,
                          ?is_xml(Opts) ->
    parse_args([], Opts#opts{outfile = "kanotguiden.xml"});
parse_args([], Opts) ->
    Opts.

to_simple(List) when is_list(List) ->
    Simple = fun(Elem, Acc) ->
                     case to_simple(Elem) of
                         []       -> Acc;
                         Stripped -> [Stripped | Acc]
                     end
             end,
    lists:reverse(lists:foldl(Simple, [], List));
to_simple(#xmlElement{name    = Tag,
                      content = Content}) ->
    {Tag, lists:flatten(to_simple(Content))};
to_simple(#xmlText{value = Text}) ->
    strip_cdata(strip(Text));
to_simple(#xmlComment{value = _Text}) ->
    "".

strip_cdata(Text) ->
    Opts = [global, {return, list}],
    Text2 = re:replace(Text, "^<!\\[CDATA\\[", "", Opts),
    re:replace(Text2, "\]\]>", "", Opts);
strip_cdata(Text) ->
    binary_to_list(strip_cdata(?UNICODE(Text))).

%% Strips off leading and trailing white spaces
strip([]) ->
    [];
strip([Char | Text]) when Char==$\s; Char==$\n; Char==$\t ->
    strip(Text);
strip(Text) ->
    strip(Text,[],[]).
strip([Char | Text], TAcc, SAcc) when Char==$\s; Char==$\n; Char==$\t ->
    strip(Text, TAcc, [Char | SAcc]);
strip([Char |Text], TAcc, SAcc) ->
    strip(Text, [Char | SAcc ++ TAcc], []);
strip([], TAcc, _SAcc) ->
    lists:reverse(TAcc).

emit_header(Opts, Title) when ?is_html(Opts) ->
    Url = "http://www.unsponsored.co.uk/press/?p=1168",
    [
     <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" "
       "\"http://www.w3.org/TR/html4/strict.dtd\">\n">>,
     <<"<html>\n">>,
     <<"<head>\n">>,
     <<"  <meta charset=\"UTF-8\">\n">>,
     <<"  <title>">>, emit(Opts, Title), <<"</title>\n">>,
     <<"</head>\n\n">>,
     <<"<body>\n">>,
     "  ", tag(Opts, "h1", Title),
     "  ", emit(Opts, "Graderingssystemet i Forsguiden finns beskrivet "),
     "<a href=\"", Url, "\">", <<"här">>, "</a>\n"
    ];
emit_header(Opts, _Title) when ?is_xml(Opts) ->
    [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n">>].

emit_footer(Opts) when ?is_html(Opts) ->
    {{Year,Mon,Day},{Hour,Min,Sec}} =
        calendar:now_to_local_time(erlang:timestamp()),
    [
     tag(Opts, "h3", ["Genererad ",
                lists:concat([Year, "-", Mon, "-", Day," ",
                              Hour, ":", Min, ":", Sec])]),
     <<"</body>\n">>
    ];
emit_footer(Opts) when ?is_xml(Opts) ->
    [].

emit_district_table(Opts, {districts, Districts}, FileContext)
  when ?is_html(Opts) ->
    [
     "  ", tag(Opts, "h1", ["Alla distrikt"]),
     "  <table border=1>\n",
     [emit_district_row(Opts, District, [districts | FileContext]) ||
         District <- Districts],
     "  </table>\n\n"
    ];
emit_district_table(Opts, _Districts, _FileContext)
  when ?is_xml(Opts) ->
    [].

emit_district_row(Opts, {district, District}, DistrictsContext) ->
    DistrictContext0 = [district | DistrictsContext],
    DistrictName = ?NAME(Opts, districtname, District, DistrictContext0),
    DN = emit(Opts, DistrictName),
    DistrictContext = [DN | DistrictContext0],
    Names =
        [emit(Opts, ?PICK(rivername, River,
                    [river, rivers | DistrictContext])) ||
            {rivers, Rivers} <- District,
            {river, River} <- Rivers],
    Sorted = lists:sort(Names),
    Links = [["<a href=\"#", Name, "\">", Name, "</a> "] || Name <- Sorted],
    [
     "    <tr>\n",
     "      <td>", "<a href=\"#", DN, "\">", DN, "</a> ", "</td>\n",
     "      <td>", Links, "</td>\n",
     "    </tr>\n"
    ].

emit_river_table(Opts, {districts, Districts}, FileContext)
  when ?is_html(Opts)->
    DistrictsContext = [districts | FileContext],
    Headers =
        [pick_header(Opts, River, District, DistrictsContext) ||
            {district, District} <- Districts,
            {rivers, Rivers} <- District,
            {river, River} <- Rivers],
    Sorted = lists:keysort(3, Headers),
    [
     "  ", tag(Opts, "h1", <<"A-Ö">>),
     "  <table border=1>\n",
     [emit_river_header(Opts, Header, DistrictsContext) || Header <- Sorted],
     "  </table>\n\n",
     "  <p/>\n"
    ];
emit_river_table(Opts, _Districts, _FileContext)
  when ?is_xml(Opts) ->
    [].

emit_river_header(Opts, {header, DistrictName, RiverName, River, RiverContext},
                   _DistrictsContext) ->
    Href =
        fun(RapidName) ->
                N = rapid_name(Opts, RiverName, RapidName),
                ["<a href=\"#", N,  "\">", emit(Opts, RapidName), "</a>"]
        end,
    Rapids = ?PICK(rapids, River, RiverContext),
    GradeSpan = ?PICK(gradespan, River, RiverContext),
    RapidNames =
        [?NAME(Opts, rapidname, Rapid, [rapid, rapids | RiverContext]) ||
            {rapid, Rapid} <- Rapids],
    Sorted = lists:sort(RapidNames),
    Links = [[Href(RapidName), " "] || RapidName <- Sorted],
    RN = emit(Opts, RiverName),
    DN = emit(Opts, DistrictName),
    [
     "    <tr>\n",
     "      <td><a href=\"#", RN, "\">", RN, "</a></td>\n",
     "      <td><a href=\"#", DN, "\">", DN, "</a></td>\n",
     "      <td>", emit(Opts, GradeSpan), "</td>\n",
     "      <td>", Links, "</td>\n",
     "    </tr>\n"
    ].

rapid_name(Opts, RiverName, RapidName) ->
    emit(Opts, [RiverName, "_", RapidName]).

pick_header(Opts, River, District, DistrictsContext) ->
    DistrictContext0 = [district | DistrictsContext],
    DistrictName = ?NAME(Opts, districtname, District, DistrictContext0),
    DN = emit(Opts, DistrictName),
    RiverContext0 = [river, rivers, DN | DistrictContext0],
    RiverName = ?NAME(Opts, rivername, River, RiverContext0),
    RN = emit(Opts, RiverName),
    {header, DistrictName, RiverName, River, [RN | RiverContext0]}.

emit_districts(Opts, {districts, Districts}, FileContext) ->
    DistrictsContext = [districts | FileContext],
    DistrictsIoList =
        [emit_district(indent(Opts), District, DistrictsContext) ||
            District <- Districts],
    if
        ?is_html(Opts) ->
            DistrictsIoList;
        ?is_xml(Opts) ->
            [
             start_tag(Opts, "districts"),
             DistrictsIoList,
             end_tag(Opts, "districts")
            ]
    end.

emit_district(Opts, {district, District}, DistrictsContext) ->
    DistrictContext0 = [district | DistrictsContext],
    DistrictName = ?PICK(districtname, District, DistrictContext0),
    DN = emit(Opts, DistrictName),
    DistrictContext = [DN | DistrictContext0],
    Rivers = ?PICK(rivers, District, DistrictContext),
    verbose(Opts, "~s\n", [DN]),
    Headers =
        [pick_header(Opts, River, District, DistrictsContext) ||
            {river, River} <- Rivers],
    Sorted = lists:keysort(3, Headers),
    RiverContext0 = [river, rivers | DistrictContext],
    Opts2 = indent(Opts),
    Opts3 = indent(Opts2),
    RiversIoList =
        [emit_river(Opts3, DistrictName, River, RiverContext0) ||
            River <- Rivers],
    if
        ?is_html(Opts) ->
            [
             "  <a name=\"", DN, "\">\n",
             "  ", tag(Opts, "h1", "Distrikt " ++ DistrictName),
             "  <table border=1>\n",
             [emit_river_header(Opts, Header, DistrictsContext) ||
                 Header <- Sorted],
             "  </table>\n\n",
             "  <p/>\n"
             "  <hr/>\n",
             "  </table>\n",
             "  <p/>\n",
             RiversIoList
            ];
        ?is_xml(Opts) ->
            [
             start_tag(Opts, "district"),
             tag(Opts2, "districtname", DistrictName),
             start_tag(Opts2, "rivers"),
             RiversIoList,
             end_tag(Opts2, "rivers"),
             end_tag(Opts, "district")
            ]
    end.

emit_river(Opts, DistrictName, {river, River}, RiverContext0) ->
    RiverName = ?NAME(Opts, rivername, River, RiverContext0),
    RiverContext = [emit(Opts, RiverName) | RiverContext0],
    RN = emit(Opts, RiverName),
    GradeSpan = ?PICK(gradespan, River, RiverContext),
    AltName = altname(Opts, ?PICK(altname, River, RiverContext), RiverContext),
    PrimeSeason = ?PICK(primeseason, River, RiverContext),
    Season = ?PICK(season, River, RiverContext),
    Source = ?PICK(source, River, RiverContext),
    LastUpdated = ?PICK(lastupdated, River, RiverContext),
    Description = ?PICK(description, River, RiverContext),
    HowToFind = ?PICK(howtofind, River, RiverContext),
    WhereToStay = ?PICK(wheretostay, River, RiverContext),
    Rapids = ?PICK(rapids, River, RiverContext),
    RapidContext0 = [rapid, rapids, RN | RiverContext],
    verbose(Opts, "\t~s - ~s\n", [RN, emit(Opts, GradeSpan)]),
    DN = emit(Opts, DistrictName),
    Opts2 = indent(Opts),
    Opts3 = indent(Opts2),
    RapidsIoList =
        [emit_rapid(Opts3, RiverName, Rapid, RapidContext0) || Rapid <- Rapids],
    if
        ?is_html(Opts) ->
            [
             "\n",
             "  <a name=\"", RN, "\">\n",
             "  ", tag(Opts, "h1",
                       [RN,
                        opt(" (", AltName, ")"),
                        " i " , "<a href=\"#", DN, "\">", DN, "</a>",
                        " - ", GradeSpan]),
             "  <table border=1>\n",
             do_emit_river_row(<<"Svårighetsgrad">>, GradeSpan),
             do_emit_river_row(<<"Säsong">>,
                               [PrimeSeason,
                                opt(" (", emit(Opts, Season), ")")]),
             emit_river_prop(Opts, bigwater, River, RiverContext),
             emit_river_prop(Opts, creeking, River, RiverContext),
             emit_river_prop(Opts, freestyle, River, RiverContext),
             emit_river_prop(Opts, rookie, River, RiverContext),
             emit_river_prop(Opts, slalom, River, RiverContext),
             emit_river_prop(Opts, riverrunning, River, RiverContext),
             do_emit_river_row(<<"Källa">>,
                               [emit(Opts, Source),
                                opt(<<" (senast ändrad ">>,
                                    emit(Opts, LastUpdated), ")")]),
             "  </table>\n",
             "  <p/>\n",
             emit(Opts, Description),
             " <p/>",
             opt(<<"Vägbeskrivning: ">>,
                 emit(Opts, HowToFind), "\n<p/>"),
             opt(emit(Opts, "Boende: "),
                 emit(Opts, WhereToStay), "\n<p/>"),
             RapidsIoList,
             "  <hr/>\n"
            ];
        ?is_xml(Opts) ->
            Description2 =
                [
                 Description,
                 opt(<<"\n\nVägbeskrivning: ">>,
                     emit(Opts, HowToFind), ""),
                 opt(emit(Opts, "\n\nBoende: "),
                     emit(Opts, WhereToStay), ""),
                 opt(<<"\n\nKälla: ">>,
                     [emit(Opts, Source),
                      opt(<<" (senast ändrad ">>,
                          emit(Opts, LastUpdated), ")")], "")
                ],
            [
             start_tag(Opts, "river"),
             tag(Opts2, "rivername", [RiverName, opt(" (", AltName, ")")]),
             tag(Opts2, "gradespan", GradeSpan),
             tag(Opts2, "description", Description2),
             start_tag(Opts2, "rapids"),
             RapidsIoList,
             end_tag(Opts2, "rapids"),
             end_tag(Opts, "river")
            ]
    end.

emit_river_prop(Opts, Tag, River, RiverContext) ->
    Val = ?PICK(Tag, River, RiverContext),
    Label = capitalize(atom_to_list(Tag), RiverContext),
    emit_river_row(Opts, Label, Val).

emit_river_row(Opts, Label, Val) ->
    do_emit_river_row(emit(Opts, Label), emit(Opts, Val)).

do_emit_river_row(Label, Val) ->
     [
      "    <tr>\n",
      "      <td>", Label, "</td>\n",
      "      <td>", Val, "</td>\n",
      "    </tr>\n"
     ].

emit_rapid(Opts, RiverName, {rapid, Rapid}, RapidContext0) ->
    RapidName = ?NAME(Opts, rapidname, Rapid, RapidContext0),
    RapidContext = [emit(Opts, RapidName) | RapidContext0],
    Grade = ?PICK(grade, Rapid, RapidContext),
    Gradient = ?PICK(avggradient, Rapid, RapidContext),
    Description = ?PICK(description, Rapid, RapidContext),
    Dangers = ?PICK(dangers, Rapid, RapidContext),
    HowToFind = ?PICK(howtofind, Rapid, RapidContext),
    FirstDescent = ?PICK(firstdescent, Rapid, RapidContext),
    Spots = ?PICK(spots, Rapid, RapidContext),
    SpotContext0 = [spot, spots | RapidContext],
    verbose(Opts,
            "\t\t~s - ~s\n", [emit(Opts, RapidName), emit(Opts, Grade)]),
    Opts2 = indent(Opts),
    Opts3 = indent(Opts2),
    SpotsIoList = [emit_spot(Opts3, Spot, SpotContext0) || Spot <- Spots],
    if
        ?is_html(Opts) ->
            [
             "  <a name=\"", rapid_name(Opts, RiverName, RapidName), "\">\n",
             "  ", tag(Opts, "h2", [RapidName, " - ", Grade,
                                    opt(" (lutning ", Gradient, ")")]),
             emit(Opts, Description),
             "  <p/>",
             opt("<font color=\"red\"><b>Faror: ",
                 emit(Opts, Dangers), "</b></font>\n<p/>"),
             opt(<<"Vägbeskrivning: ">>, emit(Opts, HowToFind), "\n<p/>"),
             opt("First descent: ", emit(Opts, FirstDescent), "\n<p/>"),
             SpotsIoList
            ];
        ?is_xml(Opts) ->
            Description2 =
                [
                 Description,
                 opt(emit(Opts, "\n\nFaror: "),
                     emit(Opts, Dangers), ""),
                 opt(emit(Opts, "\n\nLutning: "),
                     emit(Opts, Gradient), ""),
                 opt(<<"\n\nVägbeskrivning: ">>,
                     emit(Opts, HowToFind), ""),
                 opt(emit(Opts, "\n\nFirst descent: "),
                     emit(Opts, FirstDescent), ""),
                 SpotsIoList
                ],
            [
             start_tag(Opts, "rapid"),
             tag(Opts2, "rapidname", [RapidName]),
             tag(Opts2, "grade", Grade),
             tag(Opts2, "description", Description2),
             SpotsIoList,
             end_tag(Opts2, "rapid")
            ]
    end.

emit_spot(Opts, {spot, Spot}, SpotContext0) ->
    SpotName= ?NAME(Opts, spotname, Spot, SpotContext0),
    SN = emit(Opts, SpotName),
    SpotContext = [SN | SpotContext0],
    Description= ?PICK(description, Spot, SpotContext),
    verbose(Opts, "\t\t\t~s\n", [SN]),
    if
        ?is_html(Opts) ->
            [
             "  ", tag(Opts, "h3", SpotName),
             "  <p/>\n",
             emit(Opts, Description)
            ];
        ?is_xml(Opts), SpotName =/= "" orelse Description =/= "" ->
            Opts0 = Opts#opts{indent = ""},
            [
             "\n\n",
             if
                 SpotName =/= "" ->
                     [emit(Opts0, SpotName) , ": "];
                 true ->
                     []
             end,
             if
                 Description =/= "" ->
                     [emit(Opts0, Description) , ""];
                 true ->
                     []
             end
            ];
        ?is_xml(Opts) ->
            []
    end.

opt(_Before, "", _After) ->
    "";
opt(_Before, <<>>, _After) ->
    "";
opt(Before, IoList, After) ->
    [Before, IoList, After].

emit(Opts, IoList) when ?is_html(Opts) ->
    Bin = ?UNICODE(IoList),
    do_emit(Opts, Bin);
emit(Opts, IoList) when ?is_xml(Opts) ->
    emit_cdata(?UNICODE(IoList)).

emit_cdata(Bin) ->
    case re:run(Bin, <<"[\\<&]">>, []) of
        nomatch    ->
            Bin;
        {match, _} ->
            %% io:format("CDATA ~tp\n\n", [Bin]),
            <<"<![CDATA[", Bin/binary, "]]>">>
    end.

do_emit(_Opts, Bin) ->
    Replace =
        fun({From, To}, Acc) ->
                re:replace(Acc, From, To, [global])
        end,
    lists:foldl(Replace, Bin, map()).

map() ->
    [
     {<<"& ">>, <<"&amp; ">>},
     {<<"< ">>, <<"&lt; ">>},
     {<<" >">>, <<" &gt;">>},
     {<<"–">>, <<"&ndash;">>},
%% %%   {<<"\"">>, <<"&quot;">>},
%%   {<<"é">>, <<"&eacute;">>},
%%   {<<"É">>, <<"&Eacute;">>},
%%   {<<"å">>, <<"&aring;">>},
%%   {<<"Å">>, <<"&Aring;">>},
%%   {<<"ä">>, <<"&auml;">>},
%%   {<<"Ä">>, <<"&Auml;">>},
%%   {<<"ö">>, <<"&ouml;">>},
%%   {<<"Ö">>, <<"&Ouml;">>},
     {<<"\r\n">>, <<"\n">>},
     {<<"\n+">>, <<"\n">>},
     {<<"\n">>, <<"<p/>">>},
     {<<" +$">>, <<"">>}
    ].

pick(Tag, List, Context, Line) ->
    case lists:keyfind(Tag, 1, List) of
        {Tag, Val} ->
            Val;
        false ->
            io:format("ERROR(~p) Missing XML tag ~p in\n\t~p\n",
                      [Line, [Tag | Context], List]),
            halt(3)
    end.

altname(_Opts, "", _Context) ->
    "";
altname(Opts, Name, Context) ->
    name(Opts, Name, Context).

warning(#opts{no_warnings=true}, _Format, _Args) ->
    ok;
warning(#opts{no_warnings=false}, Format, Args) ->
    io:format("WARNING: " ++ Format ++ "\n", Args).

name(_Opts, "" = Name, _Context) ->
    Name;
name(Opts, [H|T] = Name, Context) ->
    if
        H >= $a, H =< $z ->
            Name2 = [H - ($a-$A) | T],
            warning(Opts, "Capitalize \"~s\" -> \"~s\"\n\t~p",
                      [emit(Opts, Name), emit(Opts, Name2), Context]),
            Name2;
        H >= $å, H =< $ö ->
            Name2 = [H - ($å-$å) | T],
            warning(Opts, "Capitalize ~s -> ~s\n\t~p",
                    [emit(Opts, Name), emit(Opts, Name2), Context]),
            Name2;
        true ->
            Name
    end.

capitalize([H|T] = Name, _Context) ->
    if
        H >= $a, H =< $z ->
            [H - ($a-$A) | T];
        H >= $å, H =< $ö ->
            [H - ($å-$å) | T];
        true ->
            Name
    end.

verbose(#opts{is_verbose = true}, Format, Args) ->
    io:format(Format, Args);
verbose(_Opts, _Format, _Args) ->
    ok.

indent(#opts{indent = Indent} = Opts) ->
    Opts#opts{indent = ["  " | Indent]}.

tag(Opts, Tag, IoList) when ?is_html(Opts) ->
    ["<", Tag, ">", IoList, "</", Tag, ">", "\n"];
tag(#opts{indent = Indent} = Opts, Tag, IoList) when ?is_xml(Opts) ->
    case iolist_to_binary(IoList) of
        <<>> ->
            [Indent, "<", Tag, "/>\n"];
        Unicode ->
            [Indent, "<", Tag, ">", Unicode, "</", Tag, ">", "\n"]
    end.

start_tag(#opts{indent = Indent}, Tag) ->
    [Indent, "<", Tag, ">\n"].

end_tag(#opts{indent = Indent}, Tag) ->
    [Indent, "</", Tag, ">\n"].
