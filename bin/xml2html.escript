#!/usr/bin/env escript

%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%% Generates an forsguiden.html file from forsguiden.xml
%%
%% xml2html [ [InFile] OutFile]

-module(xml2html).
-mode(compile).

-include_lib("xmerl/include/xmerl.hrl").

-define(UNICODE(Chars), unicode:characters_to_binary(Chars, unicode)).
-define(PICK(Key, List, Context), pick(Key, List, Context, ?LINE)).
-define(NAME(Key, List, Context),
        name(?PICK(Key, List, Context), Context)).

-record(opts, {is_verbose = false, infile, outfile}).

main(Args) ->
    Opts = #opts{infile = InFile, outfile = OutFile} =
        parse_args(Args, #opts{}),
    io:format("Reading file ~s...\n", [InFile]),
    try
        {Internal, _Rest} = xmerl_scan:file(InFile, [{quiet,true}]),
        Districts = to_simple(Internal),
        FileContext = [InFile, file],
        Html =
            [
             emit_html_header("Forsguiden Sverige"),
             emit_all_districts(Districts, FileContext),
             emit_river_headers(Districts, FileContext),
             emit_districts(Opts, Districts, FileContext),
             emit_html_footer()
            ],
        io:format("Generating file://~s\n", [filename:absname(OutFile)]),
        {ok, OutFile} = {file:write_file(OutFile, Html), OutFile}
    catch
        exit:{fatal, {Reason, {file,BadFile}, {line,L}, {col,C}}} ->
            io:format("ERROR ~s(~p:~p): ~p\n", [BadFile, L, C, Reason]),
            halt(2)
    end.

parse_args([H|T], Opts) ->
    case H of
        "-v" ->
            parse_args(T, Opts#opts{is_verbose = true});
        File when Opts#opts.infile =:= undefined ->
            parse_args(T, Opts#opts{infile = File});
        File when Opts#opts.outfile =:= undefined ->
            parse_args(T, Opts#opts{outfile = File})
    end;
parse_args([], Opts) when Opts#opts.infile =:= undefined ->
    parse_args([], Opts#opts{infile = "forsguiden.xml"});
parse_args([], Opts) when Opts#opts.outfile =:= undefined ->
    Base = filename:basename(Opts#opts.infile, ".xml"),
    parse_args([], Opts#opts{outfile = Base++".html"});
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
    strip(Text);
to_simple(#xmlComment{value = _Text}) ->
    "".

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

emit_html_header(Title) ->
    Url = "http://www.unsponsored.co.uk/press/?p=1168",
    [
     <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" "
       "\"http://www.w3.org/TR/html4/strict.dtd\">\n">>,
     <<"<html>\n">>,
     <<"<head>\n">>,
     <<"  <meta charset=\"UTF-8\">\n">>,
     <<"  <title>">>, Title, <<"</title>\n">>,
     <<"</head>\n\n">>,
     <<"<body>\n">>,
     "  ", tag("h1", Title),
     "  ", emit("Graderingssystemet i Forsguiden finns beskrivet "),
     "<a href=\"", Url, "\">", emit("här"), "</a>\n"
    ].

emit_html_footer() ->
    {{Year,Mon,Day},{Hour,Min,Sec}} = calendar:now_to_local_time(erlang:now()),
    [
     tag("h3", ["Genererad ",
                lists:concat([Year, "-", Mon, "-", Day," ",
                              Hour, ":", Min, ":", Sec])]),
     <<"</body>\n">>
    ].

emit_all_districts({districts, Districts}, FileContext) ->
    [
     "  ", tag("h1", ["Alla distrikt"]),
     "  <table border=1>\n",
     [emit_district(District, [districts | FileContext]) ||
         District <- Districts],
     "  </table>\n\n"
    ].

emit_district({district, District}, DistrictsContext) ->
    DistrictContext0 = [district | DistrictsContext],
    DistrictName = ?NAME(districtname, District, DistrictContext0),
    DN = emit(DistrictName),
    DistrictContext = [DN | DistrictContext0],
    Names =
        [emit(?PICK(rivername, River,
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

emit_river_headers({districts, Districts}, FileContext) ->
    DistrictsContext = [districts | FileContext],
    Headers =
        [header(River, District, DistrictsContext) ||
            {district, District} <- Districts,
            {rivers, Rivers} <- District,
            {river, River} <- Rivers],
    Sorted = lists:keysort(3, Headers),
    [
     "  ", tag("h1", "A-Ö"),
     "  <table border=1>\n",
     [emit_river_header(Header, DistrictsContext) || Header <- Sorted],
     "  </table>\n\n",
     "  <p/>\n"
    ].

emit_river_header({header, DistrictName, RiverName, River, RiverContext},
                   _DistrictsContext) ->
    Href =
        fun(RapidName) ->
                N = rapid_name(RiverName, RapidName),
                ["<a href=\"#", N,  "\">", emit(RapidName), "</a>"]
        end,
    Rapids = ?PICK(rapids, River, RiverContext),
    GradeSpan = ?PICK(gradespan, River, RiverContext),
    RapidNames =
        [?NAME(rapidname, Rapid, [rapid, rapids | RiverContext]) ||
            {rapid, Rapid} <- Rapids],
    Sorted = lists:sort(RapidNames),
    Links = [[Href(RapidName), " "] || RapidName <- Sorted],
    RN = emit(RiverName),
    DN = emit(DistrictName),
    [
     "    <tr>\n",
     "      <td><a href=\"#", RN, "\">", RN, "</a></td>\n",
     "      <td><a href=\"#", DN, "\">", DN, "</a></td>\n",
     "      <td>", GradeSpan, "</td>\n",
     "      <td>", Links, "</td>\n",
     "    </tr>\n"
    ].

rapid_name(RiverName, RapidName) ->
    emit([RiverName, "_", RapidName]).

header(River, District, DistrictsContext) ->
    DistrictContext0 = [district | DistrictsContext],
    DistrictName =
        ?NAME(districtname, District, DistrictContext0),
    DN = emit(DistrictName),
    RiverContext0 = [river, rivers, DN | DistrictContext0],
    RiverName =
        ?NAME(rivername, River, RiverContext0),
    RN = emit(RiverName),
    {header, DistrictName, RiverName, River, [RN | RiverContext0]}.

emit_districts(Opts, {districts, Districts}, FileContext) ->
    DistrictsContext = [districts | FileContext],
    [emit_district(Opts, District, DistrictsContext) || District <- Districts].

emit_district(Opts, {district, District}, DistrictsContext) ->
    DistrictContext0 = [district | DistrictsContext],
    DistrictName = ?PICK(districtname, District, DistrictContext0),
    DN = emit(DistrictName),
    DistrictContext = [DN | DistrictContext0],
    Rivers = ?PICK(rivers, District, DistrictContext),
    verbose(Opts, "~s\n", [DN]),
    Headers =
        [header(River, District, DistrictsContext) || {river, River} <- Rivers],
    Sorted = lists:keysort(3, Headers),
    RiverContext0 = [river, rivers | DistrictContext],
    [
     "  <a name=\"", DN, "\">\n",
     "  ", tag("h1", emit("Distrikt " ++ DistrictName)),
     "  <table border=1>\n",
     [emit_river_header(Header, DistrictsContext) || Header <- Sorted],
     "  </table>\n\n",
     "  <p/>\n"
     "  <hr/>\n",
     "  </table>\n",
     "  <p/>\n",
     [emit_river(Opts, DistrictName, River, RiverContext0) || River <- Rivers]
    ].

emit_river(Opts, DistrictName, {river, River}, RiverContext0) ->
    RiverName = ?NAME(rivername, River, RiverContext0),
    RiverContext = [emit(RiverName) | RiverContext0],
    RN = emit(RiverName),
    GradeSpan = ?PICK(gradespan, River, RiverContext),
    AltName = altname(?PICK(altname, River, RiverContext), RiverContext),
    PrimeSeason = ?PICK(primeseason, River, RiverContext),
    Season = ?PICK(season, River, RiverContext),
    Source = ?PICK(source, River, RiverContext),
    LastUpdated = ?PICK(lastupdated, River, RiverContext),
    Description = ?PICK(description, River, RiverContext),
    HowToFind = ?PICK(howtofind, River, RiverContext),
    WhereToStay = ?PICK(wheretostay, River, RiverContext),
    Rapids = ?PICK(rapids, River, RiverContext),
    RapidContext0 = [rapid, rapids, RN | RiverContext],
    verbose(Opts, "\t~s - ~s\n", [RN, emit(GradeSpan)]),
    DN = emit(DistrictName),
    [
     "\n",
     "  <a name=\"", RN, "\">\n",
     "  ", tag("h1",
               [RN,
                opt(" (", AltName, ")"),
                " i " , "<a href=\"#", DN, "\">", DN, "</a>",
                " - ", emit(GradeSpan)]),
     "  <table border=1>\n",
     emit_river_row("Svårighetsgrad", GradeSpan),
     emit_river_row("Säsong", [PrimeSeason, opt(" (", emit(Season), ")")]),
     emit_river_prop(bigwater, River, RiverContext),
     emit_river_prop(creeking, River, RiverContext),
     emit_river_prop(freestyle, River, RiverContext),
     emit_river_prop(rookie, River, RiverContext),
     emit_river_prop(slalom, River, RiverContext),
     emit_river_prop(riverrunning, River, RiverContext),
     emit_river_row("Källa", [Source, opt(" (senast ändrad ", LastUpdated, ")")]),
     "  </table>\n",
     "  <p/>\n",
     emit(Description),
     " <p/>",
     opt(emit("Hitta hit: "), emit(HowToFind), "\n<p/>"),
     opt(emit("Boende: "), emit(WhereToStay), "\n<p/>"),
     [emit_rapid(Opts, RiverName, Rapid, RapidContext0) || Rapid <- Rapids],
     "  <hr/>\n"
    ].

emit_river_prop(Tag, River, RiverContext) ->
    Val = ?PICK(Tag, River, RiverContext),
    Label = capitalize(atom_to_list(Tag), RiverContext),
    emit_river_row(Label, Val).

emit_river_row(Label, Val) ->
     [
      "    <tr>\n",
      "      <td>", emit(Label), "</td>\n",
      "      <td>", emit(Val), "</td>\n",
      "    </tr>\n"
     ].

emit_rapid(Opts, RiverName, {rapid, Rapid}, RapidContext0) ->
    RapidName = ?NAME(rapidname, Rapid, RapidContext0),
    RapidContext = [emit(RapidName) | RapidContext0],
    Grade = ?PICK(grade, Rapid, RapidContext),
    AvgGradient = ?PICK(avggradient, Rapid, RapidContext),
    Description = ?PICK(description, Rapid, RapidContext),
    Dangers = ?PICK(dangers, Rapid, RapidContext),
    HowToFind = ?PICK(howtofind, Rapid, RapidContext),
    FirstDescent = ?PICK(firstdescent, Rapid, RapidContext),
    Spots = ?PICK(spots, Rapid, RapidContext),
    SpotContext0 = [spot, spots | RapidContext],
    verbose(Opts,
            "\t\t~s - ~s\n", [emit(RapidName), emit(Grade)]),
    [
     "  <a name=\"", rapid_name(RiverName, RapidName), "\">\n",

     "  ", tag("h2", [RapidName, " - ", Grade,
                      opt(" (lutning ", AvgGradient, ")")]),
     emit(Description),
     "  <p/>",
     opt("<font color=\"red\"><b>Faror: ", emit(Dangers), "</b></font>\n<p/>"),
     opt("Hitta hit: ", emit(HowToFind), "\n<p/>"),
     opt("First descent: ", emit(FirstDescent), "\n<p/>"),
     [emit_spot(Opts, Spot, SpotContext0) || Spot <- Spots]
    ].

emit_spot(Opts, {spot, Spot}, SpotContext0) ->
    SpotName= ?NAME(spotname, Spot, SpotContext0),
    SN = emit(SpotName),
    SpotContext = [SN | SpotContext0],
    Description= ?PICK(description, Spot, SpotContext),
    verbose(Opts, "\t\t\t~s\n", [SN]),
    [
     "  ", tag("h3", SpotName),
     "  <p/>\n",
     emit(Description)
    ].

opt(_Before, "", _After) ->
    "";
opt(_Before, <<>>, _After) ->
    "";
opt(Before, IoList, After) ->
    [Before, IoList, After].

tag(Tag, IoList) ->
    ["<", Tag, ">", emit(IoList), "</", Tag, ">", "\n"].

emit(IoList) when is_list(IoList) ->
    emit(?UNICODE(IoList));
emit(Bin) when is_binary(Bin) ->
    case strip([B || B <- binary:split(Bin, <<"\n">>, [global])]) of
        [] ->
            [];
        [Single] ->
            do_emit(Single);
        [First | Rest] ->
            IoList = [do_emit(First) | [["\n", do_emit(R)] || R <- Rest]],
            ?UNICODE(IoList)
    end.

do_emit(Bin) ->
    Replace =
        fun({From, To}, Acc) ->
                Acc2 = binary:replace(Acc, ?UNICODE(From), To, [global]),
                From2 = ?UNICODE(From),
                binary:replace(Acc2, From2, To, [global])
        end,
    lists:foldl(Replace, Bin, map()).

map() ->
    [
     {"& ", <<"&amp; ">>},
     {"< ", <<"&lt; ">>},
     {" >", <<" &gt;">>},
     {"–", <<"&ndash;">>},
%% %%  {"\"", <<"&quot;">>},
%%   {"é", <<"&eacute;">>},
%%   {"É", <<"&Eacute;">>},
%%   {"å", <<"&aring;">>},
%%   {"Å", <<"&Aring;">>},
%%   {"ä", <<"&auml;">>},
%%   {"Ä", <<"&Auml;">>},
%%   {"ö", <<"&ouml;">>},
%%   {"Ö", <<"&Ouml;">>},
     {"\r", <<"\n">>},
     {"\n+", <<"\n">>},
     {"\n", <<"<p/>">>},
     {" +$", <<"">>}
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

altname("", _Context) ->
    "";
altname(Name, Context) ->
    name(Name, Context).

name("", Context) ->
    io:format("WARNING: Missing name\n\t ~p\n", [Context]),
    "";
name([H|T] = Name, Context) ->
    if
        H >= $a, H =< $z ->
            Name2 = [H - ($a-$A) | T],
            io:format("WARNING: Capitalize \"~s\" -> \"~s\"\n\t~p\n",
                      [emit(Name), emit(Name2), Context]),
            Name2;
        H >= $å, H =< $ö ->
            Name2 = [H - ($å-$å) | T],
            io:format("WARNING: Capitalize ~s -> ~s\n\t~p\n",
                      [emit(Name), emit(Name2)], Context),
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
