% @doc Helper functions for CLI.
-module(braid_cli_util).

% API
-export([abort/2]).
-export([print/1]).
-export([print/2]).
-export([input/1]).
-export([confirm/1]).

% Callbacks
-export([format/2]).

-include_lib("kernel/include/logger.hrl").

-define(YES, "^[Yy]([Ee][Ss])?$").

%--- API -----------------------------------------------------------------------

abort(Format, Args) ->
    io:format("~s~n", [color:red(io_lib:format(Format, Args))]),
    erlang:halt(1).

print(Text) -> print(Text, []).
print(Format, Args) ->
    io:format(Format ++ "~n", Args).

input(Prompt) ->
    case io:get_line(Prompt ++ " ") of
        eof ->
            "";
        {error, Reason} ->
            abort("Error reading input: ~p", [Reason]);
        Data ->
            re:replace(Data, "^[[:space:]]*+|[[:space:]]*+$", <<>>,
                [global, {return, binary}]
            )
    end.

confirm(Prompt) ->
    case re:run(input(Prompt), ?YES, [{capture, none}]) of
        match -> true;
        _     -> false
    end.

%--- Callbacks -----------------------------------------------------------------

format(Event, _Config) ->
    #{
        level := Level,
        meta := #{
            mfa := {Module, Function, Arity},
            time := Time
        }
    } = Event,
    io:format("~p~n", [Event]),
    Timestamp = calendar:system_time_to_rfc3339(
        erlang:convert_time_unit(Time, microsecond, millisecond),
        [{unit, millisecond}, {time_designator, $\s}, {offset, "Z"}]
    ),
    io_lib:format("~s ~s ~p:~p/~b~n", [
        Timestamp,
        format_level(Level),
        Module, Function, Arity
    ]).

format_level(Level) ->
    format_level(Level, string:uppercase(atom_to_binary(Level))).

format_level(debug, String) -> color:on_cyan([<<"[">>, String, <<"]">>]);
format_level(_Level, String) -> String.
