% @doc Task to provide info to connect to braidnet instance
-module(braid_net).

-behaviour(cli).
-export([cli/0]).

% API
-export([init/0]).
-export([setup/1]).
-export([ensure/0]).

-export([get/1]).

-record(setting, {
    name,
    title,
    description
}).

%--- Macros --------------------------------------------------------------------


-define(SETTINGS, [
    #setting{
        name = braidnet_domain,
        title = "BraidNet Domain",
        description = "Hostname to connect to the braidnet service"
    },
    #setting{
        name = braidnet_access_token,
        title = "Authorization Token",
        description = "Access Token for the braidnet REST API"
    }
]).

%--- API -----------------------------------------------------------------------

init() ->
    braid_table:ensure_table(?MODULE, []).

cli() ->
    #{
        commands => #{
            "setup" => #{help => "settigs to connect to a braidnet cloud"}
        }
    }.

setup(_) ->
    [check(T) || T <- ?SETTINGS].

ensure() ->
    [ensure(T) || T <- ?SETTINGS].

get(Token) ->
    braid_table:get(?MODULE, Token).

%--- Internal ------------------------------------------------------------------

check(#setting{name = Name, title = Title, description = Description}) ->
    check(Name, Title, Description).

check(Token, Name, Text) ->
    case braid_table:find(?MODULE, Token) of
        {ok, Value} ->
            braid_cli:print("~s:\n    ~s", [Name, Value]),
            case braid_cli:confirm("Do you want to replace it? [y/N]:") of
                true  -> aquire(Token, Name, Text);
                false -> ok
            end;
        error ->
            aquire(Token, Name, Text)
    end.

aquire(Token, Name, Text) ->
    braid_cli:print(Text),
    Value = braid_cli:input(Name ++ ":"),
    braid_table:set(?MODULE, Token, Value).

ensure(#setting{name = Name, title = Title}) ->
    case braid_table:find(?MODULE, Name) of
        {ok, Val} ->
            application:set_env(braid, Name, Val),
            ok;
        error ->
            braid_cli:abort(
                "~s is missing. Please run:\n\n"
                "    braid setup\n"
            , [Title]),
            erlang:halt(1)
    end.
