-module(braid).

-behaviour(cli).
-export([main/1, cli/0]).

-behaviour(application).
-export([start/2, stop/1]).

-define(REQUIRED_OTP_VERSION, 22).

-include_lib("kernel/include/logger.hrl").

-export([
    launch/1,
    destroy/1,
    list/1,
    logs/2
]).

% @doc Main CLI entry point.
main(Args) ->
    check_otp_version(?REQUIRED_OTP_VERSION),
    {ok, _} = application:ensure_all_started(braid),
    try
        cli:run(Args, #{
            progname => ?MODULE,
            modules => [?MODULE, braid_net],
            warn => false
        })
    catch
        Class:Reason:Stacktrace ->
            cli_abort(Class, Reason, Stacktrace)
    after
        braid_table:close(),
        application:stop(braid)
    end.

start(_Type, _Args) ->
    braid_table:init(),
    braid_net:init(),
    {ok, self()}.

stop(_State) -> ok.


cli() ->
    #{
        %handler => {?MODULE, about},
        commands => #{
            "launch" => #{
                handler => {?MODULE, launch, undefined},
                arguments => [
                    #{name => config, type => string, help => "path to file"}
                ],
                help => "Launch a new braidnet config spanning multiple machines"
            },
            "destroy" => #{
                handler => {?MODULE, destroy, undefined},
                arguments => [
                    #{name => config, type => string, help => "path to file"}
                ],
                help => "Destroy all resources listed in the config"
            },
            "list" => #{
                handler => {?MODULE, list, undefined},
                arguments => [
                    #{name => config, type => string, help => "path to file"}
                ],
                help => "Get info about all resources listed in the config"
            },
            "logs" => #{
                handler => {?MODULE, logs, undefined},
                arguments => [
                    #{name => machine, type => string, help => "id"},
                    #{name => container, type => string, help => "id"}
                ],
                help => "Get a dump of all logs of a specific container"
            }
        }
    }.

% CLI callbacks -––-------------------------------------------------------------

launch(ConfigPath) ->
    braid_net:ensure(),
    Results = braid_rest:launch(ConfigPath),
    [braid_cli:print("~p",[R]) || R <- Results].

destroy(ConfigPath) ->
    braid_net:ensure(),
    Results = braid_rest:destroy(ConfigPath),
    [braid_cli:print("~p",[R]) || R <- Results].

list(ConfigPath) ->
    braid_net:ensure(),
    Results = braid_rest:list(ConfigPath),
    [braid_cli:print("~p",[R]) || R <- Results].

logs(Machine, CID) ->
    braid_net:ensure(),
    {Code, Logs} = braid_rest:logs(Machine, CID),
    braid_cli:print("~p~n~s",[Code, Logs]).


%--- Internal ------------------------------------------------------------------

check_otp_version(Version) ->
    check_otp_version(
        Version,
        list_to_integer(erlang:system_info(otp_release))
    ).

check_otp_version(Desired, Actual) when Desired > Actual ->
    braid_cli:abort("OTP version ~p too old. At least ~p required.", [
        Actual,
        Desired
    ]);
check_otp_version(_, _) ->
    ok.

cli_abort(Class, Reason, Stacktrace) ->
    erlang:raise(Class, Reason, Stacktrace).
