-module(braid_cli).
-define(REQUIRED_OTP_VERSION, 22).

-behaviour(cli).
-export([main/1, cli/0]).

-export([
    launch/1,
    destroy/1,
    list/1,
    logs/2,
    rpc/5,
    config/3
]).

-include_lib("kernel/include/logger.hrl").


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
            },
            "rpc" => #{
                handler => {?MODULE, rpc, undefined},
                arguments => [
                    #{name => machine, type => string, help => "id"},
                    #{name => container, type => string, help => "id"},
                    #{name => module, type => string, help => "module"},
                    #{name => function, type => string, help => "function"},
                    #{name => args, type => string, help => "args"}
                ],
                help => "Execute an arbitrary RPC on a remote container"
            },
            "config" => #{
                handler => {?MODULE, config, undefined},
                arguments => [
                    #{name => type, type => string, help => "mesh type"},
                    #{name => size, type => string, help => "size"},
                    #{name => image, type => binary, help => "docker image"}
                ],
                help => "Generates a testing braid config with various options"
            }
        }
    }.


% CLI callbacks ----------------------------------------------------------------

launch(ConfigPath) ->
    braid_net:ensure(),
    Results = braid_rest:launch(ConfigPath),
    [braid_cli_util:print("~p",[R]) || R <- Results].

destroy(ConfigPath) ->
    braid_net:ensure(),
    Results = braid_rest:destroy(ConfigPath),
    [braid_cli_util:print("~p",[R]) || R <- Results].

list(ConfigPath) ->
    braid_net:ensure(),
    Results = braid_rest:list(ConfigPath),
    [braid_cli_util:print("~p",[R]) || R <- Results].

logs(Machine, CID) ->
    braid_net:ensure(),
    {Code, Logs} = braid_rest:logs(Machine, CID),
    braid_cli_util:print("~p~n~s",[Code, Logs]).

rpc(Machine, CID, M, F, A) ->
    braid_net:ensure(),
    Result = braid_rest:rpc(Machine, CID, M, F, A),
    braid_cli_util:print("~p",[Result]).

config(Type, Size, DockerImage) ->
    braid_net:ensure(),
    N = list_to_integer(Size),
    case Type of
        "mesh" -> braid_config:gen(mesh, DockerImage, N);
        "ring" -> braid_config:gen(ring, DockerImage, N);    
        "hypercube" -> braid_config:gen(hypercube, DockerImage, N)
    end.

%--- Internal ------------------------------------------------------------------

check_otp_version(Version) ->
    check_otp_version(
        Version,
        list_to_integer(erlang:system_info(otp_release))
    ).

check_otp_version(Desired, Actual) when Desired > Actual ->
    braid_cli_util:abort("OTP version ~p too old. At least ~p required.", [
        Actual,
        Desired
    ]);
check_otp_version(_, _) ->
    ok.

cli_abort(Class, Reason, Stacktrace) ->
    erlang:raise(Class, Reason, Stacktrace).
