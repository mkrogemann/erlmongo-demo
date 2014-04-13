%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erlmongo_demo application.

-module(erlmongo_demo_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erlmongo_demo.
start(_Type, _StartArgs) ->
    erlmongo_demo_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erlmongo_demo.
stop(_State) ->
    ok.
