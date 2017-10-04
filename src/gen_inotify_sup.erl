%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Application top-level supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_inotify_sup).

-behaviour(supervisor).

%% supervision tree API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start the supervisor process.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Initialize supervisor.

init([] = _Args) ->
  Strategy = {one_for_one, 5, 10},
  Children = [
    {gen_inotify_driver_loader,
      {gen_inotify_driver_loader, start_link, []},
      permanent, 5000, worker, [gen_inotify_driver_loader]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
