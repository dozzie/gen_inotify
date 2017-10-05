%%%---------------------------------------------------------------------------
%%% @doc
%%%   `inotify(7)' bindings for Erlang.
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_inotify).

%% public interface
-export([open/0, open/1, close/1]).
-export([add/3, update/3, remove/2, list/1]).
-export([controlling_process/2]).
-export([format_error/1]).

-export_type([handle/0, message/0, posix/0]).
-export_type([flag/0, flag_event/0]).

%%%---------------------------------------------------------------------------
%%% types

-define(DRIVER_NAME, "gen_inotify_drv").

-type handle() :: port().

-type cookie() :: non_neg_integer().

-type message() ::
    {inotify, handle(), file:filename() | undefined, cookie(),
      [flag() | flag_event(), ...]}
  | {inotify_error, handle(), queue_overflow | posix()}.
%% Filename is an absolute path. `undefined' should never happen.
%%
%% Flags `unmount' and `watch_removed' are always sent with no other
%% accompanying flag.
%%
%% If `is_dir' flag is present, it's always the first one.

-type flag() :: access
              | modify
              | attrib
              | create
              | delete
              | open
              | close_write | close_nowrite
              | move_from | move_to
              | delete_self
              | move_self.

-type flag_event() :: watch_removed
                    | is_dir
                    | unmount.
%% See {@type message()} for details about these flags' positions and company.

-type posix() :: inet:posix().

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Open a new inotify handle.

-spec open() ->
  {ok, handle()} | {error, system_limit | posix()}.

open() ->
  open([]).

%% @doc Open a new inotify handle.

-spec open(Options :: [Option]) ->
  {ok, handle()} | {error, badarg | system_limit | posix()}
  when Option :: recursive | real_path.

open(Options) ->
  case build_open_options_data(Options) of
    {ok, OptData} ->
      try open_port({spawn_driver, ?DRIVER_NAME}, [binary]) of
        Handle ->
          port_control(Handle, 0, OptData),
          {ok, Handle}
      catch
        error:Reason ->
          {error, Reason}
      end;
    {error, badarg} ->
      {error, badarg}
  end.

%%----------------------------------------------------------
%% build_open_options_data() {{{

%% @doc Translate list of options to a `port_control(Port,0,_)' request
%%   payload.

-spec build_open_options_data(Options :: [Option]) ->
  {ok, {Recursive :: boolean(), UseRealPath :: boolean()}} | {error, badarg}
  when Option :: recursive | real_path.

build_open_options_data(Options) ->
  DefaultOptions = {false, false}, % `{Recursive, UseRealPath}'
  try lists:foldr(fun add_open_option/2, DefaultOptions, Options) of
    {false, false} -> {ok, <<0:8, 0:8>>};
    {true,  false} -> {ok, <<1:8, 0:8>>};
    {false, true}  -> {ok, <<0:8, 1:8>>};
    {true,  true}  -> {ok, <<1:8, 1:8>>}
  catch
    _:_ -> {error, badarg}
  end.

%% @doc Workhorse for {@link build_open_options_data/1}.

-spec add_open_option(Option, {boolean(), boolean()}) ->
  {boolean(), boolean()}
  when Option :: recursive | real_path.

add_open_option(recursive, {_Recursive, UseRealPath}) -> {true, UseRealPath};
add_open_option(real_path, {Recursive, _UseRealPath}) -> {Recursive, true}.

%% }}}
%%----------------------------------------------------------

%% @doc Close a handle.

-spec close(handle()) ->
  ok.

close(Handle) ->
  try
    unlink(Handle),
    port_close(Handle)
  catch
    % this could be caused by port already being closed, which is expected in
    % some cases
    error:badarg -> ignore
  end,
  ok.

%% @doc Assign a new owner to a handle.

-spec controlling_process(handle(), pid()) ->
  ok | {error, not_owner | closed | badarg}.

controlling_process(Handle, Pid) ->
  try erlang:port_info(Handle, connected) of
    {connected, Pid} ->
      ok; % already the owner
    {connected, Owner} when Owner /= self() ->
      {error, not_owner};
    {connected, _OldOwner} ->
      try
        port_connect(Handle, Pid),
        unlink(Handle),
        ok
      catch
        _:_ ->
          {error, closed}
      end;
    undefined ->
      {error, closed}
  catch
    _:_ ->
      {error, badarg}
  end.

%% @doc Monitor a file or directory.
%%
%%   If the path was already watched, any previous flags will be replaced.
%%
%%   `close' and `move' flags are shorthands for `close_write'
%%   + `close_nowrite' and for `move_from' + `move_to', respectively.

-spec add(handle(), file:filename(), [flag() | close | move | Option]) ->
  ok | {error, badarg | posix()}
  when Option :: follow_symlink | unwatch_on_unlink | once | if_dir.

add(Handle, Path, Flags) ->
  case build_flags(Flags, add) of
    {ok, Value} ->
      try port_control(Handle, 1, [<<Value:32>>, Path]) of
        <<>> -> ok;
        ErrorName -> {error, binary_to_atom(ErrorName, latin1)}
      catch
        _:_ -> {error, badarg}
      end;
    {error, badarg} ->
      {error, badarg}
  end.

%% @doc Add events to watch for for a file or directory.
%%
%%   If the path was not watched yet, it's added.
%%
%%   `close' and `move' flags are shorthands for `close_write'
%%   + `close_nowrite' and for `move_from' + `move_to', respectively.

-spec update(handle(), file:filename(), [flag() | close | move | Option]) ->
  ok | {error, badarg | posix()}
  when Option :: follow_symlink | unwatch_on_unlink | once | if_dir.

update(Handle, Path, Flags) ->
  case build_flags(Flags, update) of
    {ok, Value} ->
      try port_control(Handle, 1, [<<Value:32>>, Path]) of
        <<>> -> ok;
        ErrorName -> {error, binary_to_atom(ErrorName, latin1)}
      catch
        _:_ -> {error, badarg}
      end;
    {error, badarg} ->
      {error, badarg}
  end.

%%----------------------------------------------------------
%% build_flags() {{{

%% @doc Translate list of flags to an integer to be sent to port.

-spec build_flags(Flags :: [Flag], add | update) ->
  {ok, non_neg_integer()} | {error, badarg}
  when Flag :: flag() | follow_symlink | unwatch_on_unlink | once | if_dir.

build_flags(Flags, Op) ->
  % NOTE: start with "don't follow symlinks" set and subtract it if
  % `follow_symlink' flag is present
  try lists:foldl(fun add_flag/2, 16#1000, Flags) of
    Result when Op == add    -> {ok, Result};
    Result when Op == update -> {ok, Result bor 16#010000}
  catch
    _:_ -> {error, badarg}
  end.

%% @doc Workhorse for {@link build_flags/1}.

-spec add_flag(Flag, non_neg_integer()) ->
  non_neg_integer()
  when Flag :: flag() | follow_symlink | unwatch_on_unlink | once | if_dir.

add_flag(access,            Flags) -> Flags bor 16#0001;
add_flag(modify,            Flags) -> Flags bor 16#0002;
add_flag(attrib,            Flags) -> Flags bor 16#0004;
add_flag(create,            Flags) -> Flags bor 16#0008;
add_flag(delete,            Flags) -> Flags bor 16#0010;
add_flag(open,              Flags) -> Flags bor 16#0020;
add_flag(close,             Flags) -> Flags bor 16#0040 bor 16#0080;
add_flag(close_write,       Flags) -> Flags bor 16#0040;
add_flag(close_nowrite,     Flags) -> Flags bor 16#0080;
add_flag(move,              Flags) -> Flags bor 16#0100 bor 16#0200;
add_flag(move_from,         Flags) -> Flags bor 16#0100;
add_flag(move_to,           Flags) -> Flags bor 16#0200;
add_flag(delete_self,       Flags) -> Flags bor 16#0400;
add_flag(move_self,         Flags) -> Flags bor 16#0800;
add_flag(follow_symlink,    Flags) -> Flags band bnot 16#1000; % reverse flag
add_flag(unwatch_on_unlink, Flags) -> Flags bor 16#2000;
add_flag(once,              Flags) -> Flags bor 16#4000;
add_flag(if_dir,            Flags) -> Flags bor 16#8000.

%% }}}
%%----------------------------------------------------------

%% @doc Stop watching a file or directory.
%%
%%   After this function is called, the owner will receive {@type message()}
%%   with `watch_removed' flag.

-spec remove(handle(), file:filename()) ->
  ok | {error, badarg}.

remove(Handle, Path) ->
  try port_control(Handle, 2, Path) of
    <<>> -> ok;
    ErrorName -> {error, binary_to_atom(ErrorName, latin1)}
  catch
    _:_ -> {error, badarg}
  end.

%% @doc List watched paths.

-spec list(handle()) ->
  {ok, [Entry]} | {error, badarg}
  when Entry :: {file:filename(), [flag()]}.

list(Handle) ->
  try port_control(Handle, 3, <<>>) of
    <<Count:32>> ->
      Entries = receive_listing(Handle, Count, []),
      {ok, Entries}
  catch
    _:_ -> {error, badarg}
  end.

%% @doc Workhorse for {@link list/1}.

-spec receive_listing(handle(), non_neg_integer(), [Entry]) ->
  [Entry]
  when Entry :: {file:filename(), [flag()]}.

receive_listing(_Handle, 0 = _Count, Entries) ->
  Entries;
receive_listing(Handle, Count, Entries) when Count > 0 ->
  receive
    {inotify_listing, Handle, _WD, Path, Flags} ->
      receive_listing(Handle, Count - 1, [{Path, Flags} | Entries])
  end.

%%%---------------------------------------------------------------------------

%% @doc Format a reason from error tuple as a usable error message.

-spec format_error(Reason :: term()) ->
  string().

format_error(queue_overflow) ->
  "overflow in inotify event queue";
format_error(badarg) ->
  "bad argument";
format_error(closed) ->
  "file descriptor closed";
format_error(Errno) ->
  inet:format_error(Errno).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
