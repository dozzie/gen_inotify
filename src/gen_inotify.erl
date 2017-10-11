%%%---------------------------------------------------------------------------
%%% @doc
%%%   `inotify(7)' bindings for Erlang.
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_inotify).

%% public interface
-export([open/0, close/1]).
-export([add/3, update/3, remove/2, list/1, count/1]).
-export([controlling_process/2]).
-export([format_error/1]).

-export_type([handle/0, message/0, message_event/0, message_error/0, posix/0]).
-export_type([flag/0, flag_event/0]).

%%%---------------------------------------------------------------------------
%%% types

-define(DRIVER_NAME, "gen_inotify_drv").

-type handle() :: port().
%% <i>inotify</i> port handle.

-type cookie() :: non_neg_integer().
%% Cookie to correlate events that compose a more complex event. Currently
%% only `move_from' and `move_to' events use the cookie.

-type message() :: message_event() | message_scan() | message_error().
%% Filesystem event, directory listing, <i>inotify</i> error.
%%
%% {@type message_scan()} messages are only sent when a directory was added to
%% watch list with `scan' flag and they carry the directory's current content,
%% for which no normal events were generated (with the exception of a small
%% race condition).
%%
%% {@type message_scan()} messages follow the structure of regular events
%% ({@type message_event()}) to allow newly created files and files already
%% existing in the directory to be processed in the same function clause or
%% `case' branch.

-type message_scan() ::
  {inotify, Handle :: handle(), Path :: file:filename(),
    Cookie :: cookie(),
    Flags :: [is_dir | present | {name, Basename :: file:filename()}, ...]}.
%% Message sent for a a directory added with `scan' option. The message
%% describes a file or (sub)directory that was already present in the
%% directory at the moment of adding a watch.
%%
%% `Flags' for a subdirectory is a three elements list in following order:
%% `[is_dir, present, {name,Basename}]'.
%%
%% `Flags' list for a file has only two elements, `[present, {name,Basename}]'
%% (the order is kept, similar to messages for subdirectories).
%%
%% `{name,Basename}' tuple is a convenience helper which carries the last
%% segment of `Path'. It allows to handle dot-files (`.foo') in pattern match,
%% for instance.
%%
%% Note that `scan' option will trigger read events, like `open' or
%% `close_nowrite'.

-type message_event() ::
  {inotify, Handle :: handle(), Path :: file:filename(),
    Cookie :: cookie(), Flags :: [flag() | flag_event(), ...]}.
%% Filesystem event at `Path'.
%%
%% `Flags' list describes the event. See {@type flag()} and
%% {@type flag_event()} for description of the meaning of each flag.

-type message_error() ::
  {inotify_error, Handle :: handle(),
    Error :: queue_overflow | {file:filename(), posix()}}.
%% Error encountered when watching files.
%%
%% `queue_overflow' can happen when a very busy directory is watched and
%% the port driver cannot keep up. When this happens, port driver terminates
%% and the owner process must open a new one.
%%
%% `{Path, Posix}' error is generated when a directory added with `scan' flag
%% could not be read.

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
%% Filesystem event type to watch for.
%%
%% <ul>
%%   <li>`access' -- watched object or its child was accessed/read (e.g. with
%%       `read(2)')</li>
%%   <li>`modify' -- watched object or its child was modified (e.g.
%%       `write(2)')</li>
%%   <li>`attrib' -- watched object's or its child's metadata was modified
%%       (e.g. permissions, owner, group, timestamps, link count, extended
%%       attributes)</li>
%%   <li>`create' -- new child was created (`open(..., O_CREAT)', `mkdir(2)',
%%       `mknod(2)', etc.) under watched object</li>
%%   <li>`delete' -- a child was removed (`unlink(2)', `rmdir(2)') under
%%       watched object</li>
%%   <li>`open' -- watched object or its child was opened (`open(2)')</li>
%%   <li>`close_write', `close_nowrite' -- watched object or its child was
%%       closed (`close(2)')</li>
%%   <li>`move_from', `move_to' -- a file or directory was moved from/to
%%       watched object (`rename(2)'); this event carries a non-zero cookie,
%%       to correlate `move_from' and `move_to' messages</li>
%%   <li>`delete_self' -- watched object was deleted</li>
%%   <li>`move_self' -- watched object was renamed</li>
%% </ul>
%%
%% <i>NOTE</i>: If a file under watched directory was renamed, both events
%% `move_from' and `move_to' will be generated, though there's no guarantee
%% that they will be consecutive (i.e., an arbitrary number of other events
%% may be recorded by <i>inotify</i> descriptor between the two).
%%
%% <i>NOTE</i>: It's possible to receive `move_from' event without
%% accompanying `move_to' or vice versa. This means that the file was moved
%% outside of the watched directory or from outside of the watched directory.

-type flag_event() :: watch_removed
                    | is_dir
                    | unmount.
%% An event flag additional to {@type flag()}.
%%
%% Flag `is_dir', denotes that the object that generated the event is
%% a directory. This flag, if present, is always the first element of the
%% flags list.
%%
%% `unmount' event signals that the filesystem, on which the watched object
%% was located, was unmounted. This flag is the only element of flags list, if
%% present.
%%
%% `watch_removed' event signals that the watched object will no longer be
%% generating any events. `watch_removed' event is sent for watches removed
%% both implicitly (e.g. `unlink(2)', `rmdir(2)', or after `unmount' event)
%% and explicitly (with {@link remove/2}). This flag is the only element of
%% flags list, if present.

-type posix() :: inet:posix().
%% Atom representation of an `errno' value.

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Open a new <i>inotify</i> handle.

-spec open() ->
  {ok, handle()} | {error, system_limit | posix()}.

open() ->
  try open_port({spawn_driver, ?DRIVER_NAME}, [binary]) of
    Handle -> {ok, Handle}
  catch
    error:Reason -> {error, Reason}
  end.

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
%%   If `Path' was already watched, any previous flags will be replaced.
%%
%%   `close' and `move' flags are shorthands for `close_write'
%%   + `close_nowrite' and for `move_from' + `move_to', respectively.
%%
%%   `scan' option causes {@type message_scan()} messages to be generated for
%%   files already present in the directory. It should usually be accompanied
%%   by `if_dir' option. Note that listing a directory produces `open' and
%%   `close_nowrite' events.
%%
%%   `if_dir' option causes the watch only to be added if the `Path' is
%%   a directory.
%%
%%   If `Path' is a symlink, `follow_symlink' option adds a watch for the
%%   symlink's target instead of watching symlink itself.
%%
%%   `unwatch_on_unlink' causes the watch not to generate events for children
%%   that were unlinked (e.g. `read(2)', `write(2)', and `close(2)' on a file
%%   that was unlinked immediately after `open(2)').
%%
%%   `once' makes the watch to be removed after the first event on `Path'.
%%   It's usually a poor idea to combine `once' and `scan' options.
%%
%% @see update/3

-spec add(handle(), file:filename(), [flag() | close | move | Option]) ->
  ok | {error, badarg | posix()}
  when Option :: scan | follow_symlink | unwatch_on_unlink | once | if_dir.

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
%%   If `Path' was not watched yet, it's added.
%%
%%   For detailed description of options, see {@link add/3} function.
%%
%% @see add/3

-spec update(handle(), file:filename(), [flag() | close | move | Option]) ->
  ok | {error, badarg | posix()}
  when Option :: scan | follow_symlink | unwatch_on_unlink | once | if_dir.

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
  when Flag :: flag()
             | scan | follow_symlink | unwatch_on_unlink | once | if_dir.

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
add_flag(if_dir,            Flags) -> Flags bor 16#8000;
add_flag(scan,              Flags) -> Flags bor 16#100000.

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
  [Entry]
  when Entry :: {file:filename(), [flag()]}.

list(Handle) ->
  try port_control(Handle, 3, <<>>) of
    <<Count:32>> -> receive_listing(Handle, Count, [])
  catch
    _:_ -> erlang:error(badarg)
  end.

%% @doc Count watched paths.

-spec count(handle()) ->
  non_neg_integer().

count(Handle) ->
  try port_control(Handle, 4, <<>>) of
    <<Count:32>> ->
      Count
  catch
    _:_ -> erlang:error(badarg)
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
format_error({Path, Errno}) ->
  "error while scanning " ++ Path ++ ": " ++ inet:format_error(Errno);
format_error(Errno) ->
  inet:format_error(Errno).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
