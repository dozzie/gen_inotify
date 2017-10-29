inotify bindings for Erlang
===========================

`gen_inotify` is an Erlang bindings library to `inotify(7)` Linux subsystem to
watch filesystem events.

Usage example
-------------

    1> file:make_dir("/tmp/watch").
    ok
    2> {ok, H} = gen_inotify:open().
    {ok,#Port<0.620>}
    3> gen_inotify:add(H, "/tmp/watch", [if_dir, create, delete]).
    ok
    4> file:make_dir("/tmp/watch/subdir").
    ok
    5> file:del_dir("/tmp/watch/subdir").
    ok
    6> flush().
    Shell got {inotify,#Port<0.620>,"/tmp/watch/subdir",0,[is_dir,create]}
    Shell got {inotify,#Port<0.620>,"/tmp/watch/subdir",0,[is_dir,delete]}
    ok

`/proc` interface
-----------------

*inotify* has per-UID limits on number of sockets, events, and watches. These
limits can be read and modified using `/proc` interface.

* `/proc/sys/fs/inotify/max_user_instances` -- limit on number of open
  *inotify* descriptors
* `/proc/sys/fs/inotify/max_user_watches` -- limit on total number of watched
  objects (files and directories) across the *inotify* descriptors
* `/proc/sys/fs/inotify/max_queued_events` -- maximum number of unread
  filesystem events before a queue overflow message is generated and the
  events start being dropped

Details are described in `inotify(7)` man page.

Known limitations and problems
------------------------------

* watching a directory recursively in a robust way is impossible (e.g. all
  `open(2)` events between subdirectory creation and adding it to *inotify*
  descriptor are lost), so "best effort" heuristic is used
* file renames are not tracked by the port driver, thus messages about a file
  that got renamed carry the file's original path, and removing it from
  watching requires the original name as well
* unicode characters in filenames are not supported

Requirements
------------

Linux kernel 2.6.36 or above and glibc 2.9 or above. These requirements are
satisfied in Debian 7 Squeeze and CentOS 7.0.

Documentation
-------------

`gen_inotify` is documented using EDoc. A local copy is generated with
`make doc` command to `./doc/` directory. An already generated online copy is
available at <http://dozzie.jarowit.net/api/erlang-gen_inotify/>.

Contact and license
-------------------

`gen_inotify` library is written by Stanislaw Klekot <dozzie at jarowit.net>.
The primary distribution point is <http://dozzie.jarowit.net/>.

`gen_inotify` library is distributed under 3-clause BSD license. See COPYING
file for details.
