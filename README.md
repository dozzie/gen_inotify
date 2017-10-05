inotify bindings for Erlang
===========================

`gen_inotify` is an Erlang bindings library to `inotify(7)` Linux subsystem to
watch filesystem events.

`/proc` interface
-----------------

Details are in `inotify(7)` man page.

* `/proc/sys/fs/inotify/max_queued_events`
* `/proc/sys/fs/inotify/max_user_instances`
* `/proc/sys/fs/inotify/max_user_watches`

Known limitations and problems
------------------------------

* robust watching of a directory with its subdirectories is impossible, so
  "best effort" heuristics is used
* watches on files that got renamed report the original filename
* removing a watch on a file that got renamed requires using the original
  filename

Requirements
------------

Linux kernel 2.6.36 or above and glibc 2.9 or above. These requirements were
satisfied long before Debian 7 Squeeze.

Contact and license
-------------------

`gen_inotify` library is written by Stanislaw Klekot <dozzie at jarowit.net>.
The primary distribution point is <http://dozzie.jarowit.net/>.

`gen_inotify` library is distributed under 3-clause BSD license. See COPYING
file for details.
