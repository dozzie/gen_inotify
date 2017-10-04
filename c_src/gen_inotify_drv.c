//----------------------------------------------------------------------------
// preamble

//----------------------------------------------------------
// unix OS {{{

#include <stdint.h>
#include <sys/inotify.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

// }}}
//----------------------------------------------------------
// Erlang port driver {{{

#include <erl_driver.h>
#include <ei.h> // Erlang term manipulation

// }}}
//----------------------------------------------------------
// definitions {{{

#define PORT_DRIVER_NAME      "gen_inotify_drv"
#define PORT_DRIVER_NAME_SYM   gen_inotify_drv

#define PORT_DRIVER_NAME_LEN (sizeof(PORT_DRIVER_NAME) - 1)

#define FLAG_ACCESS            0x0001
#define FLAG_MODIFY            0x0002
#define FLAG_ATTRIB            0x0004
#define FLAG_CREATE            0x0008
#define FLAG_DELETE            0x0010
#define FLAG_OPEN              0x0020
#define FLAG_CLOSE_WRITE       0x0040
#define FLAG_CLOSE_NOWRITE     0x0080
#define FLAG_MOVED_FROM        0x0100
#define FLAG_MOVED_TO          0x0200
#define FLAG_DELETE_SELF       0x0400
#define FLAG_MOVE_SELF         0x0800

#define FLAG_DONT_FOLLOW       0x1000
#define FLAG_EXCL_UNLINK       0x2000
#define FLAG_ONESHOT           0x4000
#define FLAG_ONLYDIR           0x8000

#define FLAG_MASK_ADD        0x010000

// }}}
//----------------------------------------------------------

//----------------------------------------------------------------------------
// Erlang port driver API

struct watch {
  int wd;         // watch descriptor
  uint32_t flags;
  ErlDrvBinary *path;
};

struct inotify_context {
  ErlDrvPort erl_port;
  int fd;
  struct watch *paths; // order by `wd' field
  size_t npaths;
  size_t alloc_paths;
};

static uint32_t flags_to_inotify(uint32_t flags);
static int send_inotify_event(struct inotify_context *context, struct inotify_event *event);

// tuple tags
static ErlDrvTermData atom_inotify;
static ErlDrvTermData atom_inotify_error;
// errors
static ErlDrvTermData atom_queue_overflow;
// event flags
static ErlDrvTermData atom_watch_removed;
static ErlDrvTermData atom_is_dir;
static ErlDrvTermData atom_unmount;
static ErlDrvTermData atom_access;
static ErlDrvTermData atom_modify;
static ErlDrvTermData atom_attrib;
static ErlDrvTermData atom_create;
static ErlDrvTermData atom_delete;
static ErlDrvTermData atom_open;
static ErlDrvTermData atom_close_write;
static ErlDrvTermData atom_close_nowrite;
static ErlDrvTermData atom_move_from;
static ErlDrvTermData atom_move_to;
static ErlDrvTermData atom_delete_self;
static ErlDrvTermData atom_move_self;

//----------------------------------------------------------
// entry point definition {{{

static int          cdrv_init(void);
static ErlDrvData   cdrv_start(ErlDrvPort port, char *cmd);
static void         cdrv_stop(ErlDrvData drv_data);
static ErlDrvSSizeT cdrv_control(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen);
static void         cdrv_ready_input(ErlDrvData drv_data, ErlDrvEvent event);
static void         cdrv_stop_select(ErlDrvEvent event, void *reserved);

ErlDrvEntry driver_entry = {
  cdrv_init,                    // int        init(void)
  cdrv_start,                   // ErlDrvData start(ErlDrvPort port, char *cmd)
  cdrv_stop,                    // void       stop(ErlDrvData drv_data)
  NULL,                         // void       output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len) // port_command/2 handler
  cdrv_ready_input,             // void       ready_input(ErlDrvData, ErlDrvEvent)  // "ready for reading" event
  NULL,                         // void       ready_output(ErlDrvData, ErlDrvEvent) // "ready for writing" event
  PORT_DRIVER_NAME,             // <driver name>
  NULL,                         // void       finish(void)
  NULL,                         // <reserved>
  cdrv_control,                 // int        control(...) // port_control/3 handler
  NULL,                         // void       timeout(ErlDrvData drv_data)
  NULL,                         // void       outputv(ErlDrvData drv_data, ErlIOVec *ev) // port_command/2 handler, faster
  NULL,                         // void       ready_async(ErlDrvData drv_data, ErlDrvThreadData thread_data)
  NULL,                         // void       flush(ErlDrvData drv_data)
  NULL,                         // int        call(...) // erlang:port_call/3 handler
  NULL,                         // void       event(ErlDrvData drv_data, ErlDrvEvent event, ErlDrvEventData event_data)
  ERL_DRV_EXTENDED_MARKER,
  ERL_DRV_EXTENDED_MAJOR_VERSION,
  ERL_DRV_EXTENDED_MINOR_VERSION,
  ERL_DRV_FLAG_USE_PORT_LOCKING,  // driver flags
  NULL,                         // <reserved>
  NULL,                         // void  process_exit(...) // called when monitored process dies
  cdrv_stop_select              // void  stop_select(ErlDrvEvent event, void *reserved) // called to close an event object
};

// the same as <driver name> in structure above, but as identifer instead of
// string
DRIVER_INIT(PORT_DRIVER_NAME_SYM)
{
  return &driver_entry;
}

// }}}
//----------------------------------------------------------
// Erlang port driver initialization {{{

static
int cdrv_init(void)
{
  atom_inotify_error  = driver_mk_atom("inotify_error");
  atom_queue_overflow = driver_mk_atom("queue_overflow");
  atom_inotify        = driver_mk_atom("inotify");
  atom_watch_removed  = driver_mk_atom("watch_removed");
  atom_is_dir         = driver_mk_atom("is_dir");
  atom_unmount        = driver_mk_atom("unmount");
  atom_access         = driver_mk_atom("access");
  atom_modify         = driver_mk_atom("modify");
  atom_attrib         = driver_mk_atom("attrib");
  atom_create         = driver_mk_atom("create");
  atom_delete         = driver_mk_atom("delete");
  atom_open           = driver_mk_atom("open");
  atom_close_write    = driver_mk_atom("close_write");
  atom_close_nowrite  = driver_mk_atom("close_nowrite");
  atom_move_from      = driver_mk_atom("move_from");
  atom_move_to        = driver_mk_atom("move_to");
  atom_delete_self    = driver_mk_atom("delete_self");
  atom_move_self      = driver_mk_atom("move_self");

  return 0;
}

// }}}
//----------------------------------------------------------
// Erlang port start {{{

static
ErlDrvData cdrv_start(ErlDrvPort port, char *cmd)
{
  struct inotify_context *context =
    driver_alloc(sizeof(struct inotify_context));

  context->erl_port = port;
  context->paths = NULL;
  context->npaths = 0;
  context->alloc_paths = 0;
  context->fd = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
  if (context->fd < 0) {
    driver_free(context);
    return ERL_DRV_ERROR_ERRNO;
  }

  ErlDrvEvent event = (ErlDrvEvent)((long int)context->fd);
  driver_select(context->erl_port, event, ERL_DRV_USE | ERL_DRV_READ, 1);

  // port_control() should return binaries
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

  return (ErlDrvData)context;
}

// }}}
//----------------------------------------------------------
// Erlang port stop {{{

static
void cdrv_stop(ErlDrvData drv_data)
{
  struct inotify_context *context = (struct inotify_context *)drv_data;

  ErlDrvEvent event = (ErlDrvEvent)((long int)context->fd);
  driver_select(context->erl_port, event, ERL_DRV_USE | ERL_DRV_READ, 0);

  if (context->paths != NULL) {
    while (context->npaths > 0)
      driver_free_binary(context->paths[--context->npaths].path);
    driver_free(context->paths);
  }

  driver_free(context);
}

// }}}
//----------------------------------------------------------
// Erlang event close (after port stop) {{{

static
void cdrv_stop_select(ErlDrvEvent event, void *reserved)
{
  long int fd = (long int)event;
  close(fd);
}

// }}}
//----------------------------------------------------------
// Erlang port control {{{

static size_t store_errno(int error, char *buf, ErlDrvSizeT len);

static
ErlDrvSSizeT cdrv_control(ErlDrvData drv_data, unsigned int command,
                          char *buf, ErlDrvSizeT len,
                          char **rbuf, ErlDrvSizeT rlen)
{
  struct inotify_context *context = (struct inotify_context *)drv_data;

  uint32_t flags;
  char path[PATH_MAX];
  int result;

  switch (command) {
    case 1: // add/update watch {{{
      flags = flags_to_inotify((uint8_t)buf[0] << (8 * 3) |
                               (uint8_t)buf[1] << (8 * 2) |
                               (uint8_t)buf[2] << (8 * 1) |
                               (uint8_t)buf[3] << (8 * 0));
      if (realpath(buf + 4, path) == NULL)
        return store_errno(errno, *rbuf, rlen);

      if ((result = inotify_add_watch(context->fd, path, flags)) >= 0) {
        // `result' is a watch descriptor
        // TODO: store it somewhere
        return 0;
      } else { // error
        return store_errno(errno, *rbuf, rlen);
      }
    // }}}

    case 2: // remove watch {{{
      if (realpath(buf, path) == NULL)
        return store_errno(errno, *rbuf, rlen);

      // TODO: find the watch descriptor; if not found, return 0
      //
      // if (inotify_rm_watch(context->fd, wd) < 0)
      //   return store_errno(errno, *rbuf, rlen);

      return 0;
    // }}}

    //case 3: // list watches
    //  TODO: send watch listing to driver_caller(context->erl_port)
    //  return 0;

    default: // unknown request
      return -1;
  }

  return -1; // never reached
}

static
size_t store_errno(int error, char *buf, ErlDrvSizeT len)
{
  char *error_str = erl_errno_id(errno);
  size_t i;
  // let's hope that `rlen' is long enough; it's just a dozen bytes top, so it
  // should be
  for (i = 0; error_str[i] != 0 && i < len; ++i)
    buf[i] = error_str[i];
  return i;
}

// }}}
//----------------------------------------------------------
// Erlang input on select descriptor {{{

static
void cdrv_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
  struct inotify_context *context = (struct inotify_context *)drv_data;
  // `event' is the input descriptor

  char buffer[sizeof(struct inotify_event) + NAME_MAX + 1];

  int result = read((long)event, buffer, sizeof(buffer));
  if (result < 0) {
    driver_failure_posix(context->erl_port, errno);
    return;
  }
  if (result == 0) {
    driver_failure_eof(context->erl_port);
    return;
  }

  struct inotify_event *ievent = (struct inotify_event *)buffer;

  // short circuit for overflow error
  if ((ievent->mask & IN_Q_OVERFLOW) != 0) {
    ErlDrvTermData message[] = {
      ERL_DRV_ATOM, atom_inotify_error,
      ERL_DRV_PORT, driver_mk_port(context->erl_port),
      ERL_DRV_ATOM, atom_queue_overflow,
      ERL_DRV_TUPLE, 3
    };

    driver_output_term(context->erl_port, message,
                       sizeof(message) / sizeof(message[0]));
    driver_failure_eof(context->erl_port);
    return;
  }

  // TODO: add recursively if ((ievent->mask & IN_ISDIR) != 0)

  send_inotify_event(context, ievent);
}

// }}}
//----------------------------------------------------------

//----------------------------------------------------------------------------
// send_inotify_event() {{{

static
int send_inotify_event(struct inotify_context *context,
                       struct inotify_event *event)
{
  // XXX: watch the size of this array and maximum message size
  ErlDrvTermData message[64];
  size_t len = 0;

  message[len++] = ERL_DRV_ATOM;
  message[len++] = atom_inotify;
  message[len++] = ERL_DRV_PORT;
  message[len++] = driver_mk_port(context->erl_port);

  // TODO: find path for `event->wd' and append `event->name' if non-empty
  if (event->len > 0) {
    message[len++] = ERL_DRV_INT;
    message[len++] = event->wd;

    message[len++] = ERL_DRV_STRING;
    message[len++] = (ErlDrvTermData)event->name;
    message[len++] = strlen(event->name);

    message[len++] = ERL_DRV_TUPLE;
    message[len++] = 2;
  } else {
    message[len++] = ERL_DRV_INT;
    message[len++] = event->wd;

    message[len++] = ERL_DRV_ATOM;
    message[len++] = driver_mk_atom("self");

    message[len++] = ERL_DRV_TUPLE;
    message[len++] = 2;
  }

  size_t nflags = 0;

#define ADD_FLAG(flag, atom) \
  if ((event->mask & flag) != 0) { \
    ++nflags; \
    message[len++] = ERL_DRV_ATOM; \
    message[len++] = atom; \
  }

  ADD_FLAG(IN_IGNORED, atom_watch_removed);
  ADD_FLAG(IN_ISDIR,   atom_is_dir);
  ADD_FLAG(IN_UNMOUNT, atom_unmount);

  ADD_FLAG(IN_ACCESS,        atom_access);
  ADD_FLAG(IN_MODIFY,        atom_modify);
  ADD_FLAG(IN_ATTRIB,        atom_attrib);
  ADD_FLAG(IN_CREATE,        atom_create);
  ADD_FLAG(IN_DELETE,        atom_delete);
  ADD_FLAG(IN_OPEN,          atom_open);
  ADD_FLAG(IN_CLOSE_WRITE,   atom_close_write);
  ADD_FLAG(IN_CLOSE_NOWRITE, atom_close_nowrite);
  ADD_FLAG(IN_MOVED_FROM,    atom_move_from);
  ADD_FLAG(IN_MOVED_TO,      atom_move_to);
  ADD_FLAG(IN_DELETE_SELF,   atom_delete_self);
  ADD_FLAG(IN_MOVE_SELF,     atom_move_self);

  message[len++] = ERL_DRV_NIL;
  message[len++] = ERL_DRV_LIST;
  message[len++] = nflags + 1 /* for ERL_DRV_NIL */;

  message[len++] = ERL_DRV_TUPLE;
  message[len++] = 4; // {inotify, Port, Path, Flags}

  return driver_output_term(context->erl_port, message, len);
}

// }}}
//----------------------------------------------------------------------------
// flags translation {{{

static
uint32_t flags_to_inotify(uint32_t flags)
{
  uint32_t result = 0;

  if ((flags & FLAG_ACCESS)        != 0) result |= IN_ACCESS;
  if ((flags & FLAG_MODIFY)        != 0) result |= IN_MODIFY;
  if ((flags & FLAG_ATTRIB)        != 0) result |= IN_ATTRIB;
  if ((flags & FLAG_CREATE)        != 0) result |= IN_CREATE;
  if ((flags & FLAG_DELETE)        != 0) result |= IN_DELETE;
  if ((flags & FLAG_OPEN)          != 0) result |= IN_OPEN;
  if ((flags & FLAG_CLOSE_WRITE)   != 0) result |= IN_CLOSE_WRITE;
  if ((flags & FLAG_CLOSE_NOWRITE) != 0) result |= IN_CLOSE_NOWRITE;
  if ((flags & FLAG_MOVED_FROM)    != 0) result |= IN_MOVED_FROM;
  if ((flags & FLAG_MOVED_TO)      != 0) result |= IN_MOVED_TO;
  if ((flags & FLAG_DELETE_SELF)   != 0) result |= IN_DELETE_SELF;
  if ((flags & FLAG_MOVE_SELF)     != 0) result |= IN_MOVE_SELF;
  if ((flags & FLAG_DONT_FOLLOW)   != 0) result |= IN_DONT_FOLLOW;
  if ((flags & FLAG_EXCL_UNLINK)   != 0) result |= IN_EXCL_UNLINK;
  if ((flags & FLAG_ONESHOT)       != 0) result |= IN_ONESHOT;
  if ((flags & FLAG_ONLYDIR)       != 0) result |= IN_ONLYDIR;
  if ((flags & FLAG_MASK_ADD)      != 0) result |= IN_MASK_ADD;

  return result;
}

// }}}
//----------------------------------------------------------------------------
// vim:ft=c:foldmethod=marker:nowrap
