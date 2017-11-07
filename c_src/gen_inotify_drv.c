//----------------------------------------------------------------------------
// preamble

//----------------------------------------------------------
// unix OS {{{

#include <stdint.h>
#include <sys/inotify.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
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
#define FLAG_SCAN            0x100000

#define INOTIFY_MAX_EVENT_SIZE (sizeof(struct inotify_event) + NAME_MAX + 1)

#if (ERL_DRV_EXTENDED_MAJOR_VERSION > 3 || \
     (ERL_DRV_EXTENDED_MAJOR_VERSION == 3 && \
      ERL_DRV_EXTENDED_MINOR_VERSION >= 0))
// Erlang 17 (driver 3.0) deprecated driver_send_term() and
// driver_output_term() functions in favour of erl_drv_send_term() and
// erl_drv_output_term()
#  define DRIVER_SEND_TERM(port, receiver, term, n) \
            erl_drv_send_term(driver_mk_port(port), receiver, term, n)
#  define DRIVER_OUTPUT_TERM(port, term, n) \
            erl_drv_output_term(driver_mk_port(port), term, n)
#else
// before Erlang 17 (driver 3.0) erl_drv_send_term() and erl_drv_output_term()
// were not present
#  define DRIVER_SEND_TERM(port, receiver, term, n) \
            driver_send_term(port, receiver, term, n)
#  define DRIVER_OUTPUT_TERM(port, term, n) \
            driver_output_term(port, term, n)
#endif

// }}}
//----------------------------------------------------------

//----------------------------------------------------------------------------
// Erlang port driver API

struct watch {
  int wd;         // watch descriptor
  uint32_t flags;
  char *path; // this will actually hold (path_len + 1 + NAME_MAX + 1) bytes
  size_t path_len;
};

struct inotify_context {
  ErlDrvPort erl_port;
  int fd;
  struct watch *watches;
  size_t nwatches;
  size_t max_watches;
};

static ssize_t copy_path(char *path, size_t path_len, char *output);
static uint32_t flags_to_inotify(uint32_t flags);
static int send_inotify_event(struct inotify_context *context, struct inotify_event *event);
static int send_inotify_single_flag_event(struct inotify_context *context, struct inotify_event *event, ErlDrvTermData flag_atom);
static int send_inotify_error(struct inotify_context *context, ErlDrvTermData error_atom);
static int send_watch_entry(struct inotify_context *context, ErlDrvTermData receiver, struct watch *watch);
static void scan_directory(struct inotify_context *context, char *dir_path, size_t dir_path_len);

static int   watch_add(struct inotify_context *context, int wd, uint32_t flags, char *path);
static int   watch_find_wd(struct inotify_context *context, char *path);
static char* watch_find(struct inotify_context *context, struct inotify_event *event, size_t *path_len);
static void  watch_remove(struct inotify_context *context, int wd);

// tuple tags
static ErlDrvTermData atom_inotify;
static ErlDrvTermData atom_inotify_listing;
static ErlDrvTermData atom_inotify_error;
// errors
static ErlDrvTermData atom_queue_overflow;
static ErlDrvTermData atom_eol;
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
static ErlDrvTermData atom_present;
static ErlDrvTermData atom_name;

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
  atom_inotify_listing = driver_mk_atom("inotify_listing");
  atom_eol = driver_mk_atom("eol");
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
  atom_present        = driver_mk_atom("present");
  atom_name           = driver_mk_atom("name");

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
  context->watches = NULL;
  context->nwatches = 0;
  context->max_watches = 0;
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

  if (context->watches != NULL) {
    while (context->nwatches > 0)
      driver_free(context->watches[--context->nwatches].path);
    driver_free(context->watches);
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

  uint32_t request_flags;
  uint32_t inotify_flags;
  char path[PATH_MAX];
  int wd;

  struct watch *watch;
  ErlDrvTermData caller;

  switch (command) {
    case 1: // add/update watch {{{
      if (len < 5) // uint32_t + at least one character for filename
        return -1;

      request_flags = (uint8_t)buf[0] << (8 * 3) | (uint8_t)buf[1] << (8 * 2) |
                      (uint8_t)buf[2] << (8 * 1) | (uint8_t)buf[3] << (8 * 0);
      inotify_flags = flags_to_inotify(request_flags);
      if (copy_path(buf + 4, len - 4, path) < 0)
        return store_errno(errno, *rbuf, rlen);

      if ((wd = inotify_add_watch(context->fd, path, inotify_flags)) >= 0) {
        if (watch_add(context, wd, inotify_flags, path) != 0) {
          driver_failure_posix(context->erl_port, ENOMEM);
          return 0;
        }

        if ((request_flags & FLAG_SCAN) != 0)
          scan_directory(context, path, 0);

        return 0;
      } else { // error
        return store_errno(errno, *rbuf, rlen);
      }
    // }}}

    case 2: // remove watch {{{
      if (len < 1) // at least one character for filename
        return -1;

      if (copy_path(buf, len, path) < 0)
        return store_errno(errno, *rbuf, rlen);

      wd = watch_find_wd(context, path);
      if (wd >= 0 && inotify_rm_watch(context->fd, wd) < 0)
        return store_errno(errno, *rbuf, rlen);

      return 0;
    // }}}

    case 3: // list watches {{{
      if (4 > rlen) *rbuf = driver_alloc(4);
      caller = driver_caller(context->erl_port);

      for (watch = context->watches;
           watch < context->watches + context->nwatches;
           ++watch) {
        send_watch_entry(context, caller, watch);
      }
      (*rbuf)[0] = (context->nwatches >> (8 * 3)) & 0xff;
      (*rbuf)[1] = (context->nwatches >> (8 * 2)) & 0xff;
      (*rbuf)[2] = (context->nwatches >> (8 * 1)) & 0xff;
      (*rbuf)[3] = (context->nwatches >> (8 * 0)) & 0xff;
      return 4;
    // }}}

    case 4: // count watches {{{
      if (4 > rlen) *rbuf = driver_alloc(4);

      (*rbuf)[0] = (context->nwatches >> (8 * 3)) & 0xff;
      (*rbuf)[1] = (context->nwatches >> (8 * 2)) & 0xff;
      (*rbuf)[2] = (context->nwatches >> (8 * 1)) & 0xff;
      (*rbuf)[3] = (context->nwatches >> (8 * 0)) & 0xff;
      return 4;
    // }}}

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

  char buffer[INOTIFY_MAX_EVENT_SIZE * 32];

  int result = read((long)event, buffer, sizeof(buffer));
  if (result < 0) {
    if (errno != EAGAIN && errno != EWOULDBLOCK)
      driver_failure_posix(context->erl_port, errno);
    return;
  }
  if (result == 0) {
    driver_failure_eof(context->erl_port);
    return;
  }

  char *next_event = buffer;
  while (next_event < buffer + result) {
    struct inotify_event *ievent = (struct inotify_event *)next_event;
    next_event += sizeof(struct inotify_event) + ievent->len;

    // short circuit for overflow error
    if ((ievent->mask & IN_Q_OVERFLOW) != 0) {
      send_inotify_error(context, atom_queue_overflow);
      driver_failure_eof(context->erl_port);
      return;
    }

    // TODO: add recursively if ((ievent->mask & IN_ISDIR) != 0)

    // XXX: `watch_removed' and `unmount' flags are always sent as a separate
    // messages, though inotify docs don't guarantee that; if `unmount' and/or
    // `watch_removed' come with other flags, other flags are sent first, and
    // `watch_removed' is sent last

    if ((ievent->mask & ~(IN_UNMOUNT | IN_IGNORED)) != 0)
      send_inotify_event(context, ievent);

    if ((ievent->mask & IN_UNMOUNT) != 0)
      send_inotify_single_flag_event(context, ievent, atom_unmount);

    if ((ievent->mask & IN_IGNORED) != 0) {
      send_inotify_single_flag_event(context, ievent, atom_watch_removed);
      watch_remove(context, ievent->wd);
    }
  }
}

// }}}
//----------------------------------------------------------

//----------------------------------------------------------------------------
// scanning directory for already present files/subdirs {{{

static int scan_dir_send_entry(struct inotify_context *context,
                               char *path, size_t path_len,
                               char *basename, size_t basename_len,
                               int is_dir);
static int scan_dir_send_error(struct inotify_context *context,
                               char *path, size_t path_len,
                               ErlDrvTermData error_atom);
static int is_directory(char *path, struct dirent *dir_entry);

static
void scan_directory(struct inotify_context *context, char *dir_path,
                    size_t dir_path_len)
{
  if (dir_path_len == 0)
    dir_path_len = strlen(dir_path);

  if (dir_path_len >= PATH_MAX - 1)
    return;

  char file_path[PATH_MAX + NAME_MAX + 1]; // more than enough for a file path
  memcpy(file_path, dir_path, dir_path_len);
  file_path[dir_path_len] = 0;

  // dir_path is not guaranteed to be NUL terminated; use file_path instead
  DIR *dir = opendir(file_path);
  if (dir == NULL) {
    scan_dir_send_error(context, file_path, dir_path_len,
                        driver_mk_atom(erl_errno_id(errno)));
    return;
  }
  file_path[dir_path_len] = '/';

  struct dirent entry;
  struct dirent *result = NULL;
  while (readdir_r(dir, &entry, &result) == 0 && result != NULL) {
    if ((entry.d_name[0] == '.' && entry.d_name[1] == 0) ||
        (entry.d_name[0] == '.' && entry.d_name[1] == '.' &&
         entry.d_name[2] == 0))
      continue;

    size_t basename_len = strlen(entry.d_name);
    // XXX: OS guarantees that the name is NUL-terminated and short enough
    memcpy(file_path + dir_path_len + 1, entry.d_name, basename_len);

    scan_dir_send_entry(context, file_path, dir_path_len + 1 + basename_len,
                        entry.d_name, basename_len,
                        is_directory(file_path, &entry));
  }
  // the only possible error here is EBADF, according to `man readdir'
  closedir(dir);
}

static
int is_directory(char *path, struct dirent *dir_entry)
{
  struct stat buffer;
  return (dir_entry->d_type == DT_DIR) ||
         (dir_entry->d_type == DT_UNKNOWN &&
          lstat(path, &buffer) == 0 && S_ISDIR(buffer.st_mode));
}

static
int scan_dir_send_entry(struct inotify_context *context,
                        char *path, size_t path_len,
                        char *basename, size_t basename_len,
                        int is_dir)
{
  if (is_dir) {
    ErlDrvTermData message[] = {
      ERL_DRV_ATOM, atom_inotify,
      ERL_DRV_PORT, driver_mk_port(context->erl_port),
      ERL_DRV_STRING, (ErlDrvTermData)path, path_len,
      ERL_DRV_INT, 0, // cookie
        ERL_DRV_ATOM, atom_is_dir,
        ERL_DRV_ATOM, atom_present,
          ERL_DRV_ATOM, atom_name,
          ERL_DRV_STRING, (ErlDrvTermData)basename, basename_len,
        ERL_DRV_TUPLE, 2, // {name,Basename}
        ERL_DRV_NIL,
      ERL_DRV_LIST, 4,

      ERL_DRV_TUPLE, 5
    };
    return DRIVER_OUTPUT_TERM(context->erl_port, message,
                              sizeof(message) / sizeof(message[0]));
  } else {
    ErlDrvTermData message[] = {
      ERL_DRV_ATOM, atom_inotify,
      ERL_DRV_PORT, driver_mk_port(context->erl_port),
      ERL_DRV_STRING, (ErlDrvTermData)path, path_len,
      ERL_DRV_INT, 0, // cookie
        ERL_DRV_ATOM, atom_present,
          ERL_DRV_ATOM, atom_name,
          ERL_DRV_STRING, (ErlDrvTermData)basename, basename_len,
        ERL_DRV_TUPLE, 2, // {name,Basename}
        ERL_DRV_NIL,
      ERL_DRV_LIST, 3,

      ERL_DRV_TUPLE, 5
    };
    return DRIVER_OUTPUT_TERM(context->erl_port, message,
                              sizeof(message) / sizeof(message[0]));
  }
}

static
int scan_dir_send_error(struct inotify_context *context,
                         char *path, size_t path_len,
                         ErlDrvTermData error_atom)
{
  ErlDrvTermData message[] = {
    ERL_DRV_ATOM, atom_inotify_error,
    ERL_DRV_PORT, driver_mk_port(context->erl_port),
      ERL_DRV_STRING, (ErlDrvTermData)path, path_len,
      ERL_DRV_ATOM, error_atom,
      ERL_DRV_TUPLE, 2,
    ERL_DRV_TUPLE, 3
  };

  return DRIVER_OUTPUT_TERM(context->erl_port, message,
                            sizeof(message) / sizeof(message[0]));
}

// }}}
//----------------------------------------------------------------------------
// sending listing messages {{{

static
int send_watch_entry(struct inotify_context *context, ErlDrvTermData receiver,
                     struct watch *watch)
{
  // XXX: watch the size of this array and maximum message size
  ErlDrvTermData message[64];
  size_t len = 0;

  message[len++] = ERL_DRV_ATOM;
  message[len++] = atom_inotify_listing;
  message[len++] = ERL_DRV_PORT;
  message[len++] = driver_mk_port(context->erl_port);

  message[len++] = ERL_DRV_INT;
  message[len++] = (ErlDrvTermData)watch->wd;

  message[len++] = ERL_DRV_STRING;
  message[len++] = (ErlDrvTermData)watch->path;
  message[len++] = watch->path_len;

  size_t nflags = 0;

#define ADD_FLAG(flag, atom) \
  if ((watch->flags & flag) != 0) { \
    ++nflags; \
    message[len++] = ERL_DRV_ATOM; \
    message[len++] = atom; \
  }

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

#undef ADD_FLAG

  message[len++] = ERL_DRV_NIL;
  message[len++] = ERL_DRV_LIST;
  message[len++] = nflags + 1 /* for ERL_DRV_NIL */;

  message[len++] = ERL_DRV_TUPLE;
  message[len++] = 5; // {inotify_listing, Port, WD, Path, Flags}

  return DRIVER_SEND_TERM(context->erl_port, receiver, message, len);
}

// }}}
//----------------------------------------------------------------------------
// sending events {{{

static
int send_inotify_single_flag_event(struct inotify_context *context,
                                   struct inotify_event *event,
                                   ErlDrvTermData flag_atom)
{
  // XXX: watch the size of this array and maximum message size
  ErlDrvTermData message[20];
  size_t len = 0;

  message[len++] = ERL_DRV_ATOM;
  message[len++] = atom_inotify;
  message[len++] = ERL_DRV_PORT;
  message[len++] = driver_mk_port(context->erl_port);

  size_t path_len = 0;
  char *path = watch_find(context, event, &path_len);

  if (path != NULL) {
    message[len++] = ERL_DRV_STRING;
    message[len++] = (ErlDrvTermData)path;
    message[len++] = path_len;
  } else { // (path == NULL); this should never happen
    message[len++] = ERL_DRV_ATOM;
    message[len++] = driver_mk_atom("undefined");
  }

  message[len++] = ERL_DRV_UINT;
  message[len++] = event->cookie;

  message[len++] = ERL_DRV_ATOM;
  message[len++] = flag_atom;
  message[len++] = ERL_DRV_NIL;
  message[len++] = ERL_DRV_LIST;
  message[len++] = 2 /* atom + NIL (`[]') */;

  message[len++] = ERL_DRV_TUPLE;
  message[len++] = 5; // {inotify, Port, Path, Cookie, [Flag]}

  return DRIVER_OUTPUT_TERM(context->erl_port, message, len);
}

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

  size_t path_len = 0;
  char *path = watch_find(context, event, &path_len);

  if (path != NULL) {
    message[len++] = ERL_DRV_STRING;
    message[len++] = (ErlDrvTermData)path;
    message[len++] = path_len;
  } else { // (path == NULL); this should never happen
    message[len++] = ERL_DRV_ATOM;
    message[len++] = driver_mk_atom("undefined");
  }

  message[len++] = ERL_DRV_UINT;
  message[len++] = event->cookie;

  size_t nflags = 0;

#define ADD_FLAG(flag, atom) \
  if ((event->mask & flag) != 0) { \
    ++nflags; \
    message[len++] = ERL_DRV_ATOM; \
    message[len++] = atom; \
  }

  // XXX: this flag is promised to go first
  ADD_FLAG(IN_ISDIR, atom_is_dir);

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

#undef ADD_FLAG

  message[len++] = ERL_DRV_NIL;
  message[len++] = ERL_DRV_LIST;
  message[len++] = nflags + 1 /* for ERL_DRV_NIL */;

  message[len++] = ERL_DRV_TUPLE;
  message[len++] = 5; // {inotify, Port, Path, Cookie, Flags}

  return DRIVER_OUTPUT_TERM(context->erl_port, message, len);
}

static
int send_inotify_error(struct inotify_context *context,
                       ErlDrvTermData error_atom)
{
  ErlDrvTermData message[] = {
    ERL_DRV_ATOM, atom_inotify_error,
    ERL_DRV_PORT, driver_mk_port(context->erl_port),
    ERL_DRV_ATOM, error_atom,
    ERL_DRV_TUPLE, 3
  };

  return DRIVER_OUTPUT_TERM(context->erl_port, message,
                            sizeof(message) / sizeof(message[0]));
}

// }}}
//----------------------------------------------------------------------------
// watch management {{{

// function returns
//   * element with matching watch descriptor (if found)
//   * the first element with larger watch descriptor (if not found)
//   * one element past the array (if wd is larger than anything until now)
//   * first array element if there's no watches recorded
// for new descriptors, the function basically returns their insert place
static
struct watch* watch_lookup(struct inotify_context *context, int wd)
{
  if (context->nwatches == 0)
    return context->watches;
  if (wd < context->watches[0].wd)
    return context->watches;
  if (wd > context->watches[context->nwatches - 1].wd)
    return context->watches + context->nwatches;

  // XXX: [0] < wd < [nwatches - 1]

  size_t begin = 0;
  size_t end = context->nwatches;

  while (begin < end - 1) {
    size_t middle = begin + (end - begin) / 2;

    if (wd == context->watches[middle].wd)
      return context->watches + middle;
    else if (wd < context->watches[middle].wd)
      end = middle;
    else // wd > context->watches[middle].wd
      begin = middle;
  }

  if (wd > context->watches[begin].wd)
    // (wd < context->watches[begin + 1].wd), because otherwise we would
    // stop the bisection loop on (begin + 1) or later
    return context->watches + begin + 1;
  else
    // (wd <= context->watches[begin].wd)
    return context->watches + begin;
}

static
int watch_add(struct inotify_context *context, int wd, uint32_t flags,
              char *path)
{
  // make sure there's at least one element free
  if (context->watches == NULL) {
    context->max_watches = 64;
    size_t memsize = sizeof(struct watch) * context->max_watches;
    context->watches = driver_alloc(memsize);
  } else if (context->max_watches == context->nwatches) {
    context->max_watches *= 2;
    size_t memsize = sizeof(struct watch) * context->max_watches;
    context->watches = driver_realloc(context->watches, memsize);
  }
  if (context->watches == NULL) // out of memory
    return -1;

  int flags_reset = ((flags & IN_MASK_ADD) == 0);
  flags &= ~(IN_MASK_ADD | IN_DONT_FOLLOW | IN_ONESHOT | IN_ONLYDIR);

  struct watch *watch = watch_lookup(context, wd);
  if (context->nwatches > 0 && watch < context->watches + context->nwatches &&
      watch->wd == wd) {
    // watch is in the recorded part and it matches
    if (flags_reset)
      watch->flags = flags;
    else
      watch->flags |= flags;

    return 0;
  }

  // NOTE: `watch' is past the last recorded element or (wd < watch->wd)

  if (watch < context->watches + context->nwatches) {
    // not the last element of the array, so move elements by 1 to make space
    size_t nelems = context->nwatches - (watch - context->watches);
    memmove(watch + 1, watch, sizeof(*watch) * nelems);
  }
  ++context->nwatches;

  watch->wd = wd;
  watch->flags = flags;
  watch->path_len = strlen(path);
  // this must hold the actual path, slash, filename, and trailing NUL byte
  watch->path = driver_alloc(watch->path_len + 1 + NAME_MAX + 1);

  memcpy(watch->path, path, watch->path_len);
  watch->path[watch->path_len] = 0;

  return 0;
}

static
void watch_remove(struct inotify_context *context, int wd)
{
  if (context->nwatches == 0)
    return;

  struct watch *watch = watch_lookup(context, wd);
  if (watch == context->watches + context->nwatches || watch->wd != wd)
    return;

  driver_free(watch->path);

  if (watch < context->watches + context->nwatches - 1) {
    size_t nelems = context->nwatches - 1 - (watch - context->watches);
    memmove(watch, watch + 1, sizeof(*watch) * nelems);
  }
  --context->nwatches;
}

static
int watch_find_wd(struct inotify_context *context, char *path)
{
  size_t path_len = strlen(path);

  struct watch *end = context->watches + context->nwatches;
  struct watch *current;

  for (current = context->watches; current < end; ++current) {
    if (current->path_len == path_len &&
        strncmp(current->path, path, path_len) == 0)
      return current->wd;
  }

  return -1;
}

static
char* watch_find(struct inotify_context *context, struct inotify_event *event,
                 size_t *path_len)
{
  if (context->nwatches == 0)
    return NULL;

  struct watch *watch = watch_lookup(context, event->wd);
  if (watch == context->watches + context->nwatches || watch->wd != event->wd)
    return NULL;

  size_t name_len = watch->path_len;

  if (event->len > 0) {
    watch->path[name_len++] = '/';
    size_t event_name_len = event->len;
    while (event->name[event_name_len - 1] == 0)
      --event_name_len;
    memcpy(watch->path + name_len, event->name, event_name_len);
    name_len += event_name_len;
  }

  watch->path[name_len] = 0;

  if (path_len != NULL)
    *path_len = name_len;

  return watch->path;
}

// }}}
//----------------------------------------------------------------------------
// copy path to NUL-terminated buffer {{{

static
ssize_t copy_path(char *path, size_t path_len, char *output)
{
  if (path_len >= PATH_MAX) {
    errno = ENAMETOOLONG;
    return -1;
  }

  memcpy(output, path, path_len);
  output[path_len] = 0;
  return path_len;
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
