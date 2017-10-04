//----------------------------------------------------------------------------
// preamble

//----------------------------------------------------------
// unix OS {{{

#include <stdint.h>
#include <sys/inotify.h>
#include <unistd.h>
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

struct inotify_context {
  ErlDrvPort erl_port;
  // other fields
};

//----------------------------------------------------------
// entry point definition {{{

static ErlDrvData   cdrv_start(ErlDrvPort port, char *cmd);
static void         cdrv_stop(ErlDrvData drv_data);
static ErlDrvSSizeT cdrv_control(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen);
static void         cdrv_ready_input(ErlDrvData drv_data, ErlDrvEvent event);
static void         cdrv_stop_select(ErlDrvEvent event, void *reserved);

ErlDrvEntry driver_entry = {
  NULL,                         // int        init(void)
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
// Erlang port start {{{

static
ErlDrvData cdrv_start(ErlDrvPort port, char *cmd)
{
  struct inotify_context *context =
    driver_alloc(sizeof(struct inotify_context));

  // TODO: implement me
  context->erl_port = port;

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

  // TODO: implement me

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

static
ErlDrvSSizeT cdrv_control(ErlDrvData drv_data, unsigned int command,
                          char *buf, ErlDrvSizeT len,
                          char **rbuf, ErlDrvSizeT rlen)
{
  struct inotify_context *context = (struct inotify_context *)drv_data;

  // TODO: implement me

  return -1;
}

// }}}
//----------------------------------------------------------
// Erlang input on select descriptor {{{

static
void cdrv_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
  struct inotify_context *context = (struct inotify_context *)drv_data;
  // `event' is the input descriptor

  // TODO: implement me
}

// }}}
//----------------------------------------------------------

//----------------------------------------------------------------------------
// vim:ft=c:foldmethod=marker:nowrap
