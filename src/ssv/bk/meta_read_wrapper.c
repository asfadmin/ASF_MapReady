// Implementation of the interface described in meta_read_wrapper.

#include <glib.h>

#include "meta_read_wrapper.h"

G_LOCK_DEFINE_STATIC (meta_read);

meta_parameters *
meta_read_wrapper (const char *inName)
{
  meta_parameters *result;

  G_LOCK (meta_read);
  {
    result = meta_read (inName);
  }
  G_UNLOCK (meta_read);

  return result;
}
