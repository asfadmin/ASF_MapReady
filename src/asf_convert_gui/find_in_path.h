#ifndef FIND_IN_PATH_H
#define FIND_IN_PATH_H

#include <glib.h>
#include <string.h>
#include <stdlib.h>

gchar *find_in_path(gchar * file);
gchar *find_dir_in_path(gchar * file);

#endif
