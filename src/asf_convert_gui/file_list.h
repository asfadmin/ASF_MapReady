/*
  Imporant: If you update the columns (reorder, or add new ones) you must
  also update the list store create call, in file_list.c:setup_files_list().
*/
/*
#ifdef THUMBNAILS
enum FileListColumns
{
    COL_DATA_FILE = 0,
    COL_INPUT_THUMBNAIL,
    COL_OUTPUT_FILE,
    COL_OUTPUT_THUMBNAIL,
    COL_STATUS
};
#else
enum FileListColumns
{
    COL_DATA_FILE = 0,
    COL_OUTPUT_FILE,
    COL_STATUS
};
#endif
*/
extern int COL_DATA_FILE;
extern int COL_INPUT_THUMBNAIL;
extern int COL_OUTPUT_FILE;
extern int COL_OUTPUT_THUMBNAIL;
extern int COL_STATUS;

void setup_files_list(int, char **);
gboolean add_to_files_list(const gchar *);
void update_all_extensions();
void set_output_name(GtkTreeIter *, const gchar *);
