/* The parser interface function.  Takes a pre-allocated (but not
   filled in) meta_parameters structure, and fills in values from
   file_name.  Dies internally on parse error.  */
void parse_metadata(meta_parameters *dest, char *file_name);
