
typedef struct
{
    gchar * prefix;
    gchar * suffix;
    gchar * scheme;
} NamingScheme;

NamingScheme * naming_scheme_new(const gchar *, const gchar *,
                                 const gchar *);
NamingScheme * naming_scheme_default();
void naming_scheme_delete(NamingScheme *);
gchar * naming_scheme_apply(const NamingScheme *, const gchar *);
gboolean naming_schemes_equal(const NamingScheme *, const NamingScheme *);
NamingScheme * naming_scheme_copy(const NamingScheme *);
