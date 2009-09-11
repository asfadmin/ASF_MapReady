#ifdef win32

#define BYTE __byte
#include "req.h"
#undef BYTE

#include <windows.h>
#include <shlobj.h>

#else
#include "req.h"
#endif

SIGNAL_CALLBACK void
on_output_dir_browse_button_clicked(GtkWidget *widget)
{
#ifdef win32
    GtkWidget *e = get_widget_checked("output_directory_entry");
    
    BROWSEINFO bi = { 0 };
    bi.lpszTitle = "Select Output Directory";
    LPITEMIDLIST pidl = SHBrowseForFolder ( &bi );
    if ( pidl != 0 )
    {
        TCHAR path[MAX_PATH];
        if ( SHGetPathFromIDList ( pidl, path ) )
        {
            gtk_entry_set_text(GTK_ENTRY(e), path);
        }
    }
#else
    // not working on Linux yet!
#endif
}

SIGNAL_CALLBACK void
on_csv_dir_browse_button_clicked(GtkWidget *widget)
{
#ifdef win32
    GtkWidget *e = get_widget_checked("csv_directory_entry");
    
    BROWSEINFO bi = { 0 };
    bi.lpszTitle = "Select CSV Directory";
    LPITEMIDLIST pidl = SHBrowseForFolder ( &bi );
    if ( pidl != 0 )
    {
        TCHAR path[MAX_PATH];
        if ( SHGetPathFromIDList ( pidl, path ) )
        {
            gtk_entry_set_text(GTK_ENTRY(e), path);
        }
    }
#else
    // not working on Linux yet!
#endif
}
