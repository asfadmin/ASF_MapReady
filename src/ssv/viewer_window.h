// A window used to view data.

#include <gtk/gtk.h>

#include "viewer_image.h"

typedef struct {

  // Public read-only members.

  ViewerImage *image;		// Image window is currently looking at.
  GPtrArray *images;            // Pointer to image list (owned by main()).
  // The maximum extents of the big picture, in big picture
  // coordinates.
  size_t max_x, max_y;
  // Window top left corner indicies in big picture coordinates.
  // FIIXME: nuke these two?
  size_t start_x, start_y;     
  gfloat zoom;			// screen pixels per image pixel.

  // Private members.  

  // The cursor is a single pixel in big picture coordinates that is
  // the subject of information inqueries.  If cursor_x and cursor_y
  // are both -1, it means the cursor is not placed.
  gint cursor_x, cursor_y;

  // The start location of a middle button drag in GTK window
  // coordinates.
  gint drag_start_x, drag_start_y;
  gdouble drag_start_hadj_value, drag_start_vadj_value;

  // Program to invoke when the 'a' key (for 'analysis') is pressed,
  // or NULL if no analysis program is specified.  This string forms
  // the start of the first argument to g_spawn_command_line_sync or
  // g_spawn_command_line_async.
  GString *analysis_program;
  // If true, we run the analysis program asynchronously.
  gboolean async_analysis;
  // The size of the (square) tile that will be analysed with 'a' is
  // pressed.
  gint analysis_tile_size;

  // Pointer to pixel data that needs to be g_free'd when we start a
  // new draw operation.  If this is NULL we don't have to free
  // anythin.
  gfloat *dp_to_free;
  GtkAdjustment *hadj, *vadj;	// Adjustments for scrolling control.
  GtkWidget *w;			// GtkWindow itself.
  GtkWidget *da;		// GtkDrawingArea we pack into the window.
  int reference_count;

} ViewerWindow;

// Default dimensions of new windows, in pixels.
#define VIEWER_WINDOW_DRAWING_AREA_DEFAULT_WIDTH 600
#define VIEWER_WINDOW_DRAWING_AREA_DEFAULT_HEIGHT 800

// Create a window looking at image which resides in big picture
// coordinate system extending from (0, 0) to (max_x + 1, max_y + 1)
// (making (max_x, max_y) the last addressable pixel), with upper left
// corner on pixel start_x, start_y in big picture coordinates,
// imaging an area w by h image pixels in size.  If w is less than
// VIEWER_WINDOW_DEFAULT_WIDTH and h is less than
// VIEWER_WINDOW_DEDFAULT_HEIGHT, a window of size w by h pixels is
// created, otherwise a window with the default width and height is
// created and the image data is zoomed st the entire region of
// interest is displayed.  Note that in either case, the aspect ratio
// of the region of interest is preserved.  The images GPtrArray
// contains a list of other ViewerImage instances that the window
// could display (in flicker mode, or by image switching).  The
// analysis_ arguments just set the corresponding member fields (the
// analysis_program option is copied if no NULL); see the description
// of those fields (if analysis_program is NULL, the others are
// irrelevant).
ViewerWindow *
viewer_window_new (ViewerImage *image, GPtrArray *images,
		   size_t max_x, size_t max_y,
		   size_t start_x, size_t start_y, size_t w, size_t h,
		   GString *analysis_program, gboolean async_analysis,
		   gint analysis_tile_size);

// Decrement reference count, freeing instance if count falls to 0.
// Note that if the instance is the only reference holder to a
// ViewerImage instance, deconstruction might take some time (see
// ViewerImage interface comments).
void
viewer_window_unref (ViewerWindow *self);

