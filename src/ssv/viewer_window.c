// Implementation of the interface described in viewer_window.h.

#include <fcntl.h>
#include <math.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <gtk/gtkgl.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

#include <GL/gl.h>
#include <GL/glu.h>

#include <gsl/gsl_math.h>

#include "space_2d.h"
#include "utilities.h"
#include "viewer_window.h"

#ifdef G_LOG_DOMAIN
#  undef G_LOG_DOMAIN
#endif
#define G_LOG_DOMAIN "ViewerWindow"

// Flag true iff initialize_libraries has been run.
static gboolean initialized = FALSE;

// OpenGL configuration we use.
static GdkGLConfig *glconfig = NULL;

// Throw a fatal exception if an OpenGL error is found.
static void
trap_opengl_errors (void)
{
  GLenum gl_error_code = glGetError ();
  
  if ( gl_error_code != GL_NO_ERROR ) {
    g_error (__FILE__ " line %d: OpenGL error detected: %s", __LINE__,
	     gluErrorString (gl_error_code));
  }
}

// Get a pointer to a blob of floating point data using fair means or
// foul.  We want a blob st the region defined by start_x, start_y and
// extending min (ceil (win_w / zoom), image->size_x) in the x
// direction and min (ceil (win_h / zoom), image->size_y) in the y
// direction (all in base image coordinates) is fully represented at
// zoom factor zoom (as defined in ViewerWindow class) or better
// (better meaning more detailed).  We return a pointer to the region,
// the position of the upper left corner of the region with respect to
// the base, the width and height of the returned data region in
// pixels (actual pixels, not base pixels), the actual zoom factor of
// the data retrieved, and a flag indicating if the returned memory is
// otherwise unowned (if it is, then the caller is responsible for
// freeing it, otherwise the caller must not).
static void
get_data_pointer (ViewerImage *image, size_t start_x, size_t start_y,
		  gfloat zoom, gint win_w, gint win_h, float **dp,
		  size_t *dp_start_x, size_t *dp_start_y, 
		  size_t *dp_w, size_t *dp_h, gfloat *actual_zoom,
		  gboolean *unowned_memory)
{
  // Insist that (start_x, start_y) refers to an image pixel.
  g_assert (start_x >= 0);
  g_assert (start_x < image->size_x + image->x_offset);
  g_assert (start_y >= 0);
  g_assert (start_y < image->size_y + image->y_offset);
  
  // With a few small tweaks we might be able to make things work when
  // zoomed in to the point of pixelation, but I haven't thought about
  // it.
  if ( zoom > 1.0 ) {
    g_error ("zoom factor %f is greater than 1.0", zoom);
  }
  
  guint layer_zoom = lrint (pow (2.0, floor (log2 (1.0 / zoom))));
  ssize_t desired_width = GSL_MIN (ceil (win_w / zoom),
				   image->size_x - start_x);
  ssize_t desired_height = GSL_MIN (ceil (win_h / zoom),
				    image->size_y - start_y);

  if ( viewer_image_pyramid (image) != NULL ) {
    *actual_zoom = 1.0 / layer_zoom;
    // Ensure that floating point math has worked as expected since we
    // validated the zoom input parameter.
    g_assert (*actual_zoom <= 1.0);
    pyramid_get_region (image->pyramid, start_x, start_y, desired_width,
			desired_height, layer_zoom, dp, dp_start_x,
			dp_start_y, dp_w, dp_h, unowned_memory);
  }    
  else {
    // The image pyramids aren't ready yet, so in the meantime we show
    // the user some data from the quickview image, or if that isn't
    // sufficiently high resolution, we form a new region.
    if ( zoom <= 1.0 / image->quick_view_stride ) {
      *dp = image->quick_view;
      *dp_start_x = 0;
      *dp_start_y = 0;
      *dp_w = image->qvw;
      *dp_h = image->qvh;
      *actual_zoom = 1.0 / image->quick_view_stride;
      *unowned_memory = FALSE;
    }
    else {
      // We don't try to center our sample pixels in the middle of the
      // areas they represent or anything, after all we are only doing
      // this while the pyramid is prepared.
      const size_t pixel_stride = layer_zoom;
      *dp_start_x = start_x;
      *dp_start_y = start_y;
      *dp_w = desired_width / pixel_stride;
      *dp_h = desired_height / pixel_stride;
      size_t dp_area = *dp_w * *dp_h;
      g_assert (dp_area > 0);
      *dp = g_new (float, dp_area);
      size_t ii, jj;
      size_t cr = 0;
      GString *data_name
	= my_g_string_new_printf ("%s.img", image->base_name->str);
      int data_fd = open (data_name->str, O_RDONLY);
      g_assert (data_fd != -1);
      float *row_buffer = g_new (float, desired_width);
      g_assert (row_buffer != NULL);
      for ( jj = start_y ; jj <= desired_height - pixel_stride + start_y ;
	    jj += pixel_stride, cr++ ) {
	size_t cc = 0;
	off_t row_offset = sizeof (float) * (jj * image->size_x + start_x);
	off_t lseek_result = lseek (data_fd, row_offset, SEEK_SET);
	g_assert (lseek_result == row_offset);
	// We don't bother trying to seek between samples in a row:
	// its highly unlikely to be any faster, even if the stride is
	// large.
	size_t bytes_to_read = sizeof (float) * desired_width;
	g_assert (bytes_to_read <= SSIZE_MAX);
	ssize_t bytes_read = read (data_fd, row_buffer, bytes_to_read);
	g_assert (bytes_read != -1);
	if ( bytes_read != bytes_to_read ) {
	  g_error ("bytes_read (%llu) != bytes_to_read (%lld) while "
		   "attempting to read from file %s",
		   (long long unsigned) bytes_read,
		   (long long int) bytes_to_read, data_name->str);
	}
	g_assert (bytes_read == bytes_to_read);
	for ( ii = start_x ; ii <= desired_width - pixel_stride + start_x ;
	      ii += pixel_stride, cc++ ) {
	  float pv = row_buffer[ii - start_x];
	  if ( G_BYTE_ORDER == G_LITTLE_ENDIAN ) {
	    swap_bytes_32 ((unsigned char *) &pv);
	  }
	  if ( cr * *dp_w + cc >= dp_area ) {
	    g_error ("internal memory allocation or reference error");
	  }
	  (*dp)[cr * *dp_w + cc] = pv;
	}
      }
      int return_code = close (data_fd);
      g_assert (return_code != -1);
      my_g_string_free (data_name);
      g_assert (row_buffer != NULL);
      g_free (row_buffer);
      *actual_zoom = 1.0 / pixel_stride;
      *unowned_memory = TRUE;
    }
  }

  return;
}

// Use the Space2d class to translate between big picture and OpenGL
// orthographic view coordinates (implicit in this is an understanding
// of the OpenGL projection in use -- see elsewhere).
static void
coord_translate_big_picture_to_opengl (ViewerWindow *self,
				       gdouble *x, gdouble *y)
{
  GtkWidget *da = self->da;

  Space2D *space = space_2d_new ();

  // Register the big picture coordinate system.
  CoordinateSystem2D *big_picture
    = coordinate_system_2d_new (0.0, 0.0, 1.0, -1.0, 0.0, 0.0,
				self->max_x, self->max_y);
  space_2d_add_system (space, NULL, big_picture);

  // Register the virtual window coordinate system (which covers the
  // entire big picture coordinate system, but at reduced resolution).
  CoordinateSystem2D *virtual_window
    = coordinate_system_2d_new (0.0, 0.0, 1.0 / self->zoom, 1.0 / self->zoom,
				0.0, 0.0, self->max_x * self->zoom,
				self->max_y * self->zoom);
  space_2d_add_system (space, big_picture, virtual_window);

  // Register the drawing area coordinates as Gdk/Gtk knows them.
  CoordinateSystem2D *screen
    = coordinate_system_2d_new (self->hadj->value, self->vadj->value, 1.0, 1.0,
				0.0, 0.0,
				da->allocation.width,
				da->allocation.height);
  space_2d_add_system (space, virtual_window, screen);

  CoordinateSystem2D *opengl_orthographic
    = coordinate_system_2d_new (0.0, da->allocation.height, 1.0, -1.0,
				0.0, 0.0,
				da->allocation.width, da->allocation.height);
  space_2d_add_system (space, screen, opengl_orthographic);

  space_2d_translate (space, big_picture, opengl_orthographic, x, y);

  space_2d_unref (space);
}

// Draw on the current gldrawable using the current glcontext in the
// current viewport and projection (i.e. gdk_gl_drawable_gl_begin must
// be in effect and the appropriate viewport and transformation
// matricies set).
static void
draw (ViewerWindow *viewer_window)
{
  ViewerWindow *self = viewer_window; // Convenience alias.
  
  glClear (GL_COLOR_BUFFER_BIT);
  
  gint win_w = self->da->allocation.width;
  gint win_h = self->da->allocation.height;

  if ( self->dp_to_free != NULL ) {
    g_free (self->dp_to_free);
    self->dp_to_free = NULL;
  }

  // Pointer to data to draw for current frame.
  float *dp = NULL;

  // Position of upper left corner of data pointed to by dp relative
  // to base image.
  size_t dp_start_x, dp_start_y;

  // Width and height of memory region pointed to by dp.
  size_t dp_w, dp_h;

  // dp pixels per image pixel.
  gfloat dp_zoom;

  // Upper left pixel image coordinates to fetch.
  size_t ulp_x = GSL_MAX (0, (self->hadj->value / self->zoom 
			      - self->image->x_offset));
  size_t ulp_y = GSL_MAX (0, (self->vadj->value / self->zoom
			      - self->image->y_offset));

  gboolean must_free_dp;

  // If we have anything to draw (once we get trimming set up we
  // should always have).
  if ( !(ulp_x >= self->image->size_x || ulp_y >= self->image->size_y) ) {

    get_data_pointer (self->image, ulp_x, ulp_y, self->zoom,
		      win_w, win_h, &dp, &dp_start_x, &dp_start_y,
		      &dp_w, &dp_h, &dp_zoom, &must_free_dp);

    // If we don't own the memory, we will need to free it before the
    // next redraw, so save a pointer to it.
    if ( must_free_dp ) {
      self->dp_to_free = dp;
    }
    else {
      self->dp_to_free = NULL;
    }

    // We may have to let OpenGL do a bit more zooming out for us,
    // since the pixels in our data region dp may still be higher
    // resolution than we need.
    GLfloat residual_zoom = self->zoom / dp_zoom;
  
    g_assert (sizeof (int32_t) == sizeof (GLint));
    g_assert (dp_w <= INT32_MAX);
    glPixelStorei (GL_UNPACK_ROW_LENGTH, dp_w); 

    // Now we need to determine the actual region of interest in the
    // returned data.  Here we determine the offsets in get_data
    // coordinates of the point we want to put in the top left corner
    // of the window.  // FIIXME: obviously the exact positioning of
    // the raster image on screen is questionable after all the
    // shenanegans we go through.  All that is really gauranteed is
    // that the cursor cross hair rendering is right for a zoom factor
    // of 1.0 on the OpenGL implementation I developed on (though I
    // think it should be ok for other sane implementations as well).
    // It would be lovely to fix this for other zoom factors (i.e. so
    // when the cursor steps visibly off the edge of the image, the
    // info command reports it as off), but I think this is actually
    // pretty hard: it would need an special single interface through
    // which all positioning command and queries could go, plus a
    // detailed knowledge of what OpenGL actually specifies.  I'm not
    // sure if it is even possible to use OpenGL commands to, say,
    // draw the lines for the cursor and have things work out.  One
    // might have to set pixel values in the image itself.  This way
    // you only have to know precisly where the image is rendered,
    // instead of precicely where the image is rendered, plus
    // precisely where the drawing commands render, plus gaurantee
    // that the two correspond perfectly.  Another possibility is to
    // find a library like cairo and/or glitz that has already
    // addressed these formidible issues, and use it.
    GLint roi_offset_x
      = GSL_MAX ((round (self->hadj->value / self->zoom) - dp_start_x
		  - self->image->x_offset) * dp_zoom, 0.0);
    g_assert (roi_offset_x >= 0);
    GLint roi_offset_y
      = GSL_MAX ((round (self->vadj->value / self->zoom) - dp_start_y
		  - self->image->y_offset) * dp_zoom, 0.0);
    g_assert (roi_offset_y >= 0);

    glPixelStorei (GL_UNPACK_SKIP_ROWS, roi_offset_y);
    glPixelStorei (GL_UNPACK_SKIP_PIXELS, roi_offset_x);

    // Here we do a bit of defensive programming to avoid problems
    // with floating point inexactness.  I'm not sure these assertions
    // will always pass, it may be necessary to explicitly clamp the
    // region of interest to be no wider than the remaining portion of
    // the data region retrieved.

    // One dimension of the data is probably smaller than the other,
    // so we have these GSL_MIN calls to just use all the available
    // data in a given direction if there isn't enough to fill the
    // window.
    GLint roi_w = GSL_MIN (dp_w - roi_offset_x,
			   ceil (win_w / residual_zoom));
    g_assert (roi_offset_x + roi_w <= dp_w);
    GLint roi_h = GSL_MIN (dp_h - roi_offset_y,
			   ceil (win_h / residual_zoom));
    g_assert (roi_offset_y + roi_h <= dp_h);
    
    glPixelZoom (residual_zoom, -residual_zoom);

    // Raster position to pass to glRasterPos.
    GLdouble x_raster_pos 
      = GSL_MAX (round (self->image->x_offset * self->zoom)
		 - self->hadj->value, 0);
    GLdouble y_raster_pos
      = (win_h - GSL_MAX (round (self->image->y_offset * self->zoom)
			  - self->vadj->value, 0));
    // Sometimes floating point inexactness in the above expression
    // may lead cause the entire image to get clipped (since OpenGL
    // weirdly insists on clipping the entire image if the raster
    // point is clipped).  Instead of going to the trouble of texture
    // mapping the image onto a polygon (OpenGL doesn't clip the
    // entire polygon), we just have a small check to ensure that we
    // haven't slipped below zero.  But if we have slipped by very
    // much, we need to rethink our algorithm.
    GLdouble allowable_slop = 0.001;
    if ( y_raster_pos > win_h ) {
      g_assert (y_raster_pos < win_h + allowable_slop);
      y_raster_pos = win_h;
    }

    // We would like the raster position to be exactly at the top of
    // the window.  But some sort of floating point comparison
    // problems seem to occur even when we ensure that this is the
    // case with the above code, and then use a glRasterPos2i call.
    // The only thing that seems to work is to use glRasterPos2f and
    // then nudge the value down just slightly.  This gets us an image
    // placed at the very top pixel of the window (no black line at
    // the top) and doesn't seem to get the entire image clipped.  But
    // I wouldn't be shocked to see other OpenGL implementation behave
    // differently, possibly clipping the image out of existence.
    GLdouble downward_nudge = 0.0001;
    GLdouble rightward_nudge = downward_nudge;
    glRasterPos2d (x_raster_pos + rightward_nudge,
		   y_raster_pos - downward_nudge);

    // The range for which we will perform linear mapping.
    gdouble linear_bottom, linear_top;
    ViewerImage *ci = self->image;
    if ( ci->sigmas <= 0.0 ) {
      linear_bottom = ci->min;
      linear_top = ci->max;
    }
    else {
      linear_bottom = ci->mean - ci->sigmas * ci->sdev;
      linear_top = ci->mean + ci->sigmas * ci->sdev;
    }

    float bias = linear_bottom;
    float scale = linear_top - linear_bottom;

    // FIIXME: should bias be negated here?  It works this way but seems
    // strange.
    glPixelTransferf (GL_RED_BIAS, bias / scale);
    glPixelTransferf (GL_GREEN_BIAS, bias / scale);
    glPixelTransferf (GL_BLUE_BIAS, bias / scale);
    glPixelTransferf (GL_RED_SCALE, 1.0 / scale);
    glPixelTransferf (GL_GREEN_SCALE, 1.0 / scale);
    glPixelTransferf (GL_BLUE_SCALE, 1.0 / scale);

    // OpenGL will clamp the results of the above scale and bias
    // operations into the range [0, 1] for us, so at this point we
    // have effectively implemented the advertised sigma clamping.

    glDrawPixels (roi_w, roi_h, GL_LUMINANCE, GL_FLOAT, dp);
  }

  // If the cursor has been placed, draw it (it may get clipped, which
  // is fine).
  if ( self->cursor_x != -1 ) {
    g_assert (self->cursor_y != -1);
    const int cursor_size = 19;   // Cursor size, in pixels.
    g_assert (cursor_size % 2 == 1);
    gdouble x_ogl = self->cursor_x, y_ogl = self->cursor_y;
    coord_translate_big_picture_to_opengl (self, &x_ogl, &y_ogl);
    
    // Deal with translation inexactness.
    x_ogl = round (x_ogl);
    y_ogl = round (y_ogl);

    // The same trick we have to pull with the raster position for the
    // image itself to keep the image from sometimes getting clipped,
    // we have to pull here.  I'm not exactly sure why.
            GLdouble downward_nudge = 0.1;
            y_ogl -= downward_nudge;
    // Looks like we also have to pull this trick in the x direction.
    //        GLdouble rightward_nudge = downward_nudge;
    //x_ogl += rightward_nudge;
    

    glColor3d (1.0, 0.0, 0.0);   // A pretty red cursor.

    glBegin (GL_LINES);
    {
      glVertex2d (x_ogl - cursor_size / 2, y_ogl);
      glVertex2d (x_ogl + cursor_size / 2, y_ogl);
    }
    glEnd ();
    glBegin (GL_LINES);
    {
      glVertex2d (x_ogl, y_ogl - cursor_size / 2);
      glVertex2d (x_ogl, y_ogl + cursor_size / 2);
    }
    glEnd ();
    // Sigh... OpenGL is pixel-inexact.  The implementation I'm on
    // seems to omit the pixel at the end of the line segment, others
    // might do it differently.  So we explicitly add the end points
    // as dots.  IMPROVEME: we're assuming dot size of 1 pixel here,
    // this is a piece of OpenGL state that should be queried and
    // asserted.
    glBegin (GL_POINTS);
    {
      glVertex2d (x_ogl - cursor_size / 2, y_ogl);
      glVertex2d (x_ogl + cursor_size / 2, y_ogl);
      glVertex2d (x_ogl, y_ogl - cursor_size / 2);
      glVertex2d (x_ogl, y_ogl + cursor_size / 2);
    }
    glEnd ();
  }

  // This code draws a little red X near the lower left corner of the
  // window (maybe useful for sorting out flipping and clipping
  // issues).
  /*
    glColor3d (1.0, 0.0, 0.0);
    glBegin (GL_LINES);
    glVertex2d (20, 20);
    glVertex2d (80, 80);
    glEnd ();
    glBegin (GL_LINES);
    glVertex2d (20, 80);
    glVertex2d (80, 20);
    glEnd ();
  */
  
  trap_opengl_errors ();
}

// This is currently a no-op handler.  Its difficult to say exactly
// when we want to prevent the user from making the window large (they
// may be planning to zoom in as the next thing they do, so that
// displayed space that is outside the big picture coordinate system
// will then immediately get used).  And making the window big doesn't
// happen by accident or cause a lot of confusion, so we don't worry
// about it.
static void
reset_window_max_dimensions (ViewerWindow *self)
{
  // GdkGeometry hints;
  // hints.max_width = self->max_x - self->hadj->value / self->zoom;
  // hints.max_height = self->max_y - self->vadj->value / self->zoom;
  // gtk_window_set_geometry_hints (GTK_WINDOW (self->w), self->da, &hints,
  // 				    GDK_HINT_MAX_SIZE);
}

// Fetch the GdkGLContext and GdkGLDrawable associated with
// drawing_area, call the draw routing to perform OpenGL rendering
// according to the current state of self, and swap or flush buffer
// (depending on double buffered status of the drawable).
static void
redraw_drawing_area (ViewerWindow *self, GtkWidget *drawing_area)
{
  GdkGLContext *glcontext = gtk_widget_get_gl_context (drawing_area);
  GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (drawing_area);

  gboolean return_code = gdk_gl_drawable_gl_begin (gldrawable, glcontext);
  g_assert (return_code);
  {
    draw (self);

    if ( gdk_gl_drawable_is_double_buffered (gldrawable) ) {
      gdk_gl_drawable_swap_buffers (gldrawable);
    }
    else {
      // This code path is simple, but untested.
      g_assert_not_reached ();
      glFlush ();
    }
  }
  gdk_gl_drawable_gl_end (gldrawable);
}

// Recompute the maximum window dimensions given self->zoom and the
// current adjustment positions, and hint gtk appropriately, then
// redraw the drawing area.
static void
on_adjustment_changed (GtkAdjustment *adj, ViewerWindow *self)
{
  reset_window_max_dimensions (self);

  redraw_drawing_area (self, self->da);

  // FIIXME: I assume since the signature for this signal in the
  // GtkAdjustment docs says this handler returns void, that either
  // the default handler always runs and updates the rendering of the
  // sliders themselves.  Confirm this.
}

static void
after_realize (GtkWidget *widget, gpointer data)
{
  ViewerWindow *self = data;

  // It seems that only after we realize the drawing window do we
  // really finally find out how big it really is.
  GtkAdjustment *va = self->vadj, *ha = self->hadj;
  va->page_size = self->da->allocation.height;
  va->upper = ceil (GSL_MAX (self->max_y * self->zoom, va->page_size));
  ha->page_size = self->da->allocation.width;
  ha->upper = ceil (GSL_MAX (self->max_x * self->zoom, ha->page_size));

  // In case Gtk needs to know to redraw its scrollbars (maybe the
  // scrollbars get drawn before the drawing area is realized, I'm not
  // sure).  We disable our handler since the drawing area will get
  // redrawn anyway.
  g_signal_handlers_block_by_func (va, on_adjustment_changed, self);
  g_signal_handlers_block_by_func (ha, on_adjustment_changed, self);
  g_signal_emit_by_name (va, "changed::");
  g_signal_emit_by_name (ha, "changed::");
  g_signal_handlers_unblock_by_func (ha, on_adjustment_changed, self);
  g_signal_handlers_unblock_by_func (va, on_adjustment_changed, self);

  GdkGLContext *glcontext = gtk_widget_get_gl_context (widget);
  GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (widget);

  gboolean return_code = gdk_gl_drawable_gl_begin (gldrawable, glcontext);
  g_assert (return_code);
  {
    draw (self);
  }
  gdk_gl_drawable_gl_end (gldrawable);
}

typedef struct {
  GtkAdjustment *vertical;
  GtkAdjustment *horizontal;
} marshaled_adjustments;

static gboolean
on_drawing_area_configure_event (GtkWidget *widget, GdkEventConfigure *event,
				 gpointer data)
{
  GdkGLContext *glcontext = gtk_widget_get_gl_context (widget);
  GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (widget);

  gint w = widget->allocation.width, h = widget->allocation.height;

  gboolean return_code = gdk_gl_drawable_gl_begin (gldrawable, glcontext);
  g_assert (return_code);
  {
    glViewport (0, 0, w, h);
    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    gluOrtho2D (0.0, (GLfloat) w, 0.0, (GLfloat) h);
    glMatrixMode (GL_MODELVIEW);
    glLoadIdentity ();
  }
  gdk_gl_drawable_gl_end (gldrawable);

  ViewerWindow *self = data;

  // Convenience aliases.
  GtkAdjustment *ha = self->hadj, *va = self->vadj;

  // We want the increments to be whole numbers, hence floor().
  ha->step_increment = floor (w / 4.0);
  ha->page_increment = floor (w / 2.0);
  ha->page_size = w;
  va->step_increment = floor (h / 4.0);
  va->page_increment = floor (h / 2.0);
  va->page_size = h;

  // If the window is now bigger the the portion of the big picture
  // domain with upper left corner at the same position it was before
  // the resize, as rendered at this zoom level, go ahead and zoom in
  // (leaving the upper left corner in the same position in big
  // picture coordinates).

  // Bottom right of big picture in current virtual window
  // coordinates.  IMPROVEME: this way of handling resizing
  // unfortunately pulls us off power-of-two zoom levels, which
  // creates aliasing.  When power-of-two zoom level locking is
  // implemented, probably what will need to happen is to make the big
  // picture domain bigger by a precomputed amount so that we only
  // need to zoom on resize if we can do it as a power of two.

  // Bottom right of big picture coordinate domain in viewer window
  // coordinates.
  double brvw_x = self->max_x * self->zoom, brvw_y = self->max_y * self->zoom;

  gfloat old_zoom = self->zoom;

  if ( brvw_x - ha->value < w && brvw_y - va->value < h ) {
    if ( (brvw_x - ha->value) / w > (brvw_y - va->value) / h ) {
      self->zoom = (w + ha->value) / self->max_x;
    }
    else {
      self->zoom = (w + va->value) / self->max_y;
    }

    // Since we don't zooming to the point of pixelation at the
    // moment, floating inexactness in the above code can trip us up
    // elsewhere.  It would also be nice to clamp to power of two or
    // using antialiasing convolution filtering in OpenGL, as
    // mentioned elsewhere.
    if ( self->zoom > 1.0 ) {
      gdouble max_slop = 0.01;
      // If this isn't a small correction, we probably have a genuine
      // problem somewhere.
      if ( self->zoom - 1.0 > max_slop ) {
	g_error ("floating point slop too large: self->zoom - 1.0 = %lf\n",
		 self->zoom - 1.0);
      }
      self->zoom = 1.0; 
    }

    ha->value = round (ha->value * self->zoom / old_zoom);
    va->value = round (va->value * self->zoom / old_zoom);

    ha->upper = ceil (GSL_MAX (self->max_x * self->zoom, ha->page_size));
    va->upper = ceil (GSL_MAX (self->max_x * self->zoom, va->page_size));
  }

  // FIIXME: confirm: I don't think we need or want to emit any changed
  // signals for the adjustments here, since if we get a configure
  // event we should also get an expose event or the like which should
  // redraw the scroll bars and image area correctly anyway.  It seems
  // to work.
  //  g_signal_emit_by_name (self->hadj, "changed::");
  //  g_signal_emit_by_name (self->vadj, "changed::");

  return TRUE;
}

static gboolean
on_expose_event (GtkWidget *widget, GdkEventExpose *event, ViewerWindow *self)
{
  redraw_drawing_area (self, widget);

  return TRUE;
}

static gboolean
on_button_press_event (GtkWidget *widget, GdkEventButton *event,
		       ViewerWindow *self)
{
  // Here I test out the mental economy of my Space2D class.  Indulge
  // me.  It would of course be more economical to keep the coordinate
  // systems around more permanently somewhere else.
  Space2D *space = space_2d_new ();

  // Register the big picture coordinate system.
  CoordinateSystem2D *big_picture
    = coordinate_system_2d_new (0.0, 0.0, 1.0, -1.0, 0.0, 0.0,
				self->max_x, self->max_y);
  space_2d_add_system (space, NULL, big_picture);

  // Register the virtual window coordinate system (which covers the
  // entire big picture coordinate system, but at reduced resolution).
  CoordinateSystem2D *virtual_window
    = coordinate_system_2d_new (0.0, 0.0, 1.0 / self->zoom, 1.0 / self->zoom,
				0.0, 0.0, self->max_x * self->zoom,
				self->max_y * self->zoom);
  space_2d_add_system (space, big_picture, virtual_window);

  // Register the drawing area coordinates as Gdk/Gtk knows them.
  CoordinateSystem2D *screen
    = coordinate_system_2d_new (self->hadj->value, self->vadj->value, 1.0, 1.0,
				0.0, 0.0,
				widget->allocation.width,
				widget->allocation.height);
  space_2d_add_system (space, virtual_window, screen);

  // Register the current image under the big picture system.
  ViewerImage *ci = self->image;   // Convenience alias.
  CoordinateSystem2D *current_image
    = coordinate_system_2d_new (ci->x_offset, ci->y_offset, 1.0, 1.0,
				0.0, 0.0, ci->size_x - 1, ci->size_y - 1);
  space_2d_add_system (space, big_picture, current_image);

  // Now we should be able to effortlessly translate the window
  // coordinates given to us in the GdkEventButton structure into
  // coordinates in (or not in, as the case may be) the current image.
  gdouble xc = event->x, yc = event->y;
  space_2d_translate (space, screen, big_picture, &xc, &yc);

  space_2d_unref (space);

  if ( event->button == 1 ) {
    self->cursor_x = xc;
    self->cursor_y = yc;
  }

  redraw_drawing_area (self, self->da);

  return FALSE;  // Return FALSE tell Gtk to go ahead and propagate event.
}

// Convert a GDK state, keyval pair to a single representative
// character.
static gchar
find_key_op (guint state, guint keyval)
{
  if ( keyval == GDK_minus ) {
    return '-';
  }
  if ( state | GDK_SHIFT_MASK && keyval == GDK_plus ) {
    return '+';
  }
  if ( keyval == GDK_N ) {
    return 'N';
  }
  if ( keyval == GDK_n ) {
    return 'n';
  }
  if ( keyval == GDK_P ) {
    return 'P';
  }
  if ( keyval == GDK_p ) {
    return 'p';
  }
  if ( keyval == GDK_I ) {
    return 'I';
  }
  if ( keyval == GDK_i ) {
    return 'i';
  }
  if ( keyval == GDK_a ) {
    return 'a';
  }
  if ( keyval == GDK_A ) {
    return 'a';
  }
  // When Escape is pressed this routing returns... 'e'.  Pretty silly
  // but oh well, we didn't envision wanting Esc and our caller knows
  // what 'e' means.
  if ( keyval == GDK_Escape ) {
    return 'e';
  }

  // We use this magic to mean "otherwise unrecognized key press".
  return '~';
}

// Print to standard output some information about the pixel at the
// current cursor position.
static void
print_point_info_for_cursor (ViewerWindow *self)
{
  // Convert cursor coordinates to current image coordinates.
  size_t img_x = self->cursor_x - self->image->x_offset;
  size_t img_y = self->cursor_y - self->image->y_offset;

  g_print ("\n");
  if ( self->cursor_x == -1 ) {
    g_assert (self->cursor_y == -1);   // By definition.
    g_print ("Pixel info requested, but the cursor hasn't been placed yet.\n"
	     "Left click on the image to place it.\n");

  }
  else if ( img_x >= 0 && img_x < self->image->size_x
       && img_y >= 0 && img_y < self->image->size_y ) {
    // Look up the value of the current pixel in the data file.
    GString *data_file = g_string_new (self->image->base_name->str);
    g_string_append (data_file, ".img");
    // IMPROVEME: it would be nice to through in a check to make sure
    // the file size is what we expect, to help catch file changes
    // while the viewer is running.
    FILE *df = fopen (data_file->str, "r");
    g_assert (df != NULL);
    off_t offset = (img_y * self->image->size_x + img_x) * sizeof (float);
    int return_code = fseeko (df, offset, SEEK_SET);
    g_assert (return_code == 0);
    float pixel_value;
    size_t read_count = fread (&pixel_value, sizeof(float), 1, df);
    g_assert (read_count == 1);
    return_code = fclose (df);
    g_assert (return_code == 0);
    if ( G_BYTE_ORDER == G_LITTLE_ENDIAN ) {
      swap_bytes_32 ((unsigned char *) &pixel_value);
    }
    g_print ("Pixel information:\n");
    g_print ("------------------------------------------------------------\n");
    g_print ("File base name: %s\n", self->image->base_name->str);
    g_print ("Image pixel (row, column): (%d, %d)\n", img_x, img_y);
    g_print ("Pixel value: %f\n", pixel_value);
  }
  else {
    g_print ("Current cursor position is outside current image.  Left click\n"
	     "on the image to reposition the cursor inside the image.\n");
  }
  g_print ("\n");
}

// Run the analysis program on the tile surrounding the current cursor
// position using the analysis-program related data members.
static void
run_analysis (ViewerWindow *self)
{
  g_assert (self->cursor_x != -1);
  g_assert (self->cursor_y != -1);
  g_assert (self->analysis_program != NULL);

  GString *command_line = g_string_new (self->analysis_program->str);

  g_string_append_c (command_line, ' ');
  
  g_string_append_printf (command_line, "%u ", self->images->len);

  // We will end up creating a bunch of temporary files that we'll
  // need to remove.
  GPtrArray *tmp_files = g_ptr_array_new ();

  guint ii;
  for ( ii = 0 ; ii < self->images->len ; ii++ ) {

    ViewerImage *ci = g_ptr_array_index (self->images, ii);   // Current image.

    g_string_append_printf (command_line, "%s ", ci->base_name->str);

    ssize_t image_x = self->cursor_x - ci->x_offset;
    ssize_t image_y = self->cursor_y - ci->y_offset;

    // Start x and start y of tile in image.
    size_t start_x = image_x - self->analysis_tile_size / 2;
    size_t start_y = image_y - self->analysis_tile_size / 2;

    // Tile width and height.
    ssize_t tw = self->analysis_tile_size; 
    ssize_t th = self->analysis_tile_size;

    // Adjust dimensions and/or starting point if near image edges.
    if ( image_x - self->analysis_tile_size / 2 < 0 ) {
      start_x = 0;
      tw += image_x - self->analysis_tile_size / 2;
    }
    if ( image_x + self->analysis_tile_size / 2 >= ci->size_x ) {
      size_t overhang
	= image_x + self->analysis_tile_size / 2 - ci->size_x + 1;
      //      start_x -= overhang;
      tw -= overhang;
    }
    if ( tw < 0 ) {
      tw = 0;
    }
    if ( image_y - self->analysis_tile_size / 2 < 0 ) {
      start_y = 0;
      th += image_y - self->analysis_tile_size / 2;
    }
    if ( image_y + self->analysis_tile_size / 2 >= ci->size_y ) {
      size_t overhang
	= image_y + self->analysis_tile_size / 2 - ci->size_y + 1;
      //      start_y -= overhang;
      th -= overhang;
    }
    if ( th < 0 ) {
      th = 0;
    }

    g_string_append_printf (command_line, "%lld %lld ",
			    (long long int) tw, (long long int) th);

    // Read the actual tile data.
    GString *data_name = g_string_new (ci->base_name->str);
    g_string_append (data_name, ".img");
    float *tile_data = my_read_data_rectangle (data_name->str, sizeof (float),
					       ci->size_x, start_x, start_y,
					       tw, th);
    swap_array_bytes_32 (tile_data, tw * th);
    
    // Store tile data in temporary file for use by analysis program.
    // For uniqueness :)
    GString *tile_tmp_file_name
      = make_unique_tmp_file_name ("/tmp", "blagLEweird");
    g_ptr_array_add (tmp_files, tile_tmp_file_name);
    if ( tw > 0 && th > 0 ) {
      g_assert (tw <= SSIZE_MAX);
      g_assert (th <= SSIZE_MAX);
      FloatImage *tile_fi = float_image_new_from_memory (tw, th, tile_data);
      // FIIXME: verify: works for zero width or height tiles?
      int return_code = float_image_store (tile_fi, tile_tmp_file_name->str,
					   FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
      g_assert (return_code == 0);
    }
    else {
      // We have a tile file of size zero, which the float_image
      // interface doesn't contend with, so we just use touch to
      // create an empty file.
      GString *touch_command = g_string_new ("touch ");
      g_string_append (touch_command, tile_tmp_file_name->str);
      int exit_code = system (touch_command->str);
      g_assert (exit_code == 0);
      g_string_free (touch_command, TRUE);
    }
    g_string_append_printf (command_line, "%s ", tile_tmp_file_name->str);
  }

  if ( self->async_analysis ) {
    // IMPROVEME: in an ideal world we might document somehow for the
    // user the fact that when analysis commands are run
    // asynchronously, the temporary tile files that are created need
    // to be removed by the analysis program itself.
    GError *err = NULL;
    gboolean exit_code = g_spawn_command_line_async (command_line->str, &err);
    if ( ! exit_code ) {
      g_printerr ("\nasynchronous analysis command execution failed: %s\n",
		  err->message);
    }
  }
  else {
    // FIIXME: confirm: I'm guess glib allocates these for us.
    gchar *standard_output = NULL, *standard_error = NULL;
    gint exit_status;
    GError *err = NULL;
    gboolean exit_code
      = g_spawn_command_line_sync (command_line->str, &standard_output,
				   &standard_error, &exit_status, &err);
    if ( standard_output != NULL ) {
      g_print ("%s", standard_output);
    }
    if ( standard_error != NULL ) {
      g_printerr ("%s", standard_error);
    }
    if ( ! exit_code ) {
      g_printerr ("\nanalysis command execution failed: %s\n", err->message);
    }
  }
}

// Zoom in one step (power of two).  We have to do a lot of special
// stuff at minimum or maximum zoom, around image edges, etc.
static void
step_zoom_in (ViewerWindow *self)
{
  // At most one screen pixel per image pixel.
  const gfloat max_zoom = 1.0; 

  // Remember old values so we can adjust the adjustments correctly.
  GtkAdjustment *va = self->vadj, *ha = self->hadj;
  gdouble v_old_upper = va->upper, h_old_upper = ha->upper;
  gdouble v_old_middle = va->value + va->page_size / 2.0;
  gdouble h_old_middle = ha->value + ha->page_size / 2.0;
  gfloat old_zoom = self->zoom;

  // Zoom, but only up to the maximum.  IMPROVEME: it would be best to
  // only zoom by factors of 2.0, even when the window is resized.
  // The pyramids are of these powers naturally, so no additional
  // OpenGL scaling needs to be done.  The additional scaling probably
  // introduces aliasing, which is kind of a shame since the whole
  // point of building the pyramids is to produce good looking results
  // at different resolutions.  For now, we just clamp to powers ot
  // two here in the zoom in/out code, not in the code that handles
  // window resizing.  This is somewhat reasonable, since doing this
  // gives the user our best available rendering when they try to look
  // at something in detail, which is when it counts.  It isn't really
  // the ideal arrangement though.  Another potential option is to use
  // an antialiasing OpenGL convolution filter.  But I'm not sure
  // exactly how to do that, or even what the tradeoffs are exactly.
  self->zoom *= 2.0;
  // Clamp to a power of 2.0.
  gdouble power = round (log2 (self->zoom));
  self->zoom = pow (2.0, power);
  if ( self->zoom > max_zoom ) {
    self->zoom = max_zoom;
  }
  
  // Compute the new upper value and current value of the adjustments,
  // given the new virtual window size.
  gfloat zoom_ratio = self->zoom / old_zoom;

  // Update the vertical adjustment.
  
  va->upper = ceil (va->upper * zoom_ratio);
  
  gfloat v_new_middle = v_old_middle * va->upper / v_old_upper;
  
  // When we zoom, we may end up with a different part of the image in
  // the center, since we try to avoid showing areas not in the big
  // picture coordinate system, and configure events may have changed
  // the window size.
  if ( v_new_middle < va->page_size / 2.0 ) {
    v_new_middle = va->page_size / 2.0;
  }
  if ( v_new_middle > va->upper - va->page_size / 2.0 ) {
    v_new_middle = GSL_MAX (va->upper - va->page_size / 2.0,
			    va->page_size / 2.0);
  }
  va->value = round (v_new_middle - va->page_size / 2.0);
  // Floating point math might put us off by a tiny bit, so we check
  // for this and clamp if needed.
  const gfloat max_slop = 0.4;      
  if ( va->value < 0.0) {
    // We should only be dealing with slight floating point
    // inexactness here.
    g_assert (va->value >= -0.4);
    va->value = 0.0;
  }
  if ( va->value > va->upper - va->page_size ) {
    g_assert (va->value - (va->upper - va->page_size) <= max_slop);
    va->value = va->upper - va->page_size;
  }
  
  // Update the horizontal adjustment.
  
  ha->upper = ceil (ha->upper * zoom_ratio);
  
  gfloat h_new_middle = h_old_middle * ha->upper / h_old_upper;
  
  // When we zoom, we may end up with a different part of the image in
  // the center, since we try to avoid showing areas not in the big
  // picture coordinate system, and configure events may have changed
  // the window size.
  if ( h_new_middle < ha->page_size / 2.0 ) {
    h_new_middle = ha->page_size / 2.0;
  }
  if ( h_new_middle > ha->upper - ha->page_size / 2.0 ) {
    h_new_middle = GSL_MAX (ha->upper - ha->page_size / 2.0,
			    ha->page_size / 2.0);
  }
  ha->value = round (h_new_middle - ha->page_size / 2.0);
  if ( ha->value < 0.0 ) {
    // We should only be dealing with slight floating point
    // inexactness here.
    g_assert (ha->value >= -max_slop);
    ha->value = 0.0;
  }
  if ( ha->value > ha->upper - ha->page_size ) {
    gdouble slop = ha->value - (ha->upper - ha->page_size);
    // FIIXME: I swear I got this condition to trip once at this slop
    // threshold.  So it might be necessary to change this into a
    // warning, or loosen the tolerance a bit.
    if ( slop > max_slop ) {
      g_error ("too much slop (%lf) in floating point calculation in "
	       "file " __FILE__ " function %s, line %d", slop, __func__,
	       __LINE__);
    }
    ha->value = round (ha->upper - ha->page_size);
  }

  // We unblock our own handler for three of the next four
  // adjustment-related emissions, since all it does it recompute
  // and rehint the max window dimensions and redraw the drawing
  // area.  Doing this once is enough.  But we do want gtk to have
  // a chance to run any code it needs to run to redraw the
  // scrollbars and such.
  gint block_count
    = g_signal_handlers_block_by_func (va, on_adjustment_changed, self);
  g_assert (block_count == 2);
  block_count
    = g_signal_handlers_block_by_func (ha, on_adjustment_changed, self);
  g_assert (block_count == 2);
  {
    g_signal_emit_by_name (va, "changed::");
    g_signal_emit_by_name (ha, "changed::");
    // IMPROVEME: Do we need both changed and value-changed
    // emmisions?  Gtk docs seem to suggest we do as of this
    // writing, but this seems like a pretty silly requirement.
    g_signal_emit_by_name (va, "value-changed::");
  }
  gint unblock_count
    = g_signal_handlers_unblock_by_func (ha, on_adjustment_changed, self);
  g_assert (unblock_count == 2);
  unblock_count
    = g_signal_handlers_unblock_by_func (va, on_adjustment_changed, self);
  g_assert (unblock_count == 2);
  g_signal_emit_by_name (ha, "value-changed::");
}

static void
step_zoom_out (ViewerWindow *self) {
  // Remember old values so we can adjust the adjustments correctly.
  GtkAdjustment *va = self->vadj, *ha = self->hadj;
  gdouble v_old_middle = va->value + va->page_size / 2.0;
  gdouble h_old_middle = ha->value + ha->page_size / 2.0;
  gfloat old_zoom = self->zoom;

  // The maximum zoom out we will allow is one that displays the
  // entire big picture coordinate systme in the current window.
  const gfloat min_zoom = GSL_MIN (ha->page_size / self->max_x,
				   va->page_size / self->max_y);

  // Zoom, but only out to the maximum.
  self->zoom /= 2.0;
  if ( self->zoom < min_zoom ) {
    self->zoom = min_zoom;
  }

  // Compute the new upper value and current value of the adjustments,
  // given the new virtual window size.
  gfloat zoom_ratio = self->zoom / old_zoom;

  // Update the vertical adjustment.

  va->upper = ceil (GSL_MAX (self->max_y * self->zoom, va->page_size));

  gfloat v_new_middle = v_old_middle * zoom_ratio;
  // When we zoom out, we may end up with a different part of the
  // image in the center, since we try to avoid showing areas not in
  // the big picture coordinate system.
  if ( v_new_middle < va->page_size / 2.0 ) {
    v_new_middle = va->page_size / 2.0;
  }
  if ( v_new_middle > va->upper - va->page_size / 2.0 ) {
    v_new_middle = GSL_MAX (va->upper - va->page_size / 2.0,
			    va->page_size / 2.0);
  }

  va->value = round (v_new_middle - va->page_size / 2.0);
  // Floating point math might put us off by a tiny bit, so we check
  // for this and clamp if needed.
  const gfloat max_slop = 0.4;      
  if ( va->value < 0.0 ) {
    // We should only be dealing with slight floating point
    // inexactness here.
    if ( va->value < -max_slop ) {
      g_error ("Current vertical page position too negative: %lf\n",
	       va->value);
    }
    g_assert (va->value >= -max_slop);
    va->value = 0.0;
  }
  if ( va->value > va->upper - va->page_size ) {
    g_assert (va->value - (va->upper - va->page_size) <= max_slop);
    va->value = va->upper - va->page_size;
  }

  // Update the horizontal adjustment.

  ha->upper = ceil (GSL_MAX (self->max_x * self->zoom, ha->page_size));

  gfloat h_new_middle = h_old_middle * zoom_ratio;
  if ( h_new_middle < ha->page_size / 2.0 ) {
    h_new_middle = ha->page_size / 2.0;
  }
  if ( h_new_middle > ha->upper - ha->page_size / 2.0 ) {
    h_new_middle = GSL_MAX (ha->upper - ha->page_size / 2.0,
			    ha->page_size / 2.0);
  }

  ha->value = round (h_new_middle - ha->page_size / 2.0);
  if ( ha->value < 0.0 ) {
    // We should only be dealing with slight floating point
    // inexactness here.
    g_assert (ha->value > -max_slop);
    ha->value = 0.0;
  }
  if ( ha->value > ha->upper - ha->page_size ) {
    gdouble margin = ha->value - (ha->upper - ha->page_size);
    if ( margin > max_slop ) {
      g_error ("Current hosizontal page position too much too large: %lf",
	       margin);
    }
    g_assert (ha->value - (ha->upper - ha->page_size) <= max_slop);
    ha->value = ha->upper - ha->page_size;
  }
  
  // We unblock our own handler for three of the next four
  // adjustment-related emissions, since all it does it recompute and
  // rehint the max window dimensions and redraw the drawing area.
  // Doing this once is enough.  But we do want gtk to have a chance
  // to run any code it needs to run to redraw the scrollbars and
  // such.
  gint block_count
    = g_signal_handlers_block_by_func (va, on_adjustment_changed, self);
  g_assert (block_count == 2);
  block_count
    = g_signal_handlers_block_by_func (ha, on_adjustment_changed, self);
  g_assert (block_count == 2);
  {
    g_signal_emit_by_name (va, "changed::");
    g_signal_emit_by_name (ha, "changed::");
    // IMPROVEME: Do we need both changed and value-changed emmisions?
    // Gtk does seem to suggest yes as of this writing, but this seems
    // like a pretty silly requirement.
    g_signal_emit_by_name (va, "value-changed::");
  }
  gint unblock_count
    = g_signal_handlers_unblock_by_func (ha, on_adjustment_changed, self);
  g_assert (unblock_count == 2);
  unblock_count
    = g_signal_handlers_unblock_by_func (va, on_adjustment_changed, self);
  g_assert (unblock_count == 2);
  g_signal_emit_by_name (ha, "value-changed::");
}

static gboolean
on_key_press_event (GtkWidget *win, GdkEventKey *event, gpointer user_data)
{
  ViewerWindow *self = user_data;

  gchar key_op = find_key_op (event->state, event->keyval);

  switch ( key_op ) {
  case 'I':
  case 'i':
    print_point_info_for_cursor (self);
    break;
  case '+': 
    {
      // FIIXME: all this code should be replaced with a step_zoom_in call.

      // At most one screen pixel per image pixel.
      const gfloat max_zoom = 1.0; 

      // Remember old values so we can adjust the adjustments
      // correctly.
      GtkAdjustment *va = self->vadj, *ha = self->hadj;
      gdouble v_old_upper = va->upper, h_old_upper = ha->upper;
      gdouble v_old_middle = va->value + va->page_size / 2.0;
      gdouble h_old_middle = ha->value + ha->page_size / 2.0;
      gfloat old_zoom = self->zoom;

      // Zoom, but only up to the maximum.  IMPROVEME: it would be
      // best to only zoom by factors of 2.0, even when the window is
      // resized.  The pyramids are of these powers naturally, so no
      // additional OpenGL scaling needs to be done.  The additional
      // scaling probably introduces aliasing, which is kind of a
      // shame since the whole point of building the pyramids is to
      // produce good looking results at different resolutions.  For
      // now, we just clamp to powers ot two here in the zoom in/out
      // code, not in the code that handles window resizing.  This is
      // somewhat reasonable, since doing this gives the user our best
      // available rendering when they try to look at something in
      // detail, which is when it counts.  It isn't really the ideal
      // arrangement though.  Another potential option is to use an
      // antialiasing OpenGL convolution filter.  But I'm not sure
      // exactly how to do that, or even what the tradeoffs are
      // exactly.
      self->zoom *= 2.0;
      // Clamp to a power of 2.0.
      gdouble power = round (log2 (self->zoom));
      self->zoom = pow (2.0, power);
      if ( self->zoom > max_zoom ) {
	self->zoom = max_zoom;
      }

      // Compute the new upper value and current value of the
      // adjustments, given the new virtual window size.
      gfloat zoom_ratio = self->zoom / old_zoom;

      // Update the vertical adjustment.

      va->upper = ceil (va->upper * zoom_ratio);
      
      gfloat v_new_middle = v_old_middle * va->upper / v_old_upper;

      // When we zoom, we may end up with a different part of the
      // image in the center, since we try to avoid showing areas not
      // in the big picture coordinate system, and configure events
      // may have changed the window size.
      if ( v_new_middle < va->page_size / 2.0 ) {
	v_new_middle = va->page_size / 2.0;
      }
      if ( v_new_middle > va->upper - va->page_size / 2.0 ) {
	v_new_middle = GSL_MAX (va->upper - va->page_size / 2.0,
				va->page_size / 2.0);
      }
      va->value = round (v_new_middle - va->page_size / 2.0);
      // Floating point math might put us off by a tiny bit, so we
      // check for this and clamp if needed.
      const gfloat max_slop = 0.4;      
      if ( va->value < 0.0) {
	// We should only be dealing with slight floating point
	// inexactness here.
	g_assert (va->value >= -0.4);
	va->value = 0.0;
      }
      if ( va->value > va->upper - va->page_size ) {
	g_assert (va->value - (va->upper - va->page_size) <= max_slop);
	va->value = va->upper - va->page_size;
      }

      // Update the horizontal adjustment.

      ha->upper = ceil (ha->upper * zoom_ratio);

      gfloat h_new_middle = h_old_middle * ha->upper / h_old_upper;

      // When we zoom, we may end up with a different part of the
      // image in the center, since we try to avoid showing areas not
      // in the big picture coordinate system, and configure events
      // may have changed the window size.
      if ( h_new_middle < ha->page_size / 2.0 ) {
	h_new_middle = ha->page_size / 2.0;
      }
      if ( h_new_middle > ha->upper - ha->page_size / 2.0 ) {
	h_new_middle = GSL_MAX (ha->upper - ha->page_size / 2.0,
				ha->page_size / 2.0);
      }
      ha->value = round (h_new_middle - ha->page_size / 2.0);
      if ( ha->value < 0.0 ) {
	// We should only be dealing with slight floating point
	// inexactness here.
	g_assert (ha->value >= -max_slop);
	ha->value = 0.0;
      }
      if ( ha->value > ha->upper - ha->page_size ) {
	gdouble slop = ha->value - (ha->upper - ha->page_size);
	// FIIXME: I swear I got this condition to trip once at this
	// slop threshold.  So it might be necessary to change this
	// into a warning, or loosen the tolerance a bit.
	if ( slop > max_slop ) {
	  g_error ("too much slop (%lf) in floating point calculation in "
		   "file " __FILE__ " function %s, line %d", slop, __func__,
		   __LINE__);
	}
	ha->value = round (ha->upper - ha->page_size);
      }

      // We unblock our own handler for three of the next four
      // adjustment-related emissions, since all it does it recompute
      // and rehint the max window dimensions and redraw the drawing
      // area.  Doing this once is enough.  But we do want gtk to have
      // a chance to run any code it needs to run to redraw the
      // scrollbars and such.
      gint block_count
	= g_signal_handlers_block_by_func (va, on_adjustment_changed, self);
      g_assert (block_count == 2);
      block_count
	= g_signal_handlers_block_by_func (ha, on_adjustment_changed, self);
      g_assert (block_count == 2);
      {
	g_signal_emit_by_name (va, "changed::");
	g_signal_emit_by_name (ha, "changed::");
	// IMPROVEME: Do we need both changed and value-changed
	// emmisions?  Gtk docs seem to suggest we do as of this
	// writing, but this seems like a pretty silly requirement.
	g_signal_emit_by_name (va, "value-changed::");
      }
      gint unblock_count
	= g_signal_handlers_unblock_by_func (ha, on_adjustment_changed, self);
      g_assert (unblock_count == 2);
      unblock_count
	= g_signal_handlers_unblock_by_func (va, on_adjustment_changed, self);
      g_assert (unblock_count == 2);
      g_signal_emit_by_name (ha, "value-changed::");
      break;
    }
  case '-':
    {
      // FIIXME: all this code should be replaced with a step_zoom_out call.

      // Remember old values so we can adjust the adjustments
      // correctly.
      GtkAdjustment *va = self->vadj, *ha = self->hadj;
      gdouble v_old_middle = va->value + va->page_size / 2.0;
      gdouble h_old_middle = ha->value + ha->page_size / 2.0;
      gfloat old_zoom = self->zoom;

      // The maximum zoom out we will allow is one that displays the
      // entire big picture coordinate systme in the current window.
      const gfloat min_zoom = GSL_MIN (ha->page_size / self->max_x,
				       va->page_size / self->max_y);

      // Zoom, but only out to the maximum.
      self->zoom /= 2.0;
      if ( self->zoom < min_zoom ) {
	self->zoom = min_zoom;
      }

      // Compute the new upper value and current value of the
      // adjustments, given the new virtual window size.
      gfloat zoom_ratio = self->zoom / old_zoom;

      // Update the vertical adjustment.

      va->upper = ceil (GSL_MAX (self->max_y * self->zoom, va->page_size));

      gfloat v_new_middle = v_old_middle * zoom_ratio;
      // When we zoom out, we may end up with a different part of the
      // image in the center, since we try to avoid showing areas not
      // in the big picture coordinate system.
      if ( v_new_middle < va->page_size / 2.0 ) {
	v_new_middle = va->page_size / 2.0;
      }
      if ( v_new_middle > va->upper - va->page_size / 2.0 ) {
	v_new_middle = GSL_MAX (va->upper - va->page_size / 2.0,
				va->page_size / 2.0);
      }

      va->value = round (v_new_middle - va->page_size / 2.0);
      // Floating point math might put us off by a tiny bit, so we
      // check for this and clamp if needed.
      const gfloat max_slop = 0.4;      
      if ( va->value < 0.0 ) {
	// We should only be dealing with slight floating point
	// inexactness here.
	if ( va->value < -max_slop ) {
	  g_error ("Current vertical page position too negative: %lf\n",
		   va->value);
	}
	g_assert (va->value >= -max_slop);
	va->value = 0.0;
      }
      if ( va->value > va->upper - va->page_size ) {
	g_assert (va->value - (va->upper - va->page_size) <= max_slop);
	va->value = va->upper - va->page_size;
      }

      // Update the horizontal adjustment.

      ha->upper = ceil (GSL_MAX (self->max_x * self->zoom, ha->page_size));

      gfloat h_new_middle = h_old_middle * zoom_ratio;
      if ( h_new_middle < ha->page_size / 2.0 ) {
	h_new_middle = ha->page_size / 2.0;
      }
      if ( h_new_middle > ha->upper - ha->page_size / 2.0 ) {
	h_new_middle = GSL_MAX (ha->upper - ha->page_size / 2.0,
				ha->page_size / 2.0);
      }

      ha->value = round (h_new_middle - ha->page_size / 2.0);
      if ( ha->value < 0.0 ) {
	// We should only be dealing with slight floating point
	// inexactness here.
	g_assert (ha->value > -max_slop);
	ha->value = 0.0;
      }
      if ( ha->value > ha->upper - ha->page_size ) {
	gdouble margin = ha->value - (ha->upper - ha->page_size);
	if ( margin > max_slop ) {
	  g_error ("Current hosizontal page position too much too large: %lf",
		   margin);
	}
	g_assert (ha->value - (ha->upper - ha->page_size) <= max_slop);
	ha->value = ha->upper - ha->page_size;
      }

      // We unblock our own handler for three of the next four
      // adjustment-related emissions, since all it does it recompute
      // and rehint the max window dimensions and redraw the drawing
      // area.  Doing this once is enough.  But we do want gtk to have
      // a chance to run any code it needs to run to redraw the
      // scrollbars and such.
      gint block_count
	= g_signal_handlers_block_by_func (va, on_adjustment_changed, self);
      g_assert (block_count == 2);
      block_count
	= g_signal_handlers_block_by_func (ha, on_adjustment_changed, self);
      g_assert (block_count == 2);
     {
	g_signal_emit_by_name (va, "changed::");
	g_signal_emit_by_name (ha, "changed::");
	// IMPROVEME: Do we need both changed and value-changed
	// emmisions?  Gtk does seem to suggest yes as of this
	// writing, but this seems like a pretty silly requirement.
	g_signal_emit_by_name (va, "value-changed::");
      }
      gint unblock_count
	= g_signal_handlers_unblock_by_func (ha, on_adjustment_changed, self);
      g_assert (unblock_count == 2);
      unblock_count
	= g_signal_handlers_unblock_by_func (va, on_adjustment_changed, self);
      g_assert (unblock_count == 2);
      g_signal_emit_by_name (ha, "value-changed::");
      break;
    }
  case 'N':
  case 'n':
    {
      // Find the current image.
      gboolean found = FALSE;
      guint ii;
      for ( ii = 0 ; ii < self->images->len ; ii++ ) {
	ViewerImage *ci = g_ptr_array_index ( self->images, ii);
	if ( ci == self->image ) {
	  found = TRUE;
	  break;
	}
      }
      g_assert (found);
      if ( self->images->len > 1 ) {
	ii = (ii + 1) % self->images->len;
      }
      else {
	g_print ("Only 1 image is loaded (i.e. there is no next image).");
      }
      self->image = g_ptr_array_index (self->images, ii);
      redraw_drawing_area (self, self->da);
      break;
    }
  case 'P':
  case 'p':
    {
      // Find the current image.
      gboolean found = FALSE;
      guint ii;
      for ( ii = 0 ; ii < self->images->len ; ii++ ) {
	if ( g_ptr_array_index (self->images, ii) == self->image ) {
	  found = TRUE;
	  break;
	}
      }
      g_assert (found);
      if ( self->images->len > 1 ) {
	if ( ii == 0 ) {
	  ii = self->images->len - 1;
	}
	else {
	  ii--;
	}
      }
      else {
	g_print ("Only 1 image is loaded (i.e. there is no previous image).");
      }
      self->image = g_ptr_array_index (self->images, ii);
      redraw_drawing_area (self, self->da);
      break;
    }
  case 'a':
    {
      // Analyze a tile around the current cursor location.
      if ( self->analysis_program == NULL ) {
	g_print ("\n");
	g_print ("Analysis requested, but no analysis program was specified\n"
		 "with the --analysis-program command line option.\n");
      }
      else if ( self->cursor_x == -1 ) {
	g_assert (self->cursor_y == -1);
	g_print ("\n");
	g_print ("Analysis requested, but the cursor has not been placed.\n"
		 "(left click to place the cursor)\n");
      }
      else {
	run_analysis (self);
      }
    }
    break;
  case 'e':
    {
      g_signal_emit_by_name (self->w, "delete_event::");
    }
  case '~':
    {
      // An key not obviously convertible to a single letter, or an
      // key the simple conversion routine hasn't been taught about.
      switch ( event->keyval ) {
      case GDK_Up:
	self->cursor_y--;
	if ( self->cursor_y < 0 ) {
	  self->cursor_y = 0;
	}
	break;
      case GDK_Down:
	self->cursor_y++;
	// The cursor can be on max_y (instead of max_y - 1) because
	// we have defined max_y as the largest addressable index.
	if ( self->cursor_y > self->max_y ) {
	  self->cursor_y = self->max_y;
	}
	break;
      case GDK_Left:
	self->cursor_x--;
	if ( self->cursor_x < 0 ) {
	  self->cursor_x = 0;
	}
	break;
      case GDK_Right:
	self->cursor_x++;
	// The cursor can be on max_x (instead of max_x - 1) because
	// we have defined max_y as the largest addressable index.
	if ( self->cursor_x > self->max_x ) {
	  self->cursor_x = self->max_x;
	}
	break;
      default:
	break;
      }
      redraw_drawing_area (self, self->da);
      break;
    }
  default:
    break;
  }

  return FALSE;
}

static gboolean
on_scroll_event (GtkDrawingArea *da, GdkEventScroll *event, ViewerWindow *self)
{
  if ( event->direction == GDK_SCROLL_UP ) {
    step_zoom_in (self);
  }
  else if ( event->direction == GDK_SCROLL_DOWN ) {
    step_zoom_out (self);
  }

  return FALSE;
}

///////////////////////////////////////////////////////////////////////////////
//
// Click and Drag Scrolling
//
// Together these handlers implement click-and-drag-to-scroll.
//

static gboolean
also_on_button_press_event (GtkDrawingArea *da, GdkEventButton *event,
			    ViewerWindow *self)
{
  if ( event->type == GDK_BUTTON_PRESS && event->button == 2 ) {
    // FIXME: initialize these to magic and reset between to magic.
    self->drag_start_x = event->x;
    self->drag_start_y = event->y;
    self->drag_start_hadj_value = gtk_adjustment_get_value (self->hadj);
    self->drag_start_vadj_value = gtk_adjustment_get_value (self->vadj);
  }

  return FALSE;
}

static void
drag_scroll (ViewerWindow *self, gdouble x, gdouble y)
{
  gdouble delta_x = x - self->drag_start_x, delta_y = y - self->drag_start_y;

  // Determine the new horizontal adjustment value.
  gdouble new_hval = self->drag_start_hadj_value - delta_x;
  gdouble drag_hmax = self->hadj->upper - self->hadj->page_size;
  if ( new_hval > drag_hmax ) {
    new_hval = drag_hmax;
  }
  gtk_adjustment_set_value (self->hadj, new_hval);

  // Determine the new vertical adjustment value.
  gdouble new_vval = self->drag_start_vadj_value - delta_y;
  gdouble drag_vmax = self->vadj->upper - self->vadj->page_size;
  if ( new_vval > drag_vmax ) {
    new_vval = drag_vmax;
  }
  gtk_adjustment_set_value (self->vadj, new_vval);

  // We only want to end up redrawing once, so we block our handler
  // for one of the two adjustment changed emissions.  But we still
  // emit both so internal Gtk handlers can update the scroll bars or
  // whatever they do.
  gint block_count
    = g_signal_handlers_block_by_func (self->hadj, on_adjustment_changed,
				       self);
  g_assert (block_count == 2);
  {
    gtk_adjustment_value_changed (self->hadj);
  }
  gint unblock_count
    = g_signal_handlers_unblock_by_func (self->hadj, on_adjustment_changed,
					 self);
  g_assert (unblock_count == 2);
  gtk_adjustment_value_changed (self->vadj);
}

static gboolean
on_motion_notify_event (GtkDrawingArea *da, GdkEventMotion *event,
			ViewerWindow *self)
{
  if ( event->state & GDK_BUTTON2_MASK ) {
    if ( self->drag_start_x != -1 ) {
      drag_scroll (self, event->x, event->y);
    }
    else {
      g_assert (self->drag_start_y == -1);
    }

    // We don't really care what this call returns, since we have
    // already redrawn according to the motion_notify_event we just
    // received, but we make it in order to let GDK know we're ready
    // for the next motion event (seek GdkEventMask documentation, in
    // particular the discussion of GDK_POINTER_MOTION_HINT_MASK).
    gint junk_x, junk_y;
    GdkModifierType mask;
    gdk_window_get_pointer ((GTK_WIDGET (da))->window, &junk_x, &junk_y,
			    &mask);
  }

  return FALSE;
}

static gboolean
on_button_release_event (GtkDrawingArea *da, GdkEventButton *event,
			 ViewerWindow *self)
{
  if ( event->type == GDK_BUTTON_RELEASE && event->button == 2 ) {
    if ( self->drag_start_x != -1 ) {
      drag_scroll (self, event->x, event->y);
    }
    self->drag_start_x = -1;
    self->drag_start_y = -1;
  }

  return FALSE;
}
			 

///////////////////////////////////////////////////////////////////////////////

// Initialize the gtk and gtkglext libraries, and create the
// GdkGLConfig we will be usein.
static void
initialize_libraries (void)
{

  // FIIXME: verify that passing NULL is really ok.  It sure seems to work.
  gtk_init (NULL, NULL);
  gtk_gl_init (NULL, NULL);

  // Check OpenGL version.
  gint major, minor;
  gdk_gl_query_version (&major, &minor);
  g_assert (major >= 1 && minor >= 2);

  glconfig = gdk_gl_config_new_by_mode (  GDK_GL_MODE_RGB 
					  | GDK_GL_MODE_DEPTH
					  | GDK_GL_MODE_DOUBLE); 
  g_assert (glconfig != NULL);
}

ViewerWindow *
viewer_window_new (ViewerImage *image, GPtrArray *images,
		   size_t max_x, size_t max_y,
		   size_t start_x, size_t start_y, size_t w, size_t h,
		   GString *analysis_program, gboolean async_analysis,
		   gint analysis_tile_size)		   
{
  if ( ! initialized ) {
    initialize_libraries ();
  }

  ViewerWindow *self = g_new0 (ViewerWindow, 1);

  self->image = viewer_image_ref (image);

  // Note this is a pointer to a list owned by main(), so we must not
  // change or free it.
  self->images = images;

  self->max_x = max_x;
  self->max_y = max_y;

  self->start_x = start_x;
  self->start_y = start_y;

  // The cursor begins life unplaced.
  self->cursor_x = -1;
  self->cursor_y = -1;

  // Store the analysis-related arguments.
  if ( analysis_program == NULL ) {
    self->analysis_program = NULL;
  }
  else {
    self->analysis_program = g_string_new (analysis_program->str);
    self->async_analysis = async_analysis;
    self->analysis_tile_size = analysis_tile_size;
  }

  // Haven't gotten any data to draw before, so there isn't going to
  // be anything to free the first time we draw.
  self->dp_to_free = NULL;

  // Set up the Gtk window.
  GtkWidget *window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  // Top level GtkObject instances have only a floating reference
  // which gets claimed by the GTK library itself, so we have to ref
  // the window.
  gtk_widget_ref (window);
  GString *title = g_string_new (image->base_name->str);
  g_string_prepend (title, "ssv: ");
  gtk_window_set_title (GTK_WINDOW (window), title->str);
  gtk_container_set_reallocate_redraws (GTK_CONTAINER (window), TRUE);
  
  self->w = window;

  // Set up table to hold the drawing area and scrollbars.

  GtkTable *table = GTK_TABLE (gtk_table_new (2, 2, FALSE));
  GtkWidget *hbar = gtk_hscrollbar_new (NULL);
  // IMPROVEME: we may want to use UPDATE_DELAYED until we find that
  // the pyramids are done.
  gtk_range_set_update_policy (GTK_RANGE (hbar), GTK_UPDATE_CONTINUOUS);
  GtkAdjustment *hadj = gtk_range_get_adjustment (GTK_RANGE (hbar));
  hadj->lower = 0;
  hadj->value = 0;
  self->hadj = hadj;

  GtkWidget *vbar = gtk_vscrollbar_new (NULL);
  // IMPROVEME: we may want to use UPDATE_DELAYED until we find that
  // the pyramids are done.
  gtk_range_set_update_policy (GTK_RANGE (vbar), GTK_UPDATE_CONTINUOUS);
  GtkAdjustment *vadj = gtk_range_get_adjustment (GTK_RANGE (vbar));
  vadj->lower = 0;
  vadj->value = 0;
  self->vadj = vadj;

  gtk_table_attach (table, vbar, 1, 2, 0, 1, GTK_FILL, GTK_FILL | GTK_EXPAND,
		    0, 0);
  gtk_table_attach (table, hbar, 0, 1, 1, 2, GTK_FILL | GTK_EXPAND, GTK_FILL,
		    0, 0);

  // When the GtkAdjustment instances associated with the scrollbars
  // change, we need to redraw things.
  g_signal_connect (vadj, "changed::", G_CALLBACK (on_adjustment_changed),
		    self);
  g_signal_connect (vadj, "value-changed::",
		    G_CALLBACK (on_adjustment_changed), self);
  g_signal_connect (hadj, "changed::", G_CALLBACK (on_adjustment_changed),
  		    self);
  g_signal_connect (hadj, "value-changed::",
  		    G_CALLBACK (on_adjustment_changed), self);

  // Set up the drawing area.
  GtkWidget *da = gtk_drawing_area_new ();
  gboolean return_code = gtk_widget_set_gl_capability (da, glconfig, NULL,
						       TRUE, GDK_GL_RGBA_TYPE);
  g_assert (return_code);
  g_signal_connect_after (G_OBJECT (da), "realize", G_CALLBACK (after_realize),
  			  self);
  g_signal_connect (G_OBJECT (da), "configure_event",
  		    G_CALLBACK (on_drawing_area_configure_event), self);
  g_signal_connect (G_OBJECT (da), "expose_event",
  		    G_CALLBACK (on_expose_event), self);
  gtk_widget_add_events (da, GDK_BUTTON_PRESS_MASK);
  g_signal_connect (G_OBJECT (da), "button_press_event",
		      G_CALLBACK (on_button_press_event), self);
  self->da = da;

  // Path the drawing area into the table.
  gtk_table_attach_defaults (table, self->da, 0, 1, 0, 1);

  // Pack the table into the window.
  gtk_container_add (GTK_CONTAINER (self->w), GTK_WIDGET (table));

  // True iff the entire image should fit in a drawing area of the
  // default dimensions without any zooming.
  size_t def_w = VIEWER_WINDOW_DRAWING_AREA_DEFAULT_WIDTH;
  size_t def_h = VIEWER_WINDOW_DRAWING_AREA_DEFAULT_HEIGHT;
  gboolean image_fits = (w <= def_w && h <= def_h);
  if ( image_fits ) {
    self->zoom = 1.0;
    // The entire image should ift in the drawing area, so we can just
    // use a smaller drawing area.  IMPROVEME: but maybe using small
    // windows sometimes is just confusing and irritating to the user,
    // and causes window managers to place things inconsistently?
    gtk_widget_set_size_request (self->da, w, h);
  }
  else {
    // We try to make the drawing area have the standard default size.
    // The window manager may thwart us, of course.  IMPROVEME: here
    // is another point where work might be needed to implement
    // power-of-two zoom level locking (see IMPROVEME elsewhere).
    gtk_widget_set_size_request (self->da, def_w, def_h);
    if ( (double) w / def_w > (double) h / def_h ) {
      self->zoom = (double) def_w / w;
    }
    else {
      self->zoom = (double) def_h / h;
    }
  }
  vadj->page_size = self->da->allocation.height;
  vadj->upper = GSL_MAX (self->max_y * self->zoom, vadj->page_size);
  hadj->page_size = self->da->allocation.width;
  hadj->upper = GSL_MAX (self->max_x * self->zoom, hadj->page_size);

  // FIIXME: Presumably don't need to emit a changed signal or
  // anything on the adjustments here since everything is yet to be
  // drawn (confirm)?

  g_signal_connect (G_OBJECT (self->w), "key_press_event",
		    G_CALLBACK (on_key_press_event), self);

  g_signal_connect (G_OBJECT (self->da), "scroll_event",
		    G_CALLBACK (on_scroll_event), self);

  // Connect to the click-and-drag scrolling handlers.
  g_signal_connect (G_OBJECT (self->da), "button_press_event",
  		    G_CALLBACK (also_on_button_press_event), self);
  g_signal_connect (G_OBJECT (self->da), "motion_notify_event",
		    G_CALLBACK (on_motion_notify_event), self);
  g_signal_connect (G_OBJECT (self->da), "button_release_event",
		    G_CALLBACK (on_button_release_event), self);

  GdkEventMask da_events;
  g_object_get (G_OBJECT (self->da), "events", &da_events, NULL);
  if ( ! (da_events
	  & (GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON2_MOTION_MASK)) ) {
    da_events |= (GDK_POINTER_MOTION_HINT_MASK | GDK_BUTTON2_MOTION_MASK);
    g_object_set (G_OBJECT (self->da), "events", da_events, NULL);
  }

  // Set the maximum dimensions of the window commensurate with the
  // available data.
  reset_window_max_dimensions (self);

  // Here lies an attempt to figure out why sometimes the window comes
  // out only the height of the layer for a roughly square layer.  Who
  // knows.  Put it down to window manager evilness, though I think it
  // might be a concurrency issue of some sort at the Gtk level.
  /*
  GtkResizeMode rm = gtk_container_get_resize_mode (GTK_CONTAINER (table));
  switch ( rm ) {
  case GTK_RESIZE_PARENT:
    g_print ("table in PARENT mode\n");
    break;
  case GTK_RESIZE_QUEUE:
    g_print ("table in QUEUE mode\n");
    break;
  case GTK_RESIZE_IMMEDIATE:
    g_print ("table in IMMEDIATE mode\n");
    break;
  default:
    g_assert_not_reached ();
    break;
  }
  */					    

  // Display the new window and everything it contains (triggering the
  // signals that do the OpenGL drawing).
  gtk_widget_show_all (self->w);

  gtk_widget_set_size_request (self->da, -1, -1);

  self->reference_count = 1;

  return self;
}

void
viewer_window_unref (ViewerWindow *self)
{
  self->reference_count--;

  if ( self->reference_count == 0 ) {
    g_assert (self->w != NULL);
    gtk_widget_unref (self->w);
    viewer_image_unref (self->image);

    g_free (self);
  }
}
