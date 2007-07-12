// Try to isolate a failure case that seems to afflict Mesa (and
// possibly other OpenGL implementations, since the (presumably
// hardware) implementation did the same thing on my EmperorLinux
// laptop).

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include <GL/glut.h>

// Window dimensions to use.
#define WINDOW_WIDTH 600
#define WINDOW_HEIGHT 600

// File containing the raw data we want to display.
#define TEST_IMAGE_FILE_NAME "test_layer_dump.raw"

// Dimension of the raw image data.
#define TEST_IMAGE_WIDTH 700
#define TEST_IMAGE_HEIGHT 475

// Bias and scale values to use to get the test image to show up
// decently.
#define TEST_IMAGE_BIAS 7
#define TEST_IMAGE_SCALE 248

// Image data 
GLfloat testImage[TEST_IMAGE_WIDTH][TEST_IMAGE_HEIGHT];

static GLdouble zoomFactor = 0.85714;
static GLint height;

// Throw a fatal exception if an OpenGL error is found.
static void
trap_opengl_errors (void)
{
  GLenum gl_error_code = glGetError ();
  
  if ( gl_error_code != GL_NO_ERROR ) {
    fprintf (stderr, __FILE__ " line %d: OpenGL error detected: %s",
	     __LINE__, gluErrorString (gl_error_code));
    exit (EXIT_FAILURE);
  }
}

// Load the test image into global memory.
static void
loadImage (void)
{
  FILE *fp = fopen (TEST_IMAGE_FILE_NAME, "r");
  assert (fp != NULL);

  size_t floats_to_read = TEST_IMAGE_WIDTH * TEST_IMAGE_HEIGHT;
  size_t floats_read = fread (testImage, sizeof (GLfloat), floats_to_read, fp);
  assert (floats_read == floats_to_read);

  int return_code = fclose (fp);
  assert (return_code == 0);
}

void
init (void)
{    
   glClearColor (0.0, 0.0, 0.0, 0.0);
   glShadeModel (GL_FLAT);
   loadImage ();
   glPixelStorei (GL_UNPACK_ROW_LENGTH, TEST_IMAGE_WIDTH);

   // We are dealing with floats so we don't need this.
   // glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
}

void
display (void)
{
  // FIIXME: need the scale and bias functions still.
  glPixelTransferf (GL_RED_BIAS, TEST_IMAGE_BIAS / TEST_IMAGE_SCALE);
  glPixelTransferf (GL_GREEN_BIAS, TEST_IMAGE_BIAS / TEST_IMAGE_SCALE);
  glPixelTransferf (GL_BLUE_BIAS, TEST_IMAGE_BIAS / TEST_IMAGE_SCALE);
  glPixelTransferf (GL_RED_SCALE, 1.0 / TEST_IMAGE_SCALE);
  glPixelTransferf (GL_GREEN_SCALE, 1.0 / TEST_IMAGE_SCALE);
  glPixelTransferf (GL_BLUE_SCALE, 1.0 / TEST_IMAGE_SCALE);

  glClear (GL_COLOR_BUFFER_BIT);
  // FIIXME: what the mystery 100?
  glRasterPos2i (0, 0);
  //  glRasterPos2i (0, WINDOW_HEIGHT - 2 - 100);
  glDrawPixels (TEST_IMAGE_WIDTH, TEST_IMAGE_HEIGHT, GL_LUMINANCE,
		GL_FLOAT, testImage);

  // Draw a blue 'X' near the current raster position.
  glColor3d (0.0, 1.0, 0.0);
  glBegin (GL_LINES);
  {
    glVertex2d (10, WINDOW_HEIGHT - 2 - 100 - 5);
    glVertex2d (20, WINDOW_HEIGHT - 2 - 100 + 5);
  }
  glEnd ();
  glBegin (GL_LINES);
  {
    glVertex2d (20, WINDOW_HEIGHT - 2 - 100 - 5);
    glVertex2d (10, WINDOW_HEIGHT - 2 - 100 + 5);
  }
  glEnd ();



  // Here we draw a small red 'X' onto the lower left corner of the
  // window, near where we expect the bottom of the raster image to
  // end up.
  glColor3d (1.0, 0.0, 0.0);
  glBegin (GL_LINES);
  {
    glVertex2d (10, 10);
    glVertex2d (20, 20);
  }
  glEnd ();
  glBegin (GL_LINES);
  {
    glVertex2d (10, 20);
    glVertex2d (20, 10);
  }
  glEnd ();
  
  glFlush();

  trap_opengl_errors ();
}

void
reshape (int w, int h)
{
   glViewport (0, 0, (GLsizei) w, (GLsizei) h);
   height = (GLint) h;
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluOrtho2D (0.0, (GLdouble) w, 0.0, (GLdouble) h);
   glMatrixMode (GL_MODELVIEW);
   glLoadIdentity ();
}

void
motion (int x, int y)
{
   static GLint screeny;
   
   screeny = height - (GLint) y;
   glRasterPos2i (x, screeny);
   glPixelZoom (zoomFactor, zoomFactor);
   glCopyPixels (0, 0, TEST_IMAGE_WIDTH, TEST_IMAGE_HEIGHT, GL_COLOR);
   glPixelZoom (1.0, 1.0);
   glFlush ();
}

void
keyboard (unsigned char key, int x, int y)
{
   switch (key) {
      case 'r':
      case 'R':
         zoomFactor = 1.0;
         glutPostRedisplay();
         printf ("zoomFactor reset to 1.0\n");

         break;
      case 'z':
         zoomFactor += 0.5;
         if (zoomFactor >= 3.0) 
            zoomFactor = 3.0;
         printf ("zoomFactor is now %4.1f\n", zoomFactor);
         break;
      case 'Z':
         zoomFactor -= 0.5;
         if (zoomFactor <= 0.5) 
            zoomFactor = 0.5;
         printf ("zoomFactor is now %4.1f\n", zoomFactor);
         break;
      case 27:
         exit(0);
         break;
      default:
         break;
   }
}

int
main (int argc, char** argv)
{
   glutInit (&argc, argv);
   glutInitDisplayMode (GLUT_SINGLE | GLUT_RGB);
   glutInitWindowSize (WINDOW_WIDTH, WINDOW_HEIGHT);
   glutInitWindowPosition (100, 100);
   glutCreateWindow (argv[0]);
   init ();
   glutDisplayFunc (display);
   glutReshapeFunc (reshape);
   glutKeyboardFunc (keyboard);
   glutMotionFunc (motion);
   glutMainLoop ();
   return 0; 
}
