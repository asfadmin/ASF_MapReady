/*
 * Name - virtual.h
 *
 * ccs version:	1.3
 *
 * ccsid:	@(#)virtual.h	1.3 - 07/06/92 10:52:59
 * from: 	ccs/s.virtual.h
 * date: 	06/28/93 09:14:49
 *
 *--- original header:
 *
 * vroot.h -- Virtual Root Window handling header file
 *
 * This header file redefines the X11 macros RootWindow and DefaultRootWindow,
 * making them look for a virtual root window as provided by certain `virtual'
 * window managers like swm and tvtwm. If none is found, the ordinary root
 * window is returned, thus retaining backward compatibility with standard
 * window managers.
 * The function implementing the virtual root lookup remembers the result of
 * its last invocation to avoid overhead in the case of repeated calls
 * on the same display and screen arguments.
 * The lookup code itself is taken from Tom LaStrange's ssetroot program.
 *
 * Most simple root window changing X programs can be converted to using
 * virtual roots by just including
 *
 * #include "vroot.h"
 *
 * after all the X11 header files.  It has been tested on such popular
 * X clients as xphoon, xfroot, xloadimage, and xaqua.
 *
 * Andreas Stolcke (stolcke@ICSI.Berkeley.EDU), 9/7/90
 */

static char sccsid_virtual_h[] =
    "@(#)virtual.h	1.2 95/10/03 21:05:18";

static Window
VirtualRootWindow(dpy, screen)
Display *dpy;
{
	static Display *save_dpy = (Display *)0;
	static int save_screen = -1;
	static Window root = (Window)0;

	Atom __SWM_VROOT = None;
	int i;
	Window rootReturn, parentReturn, *children;
	unsigned int numChildren;

	if ( dpy != save_dpy || screen != save_screen ) {
		root = RootWindow(dpy, screen);

		/* go look for a virtual root */
		__SWM_VROOT = XInternAtom(dpy, "__SWM_VROOT", False);
		XQueryTree(dpy, root, &rootReturn, &parentReturn,
				 &children, &numChildren);
		for (i = 0; i < numChildren; i++) {
			Atom actual_type;
			int actual_format;
			unsigned long nitems, bytesafter;
			Window *newRoot = (Window *)0;

			if (XGetWindowProperty(dpy, children[i], __SWM_VROOT,
				0, 1, False, XA_WINDOW,
				&actual_type, &actual_format,
				&nitems, &bytesafter,
				(unsigned char **) &newRoot) == Success
			    && newRoot) {
			    root = *newRoot;
			    break;
			}
		}

		save_dpy = dpy;
		save_screen = screen;
	}

	return root;
}

/*********************
#undef DefaultRootWindow
#define DefaultRootWindow(dpy) RootWindow(dpy, DefaultScreen(dpy))

#undef RootWindow
#define RootWindow(dpy,screen) VirtualRootWindow(dpy,screen)
**********************/
