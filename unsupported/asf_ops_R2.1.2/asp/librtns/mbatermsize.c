/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* termsize(lines,columns) -------------------------------------------
	Retrieve the screen size of the current user's terminal
*/
#include <curses.h>
termsize(lines,columns)
int *lines,*columns;
{
	char buf[1024];
	char *name,*getenv();

	name = getenv("TERM");
	tgetent(buf,name);
	*lines = tgetnum("li");
	*columns = tgetnum("co");
}
