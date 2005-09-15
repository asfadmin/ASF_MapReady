// tk_winmain.c:  Defines the Win32 entry point for the program.
//  A simple wrapper around tkAppInit's *real* main.
//
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main(int argc,			/* Number of command-line arguments. */
    char *argv[]);		/* Values of command-line arguments. */

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{
/*I can't believe Windows makes you parse your own command line...*/
	int argc=0;
	char *argv[100]={NULL};
	{
		int location=0,numRead=0;
		char inputString[1024];
		char *cmdLine=(char *)GetCommandLine();
		char *scanFormat="\"%[^\"]%n";
		if (cmdLine[0]!='"')
			scanFormat="%s%n";
		while (0<sscanf(&cmdLine[location],scanFormat,inputString,&numRead))
		{
			int start=0;
			argv[argc]=(char *)malloc(1024);
			while (inputString[start]!=0&&isspace(inputString[start]))
				start++;/*Advance over white space*/
			if (strlen(&inputString[start])==0) break;/*Skip zero-crap*/
			strcpy(argv[argc],&inputString[start]);
			argc++;location+=numRead;numRead=0;
		}
	}
/*Call the real main*/
	main(argc,argv);
	return 0;
}
