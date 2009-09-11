/****************************************************************
FUNCTION NAME: create_name

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:

****************************************************************/


/*Now in asf.a/fileUtil.c
#define BUFFER		256

void create_name(char *outname, char *inname, char *ext)
{
  char name[BUFFER];
  int i;

  strcpy(name,inname);
  strtok(name,".");
  i = strlen(name);
  name[i] = '\0';
  strcat(strcpy(outname,name),ext);
}
*/
