/******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include <ctype.h>

#define VERSION 1.0

char *uc(char *string)
{
  char *out=(char *)MALLOC(sizeof(char)*255);
  int i;
  
  for (i=0; i<strlen(string); i++) out[i]=toupper(string[i]);
  out[i]='\0';
  
  return out;
}

char* change_special_characters(char *in)
{
  int i,j=0;
  char *out=(char *)MALLOC(sizeof(char)*512);
  *out = NULL;

  for (i=0; i<255; i++) {
    if (in[i]=='&') { 
      strcat(&out[j],"&amp;");
      j+=5;
    }
    else if (in[i]=='<') {
      strcat(&out[j],"&lt;");
      j+=4;
    }
    else if (in[i]=='>') {
      strcat(&out[j],"&gt;");
      j+=4;
    }
    else {
      out[j] = in[i];
      out[j+1] = '\0';
      j++;
    }
  }

  return out;
}

void find_tag(char *file, char *begin_tag, char *end_tag, FILE *fpOut, int html_flag)
{
  FILE *fpIn;
  char *line=(char *)MALLOC(sizeof(char)*255);
  char *out=(char *)MALLOC(sizeof(char)*255);

  fpIn = FOPEN(file, "r");
  while (fgets(line, 255, fpIn) != NULL) {
    if (strncmp(uc(line), begin_tag, strlen(begin_tag))==0) {
      fgets(line, 255, fpIn);
      while (strncmp(uc(line), end_tag, strlen(end_tag))!=0) {
        if (html_flag) {
	  out = change_special_characters(line);
	  fprintf(fpOut, "%s", out);
	}
        else 
	  fprintf(fpOut, "%s", line);
        fgets(line, 255, fpIn);
      }
    }
  }
  FCLOSE(fpIn);
}

int main(int argc, char **argv)
{
  char code_name[255], man_name[255], html_name[255], base_name[255];
  FILE *fp;
  extern int currArg; /* from cla.h in asf.h... initialized to 1 */

  /* Parse command line args */
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  strcpy(base_name, argv[currArg]);
  create_name(code_name, base_name, ".c");
  create_name(man_name, base_name, ".1");
  create_name(html_name, base_name, ".html");

  printf("\nDate: %s\nProgram: create_man_page\n\n", date_stamp());

  /* Write man page in HTML format */
  fp = FOPEN(html_name, "w");

  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01"
	  " Transitional//EN\">\n");
  fprintf(fp, "<html>\n<head>\n");
  fprintf(fp, "<title>%s</title>\n", base_name);
  fprintf(fp, "<meta http-equiv=\"Content-Type\""
	  " content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "</head>\n<body>\n");
  fprintf(fp, "<table width=600 border=0>\n");
  fprintf(fp, "<tr><td colspan=2><strong>NAME</strong></td></tr>\n");
  fprintf(fp, "<tr><td width=10></td>\n<td><pre>");
  find_tag(code_name, "<NAME>", "</NAME>", fp, 1);
  fprintf(fp, "</pre><br></td></tr>\n");
  fprintf(fp, "<tr><td colspan=2><strong>SYNOPSIS</strong></td></tr>\n");
  fprintf(fp, "<tr><td width=10></td>\n<td><pre>");
  find_tag(code_name, "<SYNOPSIS>", "</SYNOPSIS>", fp, 1);
  fprintf(fp, "</pre><br></td></tr>\n");
  fprintf(fp, "<tr><td colspan=2><strong>DESCRIPTION</strong></td></tr>\n");
  fprintf(fp, "<tr><td width=10></td>\n<td><pre>");
  find_tag(code_name, "<DESCRIPTION>", "</DESCRIPTION>", fp, 1);
  fprintf(fp, "</pre><br></td></tr>\n");
  fprintf(fp, "<tr><td colspan=2><strong>INPUT</strong></td></tr>\n");
  fprintf(fp, "<tr><td width=10></td>\n<td><pre>");
  find_tag(code_name, "<INPUT>", "</INPUT>", fp, 1);
  fprintf(fp, "</pre><br></td></tr>\n");
  fprintf(fp, "<tr><td colspan=2><strong>OUTPUT</strong></td></tr>\n");
  fprintf(fp, "<tr><td width=10></td>\n<td><pre>");
  find_tag(code_name, "<OUTPUT>", "</OUTPUT>", fp, 1);
  fprintf(fp, "</pre><br></td></tr>\n");
  fprintf(fp, "<tr><td colspan=2><strong>OPTIONS</strong></td></tr>\n");
  fprintf(fp, "<tr><td width=10></td>\n<td><pre>");
  find_tag(code_name, "<OPTIONS>", "</OPTIONS>", fp, 1);
  fprintf(fp, "</pre><br></td></tr>\n");
  fprintf(fp, "<tr><td colspan=2><strong>LIMITATIONS</strong></td></tr>\n");
  fprintf(fp, "<tr><td width=10></td>\n<td><pre>");
  find_tag(code_name, "<LIMITATIONS>", "</LIMITATIONS>", fp, 1);
  fprintf(fp, "</pre><br></td></tr>\n");
  fprintf(fp, "<tr><td colspan=2><strong>SEE ALSO</strong></td></tr>\n");
  fprintf(fp, "<tr><td width=10></td>\n<td><pre>");
  find_tag(code_name, "<SEE_ALSO>", "</SEE_ALSO>", fp, 1);
  fprintf(fp, "</pre><br></td></tr>\n");
  fprintf(fp, "<tr><td colspan=2><strong>COPYRIGHT</strong></td></tr>\n");
  fprintf(fp, "<tr><td width=10></td>\n<td><pre>");
  find_tag(code_name, "<COPYRIGHT>", "</COPYRIGHT>", fp, 1);
  fprintf(fp, "</pre><br></td></tr>\n<br>\n");
  fprintf(fp, "<tr><td colspan=2>\n<hr>\n");
  fprintf(fp, "<font size=1>\nCopyright &copy; 2004, ");
  fprintf(fp, "<A HREF=\"mailto:uso@asf.alaska.edu\">"
	  "Alaska Satellite Facility</a><br>\n");
  fprintf(fp, "Created on %s<br>\n", date_stamp());
  fprintf(fp, "</td></tr>\n"); 
  fprintf(fp,"</table>\n</font>\n</body>\n</html>\n\n");

  FCLOSE(fp);

  /* Write man page in troff format */
  fp = FOPEN(man_name, "w");

  fprintf(fp, ".TH %s 1 %s\n", base_name, date_stamp());
  fprintf(fp, "\n.SH NAME\n");
  find_tag(code_name, "<NAME>", "</NAME>", fp, 0);
  fprintf(fp, "\n.SH SYNSOPSIS\n");
  find_tag(code_name, "<SYNOPSIS>", "</SYNOPSIS>", fp, 0);
  fprintf(fp, "\n.SH DESCRIPTION\n");
  find_tag(code_name, "<DESCRIPTION>", "</DESCRIPTION>", fp, 0);
  fprintf(fp, "\n.SH INPUT\n");
  find_tag(code_name, "<INPUT>", "</INPUT>", fp, 0);
  fprintf(fp, "\n.SH OUTPUT\n");
  find_tag(code_name, "<OUTPUT>", "</OUTPUT>", fp,  0);
  fprintf(fp, "\n.SH OPTIONS\n");
  find_tag(code_name, "<OPTIONS>", "</OPTIONS>", fp, 0);
  fprintf(fp, "\n.SH LIMITATIONS\n");
  find_tag(code_name, "<LIMITATIONS>", "</LIMITATIONS>", fp, 0);
  fprintf(fp, "\n.SH SEE ALSO\n");
  find_tag(code_name, "<SEE_ALSO>", "</SEE_ALSO>", fp, 0);
  fprintf(fp, "\n.SH COPYRIGHT\n");
  find_tag(code_name, "<COPYRIGHT>", "</COPYRIGHT>", fp, 0);

  FCLOSE(fp);

  return 0;
}


void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s  <file_name>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   file_name     Base name of the data file.\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s extracts the man page out of source code file", name);
  printf("\n\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}
