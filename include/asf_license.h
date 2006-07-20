#ifndef _ASF_LICENSE_H_
#define _ASF_LICENSE_H_

enum {
        ASF_BSD_ID=1
} license_id_t;

#define ASF_BSD_LICENSE_STRING \
"Redistribution and use in source and binary forms, with or without\n"\
"modification, are permitted provided that the following conditions are met:\n"\
"\n"\
"  * Redistributions of source code must retain the above copyright notice, this\n"\
"    list of conditions and the following disclaimer.\n"\
"  * Redistributions in binary form must reproduce the above copyright notice,\n"\
"    this list of conditions and the following disclaimer in the documentation\n"\
"    and/or other materials provided with the distribution.\n"\
"  * Neither the name of the University of Alaska Fairbanks, nor its subunits,\n"\
"    nor the names of its contributors may be used to endorse or promote products\n"\
"    derived from this software without specific prior written permission.\n"\
"  * Redistribution and use of source and binary forms are for noncommercial\n"\
"    purposes only.\n"\
"\n"\
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND\n"\
"ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\n"\
"WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\n"\
"DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR\n"\
"ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n"\
"(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n"\
"LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON\n"\
"ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n"\
"(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n"\
"SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"\

/* If either of -license or -version are given, print the appropriate info
   and then exit. */
void handle_license_and_version_args(int argc, char *argv[],
                                     const char *program_name);

#endif   // _ASF_LICENSE_H_

