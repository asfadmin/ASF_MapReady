#ifndef _ASF_LICENSE_H_
#define _ASF_LICENSE_H_

#define ASF_COPYRIGHT_STRING \
"Copyright 1987-2014 US Government. This work, authored by Alaska Satellite\n" \
"Facility employees, was funded in whole or in part by NASA, under US\n" \
"Government contract NNG13HQ05C.\n" \

enum {
        ASF_LICENSE_ID=1
} license_id_t;

#define ASF_LICENSE_STRING \
"ASF Mapready is free software: you can redistribute it and/or modify it\n" \
"under the terms of the GNU General Public License as published by the Free\n" \
"Software Foundation, either version 3 of the License, or (at your option)\n" \
"any later version.\n" \
"\n" \
"ASF Mapready is distributed in the hope that it will be useful, but WITHOUT\n" \
"ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or\n" \
"FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for\n" \
"more details.\n" \
"\n" \
"You should have received a copy of the GNU General Public License along\n" \
"with ASF Mapready. If not, see <http://www.gnu.org/licenses/>.\n" \

/* If either of -license or -version are given, print the appropriate info
   and then exit. */
void handle_license_and_version_args(int argc, char *argv[],
                                     const char *program_name);
void print_copyright(void);
void print_license(int license_id);
void print_version(const char *program_name);
const char *version_string(const char *);

#endif   // _ASF_LICENSE_H_

