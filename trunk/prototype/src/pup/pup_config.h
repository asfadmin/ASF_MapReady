/*
  Pup machine-dependent configuration file.
  Only needed if you care about these funky datatypes.
*/
#ifndef __LAWLOR_PUP_CONFIG_H
#define __LAWLOR_PUP_CONFIG_H

/* Gcc uses "long long" for 64-bit type */
//#undef PUP_LONG_LONG long long
/* Win32 uses "__int64" for a 64-bit type */
//#undef PUP_LONG_LONG __int64
/* Set this if int, short, and long are enough for you */
#undef PUP_LONG_LONG

/* Set this to a 4-byte integer type */
#define PUP_TYPEDEF_INT4 int

/* Set this if "signed char" isn't the same as just "char" */
#define PUP_SIGNEDCHAR_DIFF_CHAR 1

/* Set this if you need "long double" */
#define PUP_LONG_DOUBLE_DEFINED 0

/* Set this to 1 for a slight speed increase */
#define PUP_OPTIMIZE 0

/* Fortran name mangling (if you need it) */
#define PUP_FTN_NAME(UPPER,lower) lower##_ 

#endif
