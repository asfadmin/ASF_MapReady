static char *sccs_h = "@(#)ims_access.h	1.1  01/23/97";

/***********************************************************
**
** File:        ims_access.h
**
** Function:    This include file is used by ims_access.c, the
**              v0 access program for the ASF IMS system.
**              originally (and mostly the same as) written by
**              Jeff Cleveland, Langley DAAC.
**              later, Jeff Cleveland, Computer Sciences Corp.
**
** Author:      David Pass
**
** Date:        11/1/96
**
**************************************************************** */



/* Version 2: The following is the message for new users */


/* Version 2: The following are environment variables and values */
/*            for communicating user type to the IMS client      */

#define USER_TYPE "IMS_USER_TYPE"
#define GUEST "IMS_GUEST_USER"
#define REGISTERED "IMS_REGISTERED_USER"
#define STAFF "IMS_STAFF_USER"

#ifdef Motif1_1
/* some versions of Motif 1.1 won't automatically size widgets to the */
/* size of their label text, so use this define to define size for    */
/* widgets in forms. If you change the fonts in the defaults file,    */
/* you will need to change this value so that the new font size won't */
/* get clipped */

#define ITEMHEIGHT 32

#endif

/* field lengths (from ODL for user profile information) */
/* the user access database should match these values */
#define FNAME_LEN 20
#define LNAME_LEN 20
#define KEY_LEN   12
#define ENCRYPTED_KEY_LEN 20

/* this length from database schema */
#define DIR_LEN 100

/* size for miscellaneous strings */
/* should be larger than any of the above sizes */
#define BIG_STRING 150

/* modes for changecase routine */
#define ANY_CASE        0
#define LOWER_CASE      1
#define UPPER_CASE      2
#define LEADING_CAPS    3

/* key for identifying staff logins */
#define STAFF_KEY "ims_staff"

/* patch 1:macro for truncating strings */
#define truncate( a,b,c ) { (void) strncpy( a,b,c ); a[c-1]='\0'; }
/* Version 2: use all caps for truncate */
#define TRUNCATE( a,b,c ) { (void) strncpy( a,b,c ); a[c-1]='\0'; }

/*
**  define values needed to identify a user
*/
typedef  struct  id_values__t{
    char first_name[FNAME_LEN+1];
    char middle_initial[FNAME_LEN+1];
    char last_name[FNAME_LEN+1];
    char address[129];
    char city[36];
    char phone[26];
    char email[129];
    char state[21];
    char country[21];
    char zip[11];
    char fax[26];
    char title[11];
    char organization[36];
    }  id_values_t;

typedef  struct  profile__t{
    char initial_user_key[KEY_LEN+1];
    char authenication_key[KEY_LEN+1];
    char authenticator[KEY_LEN+1];
    id_values_t  request;
    id_values_t  shipping;
    id_values_t  billing;
    char  catagory[36];
    char  type[36];
    } profile_t;


#ifdef GUI

/* structure for keeping window information */
struct welcomerec {
     Widget fname;
     Widget lname;
     Widget accesskey;
     Widget shell;
     Widget changeflag;
     Widget dialog;
     char key[KEY_LEN+1];
     char encrypted_key[ENCRYPTED_KEY_LEN+1];
     char dir[BIG_STRING];
     float diskquota;
     Boolean isNew; /* is this a new user? */
};

/* structure for changing password */

struct keyrec {
     struct welcomerec *info;
     Widget shell;
     Widget key;
     Boolean new; /* new user? */
     char key1[KEY_LEN+1];
     char key2[KEY_LEN+1];
     int pass; /* how many times have we asked for key?*/

     /* Version 2: save info so we can change password/name
        after ims execution */
     Boolean postIMS;
     char fname[FNAME_LEN+1];
     char lname[LNAME_LEN+1];
     char thekey[KEY_LEN+1];
     char old_fname[FNAME_LEN+1];
     char old_lname[LNAME_LEN+1];
     char orig_key[ENCRYPTED_KEY_LEN];
};

#define TIMEOUT 3000   /* 30 secs */

/* ************************
**  define structure for holding window information.
*/

/* structure for delayed info update from client */

struct clientinforec {
  /* Version 2: add key field so we can update key after IMS exit. */
  char fname[FNAME_LEN+1];
  char lname[LNAME_LEN+1];
  char orig_key[ENCRYPTED_KEY_LEN+1];
  char tempdir[BIG_STRING];
  char guestdir[BIG_STRING];
  char key[KEY_LEN+1];
  int pid;
  Boolean guest;
};

#else /*ChUI*/

/* structure for storing welcome stuff */

struct welcomerec {
     char fname[FNAME_LEN+1],lname[LNAME_LEN+1],key[KEY_LEN+1],
          encrypted_key[ENCRYPTED_KEY_LEN+1],dir[BIG_STRING];
     float diskquota;
};

#endif

