static char *sccs = "@(#)ims_access.c	1.7  03/17/98";

/* *********************************************************
**
** File:        ims_access.c
**
** Function:    This program generates the IMS V0 access scripts, the
**              GUI version.  This is basically the version by
**              Jeff Cleveland, Langley DAAC.
**              later, Jeff Cleveland, Computer Sciences Corp.
**              This is called by the access script, and gets the
**              V0 login from users.
**
**
** Author:      David Pass
**
** Date:        10/29/96
**
**************************************************************** */

/* replaced the logo image code in ims_access.c with the following
 provided by P. Durbin (j. donhauser 2/10/98) */
/*
 *  FILE: logo.c - contains routines that setup the logo image. 
 * 
 * Author: Jeff Cleveland 
 * 
 * PROGRAM: access_gui telnet program
 *
 * DESCRIPTION:
 *       Reads in the HDF file and creates and image.
 *       Displays image.
 *       Can display on multiple types of color visuals.
 *
 *        
 * History: 
 * Modified by Phil Durbin:
 *    Added multiple visual support. Tested on 8-bit pseudo color,
 * 16 high color and 24 bit true color visuals
 * 
 * RCS INFO:
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#define   u_long    unsigned long

#include <sys/statvfs.h>
#include <sys/utsname.h>
#include <syslog.h>
#include <dirent.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_cmd.h>
#include <ims_util.h>
#include <ims_timeConv.h>
#include <ims_getInput.h>
#include <ims_keyword.h>
#include <ims_dbms.h>
#include <unistd.h>
#include <IK_Auth.h>


#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/DrawingA.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/Frame.h>
#include <X11/Shell.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <Xm/Protocols.h>
#include <Xm/ToggleB.h>
#include <Xm/MessageB.h>
#include <Xm/DialogS.h>

/* jhd, 2/10/98 */
#include <X11/Xos.h>
#include <X11/Xutil.h>

#include <ims_access.h>



#define  NO_ROWS             0
#define  DEFAULT_DISK_QUOTA  10000000.0


/* our application shell */
static  Widget appshell;

/* busy indicator */
static  Cursor watch;

/* application context -- we'll need this to install a work proc later*/
static  XtAppContext app_context;

/* current settings (from accesslib.c)  */
static  char userpath[255],temppath[255],logopath[255],
    gui_welcome_message[255],
    gui_name[255],chui_name[255];
static  char  user_temp_dir[255], user_data_dir[255];
static  float default_disk_quota;
static  int granule_limit_cap;

/*
**  when find_user is called, this is set
*/
static  char  user_id[16];

/*  from callbacks.c  */
/* Version 2: add definition of get_user_key() and questiondialog_3()*/

/* this is a global so we don't risk a malloc failing right before
    starting the client. (An out of memory  error at that point would
    be rude.) */
static  struct clientinforec imsinfo;

/*  from window.c   */

/* Version 2: gloabl keyinfo for changing key after ims execution */
/* (We don't want an out of memory error at this point.) */
static  struct keyrec keyinfo;
static  struct welcomerec welcomeinfo;


/*  from user_update.c  */

static IMS_MSG_STRUCT *msgDesc;
static IMS_QI_DESC_OBJ *qDesc;

static  profile_t  profile; /* has all the setable variables  */


static  Widget startbox;  /* show_new_name can close the start
    box if open */
static  struct systemlogorec {
  XImage *ximage; /* image info */
  char *image; /* the image */
  int width,height;
  GC gc;
} systemlogo = { NULL,NULL,0,0,0 };

static  struct shellrec {
  Widget shell;
  struct shellrec *next;
} *shelllist=NULL;

/*
** User Information structure definition.
*/
typedef struct userSpec{
    char *username;
    char *password;
    char *server;
    char *database;
    char *program;
} USER_SPEC;

/* jhd, 2/10/98 */
int write_pixel(Pixel pvalue, int pix_size, char *image_loc);
int determine_Endianess(void);
/* jhd, 2/10/98 */

static int checkRetStatus ( IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ * );
static int find_user( char *, char *, char *, char *, char *, float * );
static int execCmd (  IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ * );
static int openConnection (  IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ **,
    USER_SPEC * );
static int getArgInput ( IMS_MSG_STRUCT *, USER_SPEC * );
static void usage (char *);
static void  get_settings();
static void waitforgui( struct clientinforec *, XtIntervalId * );
static Widget pausedialog( Widget, char *, char *, XtPointer,
    XtPointer );
static Widget questiondialog( Widget, char *, char *, XtPointer,
    XtPointer, unsigned char );
static Widget questiondialog_3( Widget, char *, unsigned char, char *,
    XtPointer, XtPointer, char *, XtPointer, XtPointer, char *,
    XtPointer, XtPointer );
static void show_start_window( Display * );
static void get_user_key( Widget, struct keyrec *, XtPointer );
static void make_getuser(  Widget, Boolean, int, int, char * );
static void make_welcome( Widget );
static int  add_user( char *, char *, char *, char *, char * );
static int  update_name( char *, char *, char *, char *, char * );
static int  touch_user( char *, char *, char * );
static int  update_key( char *, char *, char *, char * );
static char *trim( char * );
static void changecase( char *, int );
static void  closedialog( Widget, Widget, XtPointer );


/*
** Structure for getting arguments from the command-line, and also
** for getting them from the command-file.
*/
static struct commands
{
    char *username;
    char *password;
    char *commandFile;
    char *server;
    char *database;
    char *help;
    char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the command-line.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
    {"-U",              &commands.username},
    {"+username",       &commands.username},
    {"-P",              &commands.password},
    {"+password",       &commands.password},
    {"-C",              &commands.commandFile},
    {"+commandFile",    &commands.commandFile},
    {"-X",              &commands.server},
    {"+server",         &commands.server},
    {"-Y",              &commands.database},
    {"+database",       &commands.database},
    {"-h",              &commands.help},
    {"+help",           &commands.help},
    {"-r",              &commands.release},
    {"+release",        &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the above commands from the command-file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
    {"username",        &commands.username},
    {"password",        &commands.password},
    {"server",          &commands.server},
    {"database",        &commands.database},

};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);



/* *************************************************************
**
**  main () :  main routine for ims_access.c
**
**************************************************************** */
void  main( argc,argv )
int argc;
char **argv;
{
    Display *display;
    USER_SPEC userSpec;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    char *programName;
    int status;
    long seconds;
    char *char_tmp;


    commands.username = NULL;
    commands.password = NULL;
    commands.commandFile = NULL;
    commands.server = NULL;
    commands.database = NULL;
    commands.help = NULL;
    commands.release = NULL;

    /*
    ** Initialize variables.
    */
    (void) memset (&userSpec, 0, (size_t) sizeof (USER_SPEC));

    /*
    ** Get the program name and the node name.
    */
    programName = ims_extractFileName (argv[0]);
    userSpec.program = programName;
    (void) uname (&uname_info);
    (void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
    hostName[IMS_HOST_LEN] = '\0';  /* Just in case. */

    /*
    ** Allocate message facility structure.
    */
    if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
    {
        (void) fprintf (stderr,
            "Memory allocation for IMS_MSG_STRUCT structure failed.");
        exit(1);
    }

    /*
    ** Initialize the message facility options.
    */
    (void) ims_msgSubSystem (msgDesc, "IMS");
    (void) ims_msgProgramName (msgDesc, programName);
    (void) sprintf (banner, "%s::%s", hostName, programName);
    (void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
    (void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
    (void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
    (void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

    /*
    ** Initialize the signal handler.
    */

    /*
    ** Get the command-line arguments. The variable status will
    ** actually contain the number of command-line arguments
    ** processed upon successful completion.
    */
    if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
        cmdLineElmCount, msgDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "An error occurred parsing the command-line.");
        exit(1);
    }

    /*
    ** Check to see if we got everything off of the command-line.
    */
    if (status < argc)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
            "Only %d out of the %d command-line arguments were "
            "processed.", status, argc);
    }

    /*
    ** If release was specified, print it out.
    */
    if (commands.release  !=  (char *) NULL)
    {
        (void) ims_printVersion (stderr);
    }

    /*
    ** If help has been specified, print usage and exit.
    */
    if (commands.help  !=  (char *) NULL)
    {
        usage ( programName );
        (void) ims_msgStructFree (msgDesc);
        exit(0);
    }

    /*
    ** If there is a command-file present, then get any commands from
    ** this file, then overlay all commands from the command-line,
    ** except password, which will be gone by this point.
    */
    if (commands.commandFile  !=  (char *) NULL)
    {
        if ((status = ims_getFileParms (commands.commandFile,
            cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "An error occurred parsing the command-file.");
            exit(1);
        }

        /*
        ** Now, get command-line arguments again to overlay file args.
        */
        if ((status = ims_getCmdLine (argc, argv,
            cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "An error occurred parsing the command-line.");
            exit(1);
        }
    }

    /*
    ** Process the information from command-line and/or command-file.
    */
    if ((status = getArgInput (msgDesc, &userSpec)) < IMS_OK)
    {
        exit(1);
    }

    /*
    ** Okay, open the dataset connection
    */

    if (openConnection(msgDesc, &qDesc, &userSpec) < IMS_OK){
        exit( 1 );
    }

    /* intialize the profile values  */
    /*  set initial_user_key to a random value based on time */
    seconds = time(NULL);
    char_tmp = (char *) ctime(&seconds);
    char_tmp = char_tmp + 11;
    (void) memcpy( profile.initial_user_key, char_tmp, 12);
    profile.authenication_key[0] = '\0';
    profile.authenticator[0] = '\0';

    profile.request.first_name[0] = '\0';
    profile.request.middle_initial[0] = '\0';
    profile.request.last_name[0] = '\0';
    profile.request.address[0] = '\0';
    profile.request.city[0] = '\0';
    profile.request.phone[0] = '\0';
    profile.request.email[0] = '\0';
    profile.request.state[0] = '\0';
    profile.request.country[0] = '\0';
    profile.request.zip[0] = '\0';
    profile.request.fax[0] = '\0';
    profile.request.title[0] = '\0';
    profile.request.organization[0] = '\0';

    profile.shipping.first_name[0] = '\0';
    profile.shipping.middle_initial[0] = '\0';
    profile.shipping.last_name[0] = '\0';
    profile.shipping.address[0] = '\0';
    profile.shipping.city[0] = '\0';
    profile.shipping.phone[0] = '\0';
    profile.shipping.email[0] = '\0';
    profile.shipping.state[0] = '\0';
    profile.shipping.country[0] = '\0';
    profile.shipping.zip[0] = '\0';
    profile.shipping.fax[0] = '\0';
    profile.shipping.title[0] = '\0';
    profile.shipping.organization[0] = '\0';

    profile.billing.first_name[0] = '\0';
    profile.billing.middle_initial[0] = '\0';
    profile.billing.last_name[0] = '\0';
    profile.billing.address[0] = '\0';
    profile.billing.city[0] = '\0';
    profile.billing.phone[0] = '\0';
    profile.billing.email[0] = '\0';
    profile.billing.state[0] = '\0';
    profile.billing.country[0] = '\0';
    profile.billing.zip[0] = '\0';
    profile.billing.fax[0] = '\0';
    profile.billing.title[0] = '\0';
    profile.billing.organization[0] = '\0';

    profile.catagory[0] = '\0';
    profile.type[0] = '\0';

    /*  appshell=XtVaAppInitialize( &app_context,"Access",NULL,0,
                 &argc,argv,NULL,NULL );*/
    XtToolkitInitialize();
    app_context=XtCreateApplicationContext();
    display=XtOpenDisplay( app_context,NULL,"Access","Access",NULL,0,
        &argc,argv );
    if ( display  ==  NULL )
        XtError( "Could not open display." );

    /* Version 2: get setup from environment */
    get_settings();

    appshell=XtAppCreateShell( "Access","Access",
        applicationShellWidgetClass, display, NULL, 0 );

    watch=XCreateFontCursor( XtDisplay( appshell ),XC_watch );
    make_welcome( appshell );

    XtAppMainLoop( app_context );
    return;
}   /*  main  */


/***************************************************************
**
** usage ()
**
** Print command line argument switches.
**
**************************************************************** */
static void usage ( char  *programName)
{
    int i;

    (void) fprintf (stderr,
        "\n%s command-line arguments:\n\n", programName);

    for (i = 0; i < cmdLineElmCount; i++)
    {
        (void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
    }

    (void) fprintf (stderr, "\n\n");
}   /*  usage  */


/* *************************************************************
**
** getArgInput ()
**
** Process command-line and command-file arguments.
**
**************************************************************** */
static int getArgInput (
    IMS_MSG_STRUCT *msgDesc,
    USER_SPEC *userSpec )
{
    char inputBuffer[IMS_INPUT_BUF_LEN+1];


    /*
    ** Prompt user for any information NOT provided in the command
    ** structure.
    */

    /* username */
    if (commands.username  !=  (char *) NULL)
    {
        userSpec->username = commands.username;
    }
    else
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Username: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        userSpec->username = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (userSpec->username, inputBuffer);
    }

    /* password */
    if (commands.password  !=  (char *) NULL)
    {
        userSpec->password = commands.password;
    }
    else
    {
        if (ims_getPassword (inputBuffer) == NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        userSpec->password = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (userSpec->password, inputBuffer);
    }

    /* server */
    if (commands.server  !=  (char *) NULL){
        userSpec->server = commands.server;
    }

    /* database */
    if (commands.database  !=  (char *) NULL){
        userSpec->database = commands.database;
    }

    return (IMS_OK);
}   /* getArgInput */


/***************************************************************
**
**  subr openConnection ()
**
**  This function will open a connection to the SQL server.
**
**************************************************************** */
static int openConnection (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ **qDescPass,
    USER_SPEC *userSpec)
{
    IMS_QI_DESC_OBJ *qDesc;
    int status;


    /*
    ** Allocate a query descriptor
    */

    if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
                    "Could not allocate a query descriptor.");
        return(IMS_ERROR);
    }

    qDesc->cmd = (char *) malloc(IMS_COL512_LEN);

    if ((char *) qDesc->cmd == NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate the command area for the "
            "query descriptor.");
         free(qDesc->cmd);
         (void) ims_qiFreeDesc(qDesc);
         return(IMS_ERROR);
    }

    IMS_SETUSER (qDesc, userSpec->username);
    IMS_SETPSWD (qDesc, userSpec->password);

    if (userSpec->program != NULL)
        IMS_SETPROG(qDesc, userSpec->program);

    if (userSpec->server != NULL)
        IMS_SETSERVER(qDesc, userSpec->server);

    if (userSpec->database != NULL)
        IMS_SETDBNAME(qDesc, userSpec->database);

    /*
    ** Attempt to logon to database
    */

    status = ims_qiLogin(qDesc);

    if (status < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "Could not login to database server.");
        free(qDesc->cmd);
        (void) ims_qiFreeDesc(qDesc);
        return(IMS_ERROR);
    }

    IMS_SET_USERDATA(qDesc);

    *qDescPass = qDesc;  /* Set return query descriptor */
    return(IMS_OK);
}   /*  opernConnection   */


/* *************************************************************
**
**  subr abort_access -- prints error message and aborts program
**
**************************************************************** */
static void abort_access( message )
char *message;
{

    (void) ims_msg( msgDesc,  IMS_ERROR, "\n%s\n\n",message );
    (void) fflush( stderr );
    (void) sleep( 5 ); /* flush and sleep so telnet prints message
        before connection is broken */
    exit( 1 );
}   /*  abort_access   */


/* *************************************************************
**
**  subr get_settings -- reads configuration settings from environment.
**      prints warning and exits if necessary setting isn't found.
**
**************************************************************** */
static void get_settings( )
{
    char *value;
#ifndef GUI
    char  chui_welcome_message[255];
#endif


    if ( (value=getenv( "USERPATH" ))  ==  NULL )
        abort_access( "USERPATH environment variable not set." );
    else
        (void) strcpy( userpath,value );

    if ( (value=getenv( "TEMPPATH" ))  ==  NULL )
        abort_access( "TEMPPATH environment variable not set." );
    else
        (void) strcpy( temppath,value );

#ifdef GUI
    if ( (value=getenv( "LOGOPATH" ))  ==  NULL )
        abort_access( "LOGOPATH environment variable not set." );
    else
        (void) strcpy( logopath,value );

    if ( (value=getenv( "GUI_WELCOME_MESSAGE" ))  ==  NULL )
        abort_access(
            "GUI_WELCOME_MESSAGE environment variable not set." );
    else
        (void) strcpy( gui_welcome_message,value );

    if ( (value=getenv( "GUI_NAME" ))  ==  NULL )
        abort_access( "GUI_NAME environment variable not set." );
    else
        (void) strcpy( gui_name,value );

#else
    if ( (value=getenv( "CHUI_WELCOME_MESSAGE" ))  ==  NULL )
        abort_access(
            "CHUI_WELCOME_MESSAGE environment variable not set." );
    else
        (void) strcpy( chui_welcome_message,value );
    if ( (value=getenv( "CHUI_NAME" ))  ==  NULL )
        abort_access( "CHUI_NAME environment variable not set." );
    else
        (void) strcpy( chui_name,value );
#endif

    if ( (value=getenv( "DEFAULT_DISK_QUOTA" ))  ==  NULL )
        abort_access(
            "DEFAULT_DISK_QUOTA environment variable not set." );
    else
        (void) sscanf( value,"%f",&default_disk_quota );

    if ( (value=getenv( "GRANULE_LIMIT_CAP" ))  ==  NULL )
        abort_access(
            "GRANULE_LIMIT_CAP environment variable not set." );
    else
        (void) sscanf( value,"%d",&granule_limit_cap );
    return;
}   /*  get_settings  */


/* *************************************************************
**
**  subr make_new_dir -- combines specified name and pid and makes a new
**      directory.
**  basepath is directory to create in.
**  if directory already exists, halves pid and tries again.
**  returns name in dir.
**
**************************************************************** */
static void make_new_dir( basepath,lname,dirname )
char *basepath,*lname,*dirname;
{
    int numtries;
    extern int errno; /* Version 2: provide errno in error message */


    (void) sprintf( dirname,"%s/%s%ld",basepath,lname, (long) getpid());

    for ( numtries=1 ; ; numtries++ ) {  /* patch 2: start w/1 to avoid
        div by zero */
        if ( mkdir( dirname,S_IRUSR|S_IWUSR|S_IXUSR )  ==  -1 ){
            if ( errno  ==  EEXIST && getpid() / numtries  !=  0 )
                (void) sprintf( dirname,"%s/%s%ld",basepath,lname,
                    (long) (getpid()/numtries) );
            else {
                (void)  ims_msg( msgDesc, IMS_ERROR,
                    "Could not create an account for you due to a "
                    "system error (number %d).\n", errno);
                (void)  ims_msg( msgDesc, IMS_ERROR,
                    "Please contact User Services for help.");
                (void) fflush( stderr ); /* make sure message printed */
                (void) sleep( 10 );
                exit( 1 );

            }
        }
        else{
            break;
        }
    }
    return;
}   /*  make_new_dir   */


/* *************************************************************
**
**  subr remove_dir -- removes a directory and it's contents.
**
**************************************************************** */
static void remove_dir( directory )
char *directory;
{

    DIR *dir;
    struct dirent *entry;
    char filename[1024];
    int  status;
    struct  stat  in_stat;


    /* remove directory and files */
    dir=opendir( directory );
    if(  dir  ==  NULL  )  return;
    for ( entry=readdir( dir ); entry  !=  NULL; entry=readdir( dir ) ){
        if ( strcmp( entry->d_name,"." )  !=  0 && strcmp(
            entry->d_name,".." )  !=  0 ) {
            (void) sprintf( filename,"%s/%s",directory,entry->d_name );
            status =  remove( filename );
            if(  status  <  0  ){
                /*
                **  delete failed: if directory, continue
                */
                status = stat( filename, &in_stat );
                if(  S_ISDIR( in_stat.st_mode ) ){
                    /*
                    **  it is a directory: delete this directory.
                    */
                    remove_dir( filename );
                }
            }
        }
    }
    (void) closedir( dir );
    (void) rmdir( directory );
    return;
}   /*  remove_dir  */


/* *************************************************************
**
**  subr begin_guest -- starts a guest session (creates dir for info )
**  never returns from this function -- if isChui == NULL, starts chui.
**  (GUI calls this as a callback, do dummy holds position of widget
**      param)
**
**************************************************************** */
static void begin_guest( dummy,isChUI )
void *dummy;
#ifdef GUI
Widget *isChUI;
#else
char *isChUI;
#endif
{
    char directory[BIG_STRING],dir_var[BIG_STRING],
         quota_var[BIG_STRING],granule_var[BIG_STRING],
         temp_var[BIG_STRING],tempdir[BIG_STRING],user_var[BIG_STRING];
    int pid;
    int i,j,k;


    make_new_dir( userpath,"guest",directory );
    make_new_dir( temppath,"temp",tempdir );

    strcpy( user_data_dir, directory );
    strcpy( user_temp_dir, tempdir );

    (void) sprintf( dir_var,"USRDATA_DIR=%s/",directory );
    (void) putenv( dir_var ); /* set user directory */

    (void) sprintf( temp_var,"GAEATMP_DIR=%s/",tempdir );
    (void) putenv( temp_var );

    (void) sprintf( quota_var,"DISK_ALLOC=%2.0f",DEFAULT_DISK_QUOTA );
    (void) putenv( quota_var );

    (void) sprintf( granule_var,"GRANULE_LIMIT_CAP=%d",
        granule_limit_cap );
    (void) putenv( granule_var );

    /* Version 2: set user type for client */
    (void) sprintf( user_var,"%s=%s",USER_TYPE,GUEST );
    (void) putenv( user_var );

    /* fork here */
    if ( (pid=fork())  ==  0 ) {
        /*  Logoff from the server and free up the query
            descriptor. */
        (void) ims_qiFreeDesc (qDesc);

        (void) ims_msgStructFree (msgDesc); /* Free msg desc */

        if ( isChUI  ==  NULL )
            (void) execl( chui_name,chui_name,NULL );
        else
            (void) execl( gui_name,gui_name,NULL );
        (void) ims_msg( msgDesc, IMS_ERROR, "fork failed: %d\n",errno );
        exit( 0 );
    }
    else
#ifndef GUI
        wait( &stat );

    remove_dir( directory );
    remove_dir( tempdir );

    exit( 0 );

#else
    {
        /* geez this is ugly. add a timeout to check every thiry secs */
        /* for ims exit. now i need all sorts of X defines,includes   */

        (void) strcpy( imsinfo.tempdir,tempdir );
        (void) strcpy( imsinfo.guestdir,directory );
        imsinfo.pid=pid;
        imsinfo.guest=True;

        /* add the timeout */
        XtAppAddTimeOut( app_context,(unsigned long) TIMEOUT,
						(XtTimerCallbackProc) waitforgui,
            &imsinfo );

#ifndef Motif1_1
        XtUnmapWidget( appshell );
        if ( isChUI  !=  NULL )
            XtUnmapWidget( (Widget) isChUI );
#else
        XtDestroyWidget( appshell );
        if ( isChUI  !=  NULL )
            XtDestroyWidget( appshell );
#endif
        show_start_window( XtDisplay( appshell ) );

        /*
        **  wait for the process to fininish
        */
        sleep( 5 );
        k = 0;
        while( k == 0 ){
            j = waitpid( pid, &i, WNOHANG );
            if(  j  !=  0  )  k = 1;
            else  sleep( 1 );
        }
        /*
        **  after v0 finished, delete the temp directory.
        */
        remove_dir( user_temp_dir );
        remove_dir( user_data_dir );
    }
#endif
    return;
}   /*  begin_guest  */


/* *************************************************************
**
**  subr check_profile -- checks user profile for changes and updates
**      db if needed.  exits after check.
**  For postIMS key changes, we need the key.
**
**************************************************************** */
static void check_profile( fname,
    lname,
    orig_key,
    tempdir,
    key )
char *fname,*lname,*orig_key,*tempdir,*key;
{


    /* remove temp dir */
    remove_dir( tempdir );

    /*
    **  used to do something here: check if first and last names
    **      changed and issue a message.  that was commented out.
    */

#ifndef GUI
    exit( 0 );
#endif
    return;
}   /*  check_profile   */


/* *************************************************************
**
**  subr exec_ims -- starts the ims system.
**  pass in first name, last name, orig_key, directory.
**  updates user name if changed while in IMS.
**  isNew  !=  0 user is new, create directory and user profile.
**  isChUI  !=  0, execute gui.
**  added key as parameter (needed for post IMS key update.
**
**************************************************************** */
#ifdef GUI
static void exec_ims( fname,lname,orig_key,dir,diskquota,isNew,isChUI,
          isStaff,key,win )
#else
static void exec_ims( fname,lname,orig_key,dir,diskquota,isNew,isChUI,
          isStaff,key )
#endif
char *fname,*lname,*orig_key,*dir;
float diskquota;
int isNew,isChUI,isStaff;
char *key; /* Version 2 */
#ifdef GUI
Widget win;  /* window to unmap before starting client */
#endif
{
    /* pathnames are static so environment does not lose these
        settings after return */
    static char dir_var[BIG_STRING],dir_var2[BIG_STRING],
        quota_var[BIG_STRING],granule_var[BIG_STRING],
        temp_var[BIG_STRING],tempdir[BIG_STRING],staff_var[BIG_STRING],
        filename[BIG_STRING];  /* patch 3 */
    extern int errno;
    int pid;
    char buffer[255]; /* Version 2 */
    int i,j,k;


    /* patch 3: remove usrdata_dir/order.gdb */
    (void) sprintf( filename,"%s/order.gdb",dir );

    if ( unlink( filename ) < 0 )
        if ( errno  !=  ENOENT ) /* we don't care if it's not there */
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Could not remove ${USRDATA_DIR}/order.gdb,"
                " error %d\n",errno );
    (void) sprintf( dir_var,"USRDATA_DIR=%s/",dir );
    (void) putenv( dir_var ); /* set user directory */

    make_new_dir( temppath,"temp",tempdir );
    (void) sprintf( temp_var,"GAEATMP_DIR=%s/",tempdir );
    (void) putenv( temp_var );

    strcpy( user_temp_dir, tempdir );
    user_data_dir[0] = '\0';

    (void) sprintf( dir_var2,"UserData=%s/",dir );
    (void) putenv( dir_var2 );

    (void) sprintf( quota_var,"DISK_ALLOC=%2.0f", diskquota );
    (void) putenv( quota_var );

    (void) sprintf( granule_var,"GRANULE_LIMIT_CAP=%d",
        granule_limit_cap );
    (void) putenv( granule_var );

    /* Version 2: add new values for user type variable */
    if ( isStaff  ==  0 ) { /* staff - define variable */
        (void) sprintf( staff_var,"%s=%s",USER_TYPE,STAFF );
        (void) putenv( staff_var );
    }
    else {
        (void) sprintf( staff_var,"%s=%s",USER_TYPE,REGISTERED );
        (void) putenv( staff_var );
    }

    if ( isNew  !=  0 ) { /* pass name to client via user profile ODL */


        (void) strcpy( profile.request.first_name, fname );
        (void) strcpy( profile.request.last_name, lname );
        /* Version 2: take serial number off of user_key */
        (void) strcpy( buffer,orig_key );
        (void) strcpy( profile.initial_user_key, strtok( buffer," " ) );
        /* ************************
        **    IK_PutProfile() tabken out here: this wrote tree
        **      to disk
        */
    }

    /* fork here */
    if ( (pid=fork())  ==  0 ) {
        /*  Logoff from the server and free up the query
            descriptor. */
        (void) ims_qiFreeDesc (qDesc);

        (void) ims_msgStructFree (msgDesc); /* Free msg desc */

        if ( isChUI  ==  1 )
            (void) execl( chui_name,chui_name,NULL );
        else  (void) execl( gui_name,gui_name,NULL );
        (void) ims_msg( msgDesc, IMS_ERROR, "fork failed: %d\n",errno );
        exit( 0 );
    }
    else
#ifndef GUI
        wait( &stat );

    /* Version 2: add key */
    check_profile( fname,lname,orig_key,tempdir,key ); /* patch 3 */
#else
    { /* geez this is ugly. add a timeout to check every thiry secs  */
        /* for ims exit. now i need all sorts of X defines, includes */

        /* initialize information record for timeout proc */
        (void) strcpy( imsinfo.fname,fname );
        (void) strcpy( imsinfo.lname,lname );
        (void) strcpy( imsinfo.orig_key,orig_key );
        (void) strcpy( imsinfo.tempdir,tempdir );
        (void) strcpy( imsinfo.key,key );
        imsinfo.pid=pid;
        imsinfo.guest=False;

        /* add the timeout */
        XtAppAddTimeOut( app_context,(unsigned long ) TIMEOUT,
						(XtTimerCallbackProc) waitforgui,
            &imsinfo );

#ifndef Motif1_1
        XtUnmapWidget( appshell );
        if ( win  !=  NULL )
        XtUnmapWidget( win );
        show_start_window( XtDisplay( appshell ) );
#else
        XtDestroyWidget( appshell );
        if ( win  !=  NULL )
            XtDestroyWidget( win );
#endif

        /*
        **  wait for the process to fininish
        */
        sleep( 5 );
        k = 0;
        while( k == 0 ){
            j = waitpid( pid, &i, WNOHANG );
            if(  j  !=  0  )  k = 1;
            else  sleep( 1 );
        }
        /*
        **  after v0 finished, delete the temp directory.
        */
        remove_dir( user_temp_dir );

    }
#endif
    return;
}   /*  exec_ims   */


/* *************************************************************
**
**  subr waitforgui -- work_proc to check to see if ims exited
**
**
**************************************************************** */
static void waitforgui( clientinfo,id )
struct clientinforec *clientinfo;
XtIntervalId *id;
{


    if ( waitpid( clientinfo->pid, NULL, WNOHANG)  ==  0 )
        /* continue to check until ims exits */
        XtAppAddTimeOut( app_context, TIMEOUT,
						  (XtTimerCallbackProc) waitforgui,
							clientinfo );
    else {
        if ( clientinfo->guest  ==  False )
            check_profile( clientinfo->fname,clientinfo->lname,
            clientinfo->orig_key,clientinfo->tempdir,
            clientinfo->key );/*Version 2:add key*/
        else  exit( 0 );
/*      exit( 0 );  not needed in patch 4 */
    }
    return;
}   /*  waitforgui  */


/* *************************************************************
**
**  subr exit_ims -- exit the program
**
**
**************************************************************** */
static void exit_ims()
{
    exit( 0 );
}   /*  exit_ims  */


/* *************************************************************
**
**  subr quit_ims -- quits the ims (with verify)
**  client is parent shell for dialog
**
**************************************************************** */
static void quit_ims( w,client,call )
Widget w,client;
XtPointer call;
{

    (void) questiondialog( client,"Do you want to exit the IMS system?",
        "Exit IMS", (XtPointer) exit_ims, (XtPointer) NULL,
        XmDIALOG_CANCEL_BUTTON );
    return;
}   /*  quit_ims  */


/* warn_guest -- warns user of guest limitations before starting ims */
/* client is shell of welcome window */

/* *************************************************************
**
**  subr warn_guest -- warns user of guest limitations before starting
**      ims.  client is shell of welcome window.
**
**************************************************************** */
static void warn_guest( w,client,call )
Widget w,client;
XtPointer call;
{

    (void) questiondialog( client,"As a GUEST user, any information\n"
        "saved while using the IMS system\n(e.g. User Profile, search "
        "criteria,\nresults information), will be DELETED\nafter you "
        "exit the IMS.\n\nTo retain these data you should\nregister "
        "as a new user.\n\nContinue?","Guest Information",
        (XtPointer) begin_guest, (XtPointer) client,
        XmDIALOG_CANCEL_BUTTON );
    return;
}   /*  warn_guest  */


/* *************************************************************
**
**  subr start_ims -- starts the ims session.
**  client is an info structure to be freed before starting.
**
**************************************************************** */
static void start_ims( w,info,call )
Widget w;
struct welcomerec *info;
XtPointer call;
{
    char *fname,*lname;


    /* get orig names to send to IMS client */
    XtVaGetValues( info->fname,XmNvalue,&fname,NULL );
    XtVaGetValues( info->lname,XmNvalue,&lname,NULL );

    /* check for staff login */
    exec_ims( fname,lname,info->encrypted_key,info->dir,info->diskquota,
         info->isNew  ==  True ? 1 : 0,0,strcmp( info->key,STAFF_KEY ),
         info->key,info->shell );

    /* Version 2: free structure after we're done (avoids memory leak)*/
    XtFree( (char *) info );

    exit(0);
}   /*  start_ims  */


/* *************************************************************
**
**  subr cancel_new_user -- removes directory if user cancels password
**      set.
**
**************************************************************** */
static void cancel_new_user( w,key_info,call )
Widget w;
struct keyrec *key_info;
XtPointer call;
{

    (void) rmdir( key_info->info->dir );
    closedialog( key_info->shell,key_info->shell, (XtPointer) NULL );
    return;
}   /*  cancel_new_user  */


/* *************************************************************
**  subr check_len -- checks length of entered user key.
**  Version 2:   if post-ims processing, also changes user name.
**
**  displays dialog box on error.
**  if ok and pass=0, prompt for re-entry.
**  if ok and pass=1, verify match and save, then start ims.
**
**************************************************************** */
static void check_len( w,key_info,client )
Widget w;
struct keyrec *key_info;
XtPointer client;
{
    char *key,*fname,*lname,dummy[BIG_STRING],thekey[KEY_LEN+1];
    Widget dialog; /* Version 2 */
    XmString thestring;  /* Version 2: for changing button label */
    int  status;
    float  f_temp;


    if ( key_info->pass  ==  0 )
        key=key_info->key1;
    else
        key=key_info->key2;

    if ( strlen( key ) < 6 ) {
        (void) pausedialog( key_info->shell,"Your user access key"
            " must be at least six characters long.",
            "Access Key Length",(XtPointer) NULL, (XtPointer) NULL);
        return;
    }

    if ( key_info->pass  ==  0 ) { /* 1st pass, need to verify entry */
        key_info->pass++;
        XtDestroyWidget( key_info->shell );
        get_user_key( NULL,key_info, (XtPointer) NULL );
        return;
    }

    /* 2nd pass, compare and update */
    if ( strcmp( key_info->key1,key_info->key2 )  !=  0 ) {
        /* no match, reset, warn user, and restart */
        key_info->pass=0;

        XtDestroyWidget( key_info->shell );
        (void) pausedialog( key_info->info->shell,
            "Your entries do not match. Please try again.",
            "Verification Failed", (XtPointer) get_user_key,
            (XtPointer) key_info );
        return;
    }

    /* make sure an entry does not already exist */
    /* Version 2: if post IMS processing, don't use widgets */
    /* Version 2: remove & from strings */
    if ( key_info->postIMS  ==  True ) {
        fname= (key_info->fname);
        lname= (key_info->lname);
        (void) strcpy( thekey,key_info->thekey );
    }
    else {
        XtVaGetValues( key_info->info->fname,XmNvalue,&fname,NULL );
        XtVaGetValues( key_info->info->lname,XmNvalue,&lname,NULL );
        (void) strcpy( thekey,key_info->info->key );
    }

    if ( find_user( fname,lname,key_info->key1,dummy,dummy,&f_temp )
        !=  IMS_OK ){/* warn user and restart */
        key_info->pass=0;

        XtDestroyWidget( key_info->shell );
        (void) pausedialog( key_info->info->shell,"Your access key is"
            " not valid.\nPlease choose another access key.",
            "Access Key In Use",  (XtPointer) get_user_key,
            (XtPointer) key_info );
        return;
    }

    /* entries ok, update and start ims */

    if ( key_info->new  ==  True ){
        status = add_user( fname, lname, key_info->key2,
            key_info->info->dir, key_info->info->encrypted_key );
        if(  status  <  IMS_OK ){
            (void) pausedialog( key_info->shell,"Ability to add user "
                "has not been implimented.\nTry again or contact"
                " operator.", "Cannot add user.", (XtPointer) NULL,
                (XtPointer) NULL );
            return;
        }
    }
    else{
        if ( key_info->postIMS  ==  True ) { /* Version 2 */
            status = update_key( key_info->old_fname,
                key_info->old_lname, thekey, key_info->key2 );
            if(  status  <  IMS_OK ){
                (void) pausedialog( key_info->shell,"Error in update "
                    "key.","Error in update key.", (XtPointer) NULL,
                    (XtPointer) NULL);
                return;
            }
            status =  update_name( key_info->old_fname,
                key_info->old_lname, key_info->orig_key,
                key_info->fname, key_info->lname );
            if(  status  <  IMS_OK ){
                (void) pausedialog( key_info->shell,
                    "Error in update name.",
                    "Error in update name.", (XtPointer) NULL,
                    (XtPointer) NULL);
                return;
            }
        }
        else{
            status = update_key( fname,lname,key_info->info->key,
                key_info->key2 );
            if(  status  <  IMS_OK ){
                (void) pausedialog( key_info->shell,
                    "Error in update key.",
                    "Error in update key.",  (XtPointer) NULL,
                    (XtPointer) NULL );
                return;
            }
        }
    }

    /* copy new key to info structure so start_ims can check for
        staff logins */
    (void) strcpy( key_info->info->key,key_info->key2 );

    XtDestroyWidget( key_info->shell );

    if ( key_info->postIMS  ==  True ) {  /* Version 2 */
        dialog=pausedialog( key_info->info->shell,
            "You have changed your name and access key.\nPlease use "
            "your new identification the next time \nyou login to "
            "the IMS.","Remember Your Key", (XtPointer) exit_ims,
            (XtPointer) NULL );
        thestring=XmStringCreateLtoR( "Exit IMS","shellfont" );
        XtVaSetValues( dialog,XmNokLabelString,thestring,NULL );
        XmStringFree( thestring );
    }
    else{
        /* Version 2: if user is new, tell them about user guide */
        if ( key_info->new  ==  True )
            (void) pausedialog( key_info->info->shell,
                "Please remember your access key.\nYou should use"
                " it the next time you use the IMS system.\n\nIf you"
                " have problems while running the IMS,\na user's guide"
                " is available via anonymous"
                " FTP\nfrom harp.gsfc.nasa.gov.",
                "Remember Your Key", (XtPointer) start_ims,
                (XtPointer) key_info->info );
        else
            (void) pausedialog( key_info->info->shell,"Please remember"
            " your access key.\n You should use it next time you use "
            "the IMS system.","Remember Your Key",
            (XtPointer) start_ims, (XtPointer) key_info->info );
    }
    return;
}   /*  check_len  */


/* *************************************************************
**
**  subr change_key -- begins process to change user key.
**  pass new=True if change is for a new user.
**
**************************************************************** */
static void change_key( info,new )
struct welcomerec *info;
Boolean new;
{
    struct keyrec *key_info;


    key_info=(struct keyrec *) XtMalloc( sizeof( struct keyrec ) );
    if ( key_info  ==  NULL )
        XtError( "Could not change access key. Insufficient memory." );
    key_info->new=new;
    key_info->pass=0;
    key_info->info=info;
    /* Version 2: turn off postIMS (this is pre-ims processing) */
    key_info->postIMS=False;

    get_user_key( NULL,key_info, (XtPointer) NULL );
    return;
}   /*  change_key  */


/* *************************************************************
**
**  subr reset_window -- resets text fields by moving cursor to home
**      and clearing access key field.
**
**************************************************************** */
static void reset_window( w,info,call )
Widget w;
struct welcomerec *info;
XtPointer call;
{
    int i,j;


    XtVaSetValues( info->fname,XmNcursorPosition,0,NULL );
    XtVaSetValues( info->lname,XmNcursorPosition,0,NULL );
    XmTextSetString( info->accesskey,"" );

    /* clear access key */
    j = strlen( info->key );
    for ( i=0; i  <  j ; i++ )
        info->key[i]='\0';

    XtDestroyWidget( info->dialog );
    return;
}   /*  reset_window  */


/* *************************************************************
**
**  subr validate_user -- uses find_user to validate user.
**  begins change key process if radio button has been selected.
**
**************************************************************** */
static void validate_user( w,info,call )
Widget w;
struct welcomerec *info;
XtPointer call;
{
    char *fname,*lname,directory[BIG_STRING],
        origkey[ENCRYPTED_KEY_LEN+1];
    int result;
    float diskquota;
    int  status;


    XtVaGetValues( info->fname,XmNvalue,&fname,NULL );
    XtVaGetValues( info->lname,XmNvalue,&lname,NULL );

    /* remove extra spaces */
    (void) trim( fname );
    (void) trim( lname );

    /* check for null entries */
    if ( fname[0]  ==  '\0' || lname[0]  ==  '\0' ) {
        XtFree( fname );
        XtFree( lname );

        (void) pausedialog( info->shell,"Entries for `First Name' and "
            "`Last Name' are required.","Names Required", NULL,
            NULL );
        return;
    }

#ifdef RSH
    /* for rsh database access, ; and ~ are special characters */
    /* and illegal in names */
    if ( strchr( fname,'~' )  !=  NULL || strchr( fname,';' )
        !=  NULL ||  strchr( lname,'~' )  !=  NULL ||
        strchr( lname,';' )  !=  NULL ||
        strchr( info->key,'~' )  !=  NULL || strchr( info->key,';' )
        !=  NULL ) {
        (void) pausedialog( info->shell,"Semicolons ( ; ) and tildes "
            "( ~ ) cannot be used\n in names or passwords.\nPlease "
            "correct or reenter information.","Bad Characters",
            (XtPointer) NULL, (XtPointer) NULL );
        return;
    }
#endif

    result = find_user( fname, lname, info->key, directory, origkey,
        &diskquota );

    switch ( result ) {
    case IMS_OK: /* user found, start ims */

        (void) strcpy( info->encrypted_key,origkey );
        (void) strcpy( info->dir,directory );
        info->diskquota=diskquota;

        status = touch_user( fname,lname,info->key );
            /* update last login */
        if(  status  <  IMS_OK ){
            (void) pausedialog( info->shell,"Error in user statistics.",
                "Error in DB", NULL,
                NULL );
            return;
        }

        /* does user want to change userkey? */
        if ( XmToggleButtonGetState( info->changeflag )  ==  True ) {
          change_key( info,False );
          return;
        }

        XtFree( fname );
        XtFree( lname );

        start_ims( (Widget) NULL,info, (XtPointer) NULL );
        break;

    case IMS_ERROR: /* information unknown */
        info->dialog=
            questiondialog_3( info->shell,
            "Could not find your name and access key\nin the list of "
            "users.\n\nStart the IMS as a Guest?",
            XmDIALOG_CANCEL_BUTTON,
            "OK", (XtPointer) begin_guest, (XtPointer) info->shell,
            "Cancel", (XtPointer) reset_window, (XtPointer) info,
            "Exit IMS", (XtPointer) quit_ims, (XtPointer) info->shell );
        break;
    case IMS_FATAL: /* access dir is not given */
        info->dialog=
            questiondialog_3( info->shell,
            "Access directory is void or does not exit\n"
            " for your name and access key in the user list.\n"
            "\nStart the IMS as a Guest?",
            XmDIALOG_CANCEL_BUTTON,
            "OK", (XtPointer) begin_guest, (XtPointer) info->shell,
            "Cancel", (XtPointer) reset_window, (XtPointer) info,
            "Exit IMS", (XtPointer) quit_ims, (XtPointer) info->shell );
        break;
    }
    return;
}   /*  validate_user  */


/* *************************************************************
**
**  subr hide_accesskey -- callback to hide user input when doing key
**      (init string to all null characters before using this routine)
**
**************************************************************** */
static void hide_accesskey( w,accesskey,call )
Widget w;
char *accesskey;
XmTextVerifyCallbackStruct *call;
{


    /* backspace key */
    if ( call->text->ptr  ==  NULL || *(call->text->ptr)  ==  '\0' ) {
        /* patch 3: */
        call->endPos=strlen( accesskey );
        accesskey[call->startPos]='\0';
    }
    else if ( call->text->length>1 ) /* disallow paste operations */
        call->doit=False;
    else {                       /* add text, blank output */
        accesskey[call->startPos] = *(call->text->ptr);
        *(call->text->ptr)='.';
    }
    return;
}   /*  hide_accesskey  */


/* *************************************************************
**
**  subr next_field -- advances cursor to the next field
**
**************************************************************** */
static void next_field( w,client,call )
Widget w;
XtPointer client,call;
{
    XmProcessTraversal( w,XmTRAVERSE_NEXT_TAB_GROUP );
    return;
}   /*  next_field  */


/* *************************************************************
**
**  subr start_new -- starts new user account creation.
**  client should be the shell of the button that called callback
**      or the child of that shell.
**
**************************************************************** */
static void start_new( w,client,call )
Widget w,client;
XtPointer *call;
{

    make_getuser( client,True,300,180,"Add New User" );
    return;
}   /*  start_new  */


/* *************************************************************
**
**  subr start_registered -- starts window to ask user for id.
**  client should be the shell fo the button that called callback
**      or the child of that shell.
**
**************************************************************** */
static void start_registered( w,client,call )
Widget w,client;
XtPointer call;
{
    make_getuser( client,False,300,260,"User Identification" );
    return;
}   /*  start_registered  */


/* *****   dialog.c -- routines to create informational dialogs */

/* *************************************************************
**
**  subr check_event -- checks for window update events and processes
**      them.  call during long busy spells to keep windows updated.
**
**************************************************************** */
static void  check_event( display )
Display *display;
{
    XEvent event;

    for ( ; XCheckMaskEvent( display,
        ColormapChangeMask | EnterWindowMask | LeaveWindowMask |
        FocusChangeMask | ExposureMask | ResizeRedirectMask |
        StructureNotifyMask | SubstructureNotifyMask |
        SubstructureRedirectMask,
        &event ) == True; )
        XtDispatchEvent( &event );
    return;
}   /*  check_event  */


/* *************************************************************
**
**  subr closedialog -- closes the dialog box.
**
**************************************************************** */
static void  closedialog( w,client,call )
Widget w,client;
XtPointer call;
{
    XtDestroyWidget( client );
    return;
}   /*  closedialog  */


/* *************************************************************
**
**  subr pausedialog -- shows a user an info dialog, then calls the
**      specified ok callback to continue processing
**
**************************************************************** */
static Widget pausedialog( parent,message,title,okcallback,okdata )
Widget parent;
char *message,*title;
XtPointer okcallback,okdata;
{
    XmString thestring;
    Widget dialog,p;
    Display *display;
    Arg arg[1];

    display=XtDisplay( parent );

    if ( XtIsShell( parent ) )
        p=parent;
    else
        p=XtParent( parent );

    XtSetArg( arg[0],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL );

    dialog=XmCreateMessageDialog( p,"dialog",arg,1 );
    thestring=XmStringCreateLtoR( message,"shellfont" );
    XtVaSetValues( dialog,
        XmNdialogType,XmDIALOG_INFORMATION,
        XmNmessageString,thestring,
        NULL );
    XmStringFree( thestring );

    XtManageChild( dialog );

    if ( title  ==  NULL )
        XtVaSetValues( XtParent( dialog ),XmNtitle,"Program Note",NULL);
    else
        XtVaSetValues( XtParent( dialog ),XmNtitle,title,NULL );

    XtUnmanageChild( XmMessageBoxGetChild( dialog,
        XmDIALOG_CANCEL_BUTTON ) );
    XtUnmanageChild( XmMessageBoxGetChild( dialog,XmDIALOG_HELP_BUTTON
        ) );

    XtAddCallback( dialog,XmNokCallback,
			(XtCallbackProc) closedialog,XtParent( dialog ));
    if ( okcallback  !=  NULL )
        XtAddCallback( dialog,XmNokCallback,
			(XtCallbackProc)	okcallback,okdata );

    XtPopup( XtParent( dialog ), XtGrabNone );
    return( dialog );
}   /*  pausedialog */


/* *************************************************************
**
**  subr close_dialog -- callback to close specified dialog.
**
**************************************************************** */
static void close_dialog( w,client,call )
Widget w,client;
XtPointer call;
{
    XtDestroyWidget( client );
    return;
}   /*  close_dialog  */


/* *************************************************************
**
**  subr questiondialog -- displays a question dialog.
**
**************************************************************** */
static Widget questiondialog( Widget parent, char *message, char *title,
    XtPointer okcallback, XtPointer clientdata,
    unsigned char defaultbutton )
{
    XmString thestring;
    Widget dialog,p;
    Display *display;
    Arg arg[1];


    display=XtDisplay( parent );

    if ( XtIsShell( parent ) )
        p=parent;
    else
        p=XtParent( parent );

    XtSetArg( arg[0],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL );
    dialog=XmCreateMessageDialog( p,"dialog",arg,1 );
    thestring=XmStringCreateLtoR( message,"shellfont" );
    XtVaSetValues( dialog,
        XmNdialogType,XmDIALOG_QUESTION,
        XmNmessageString,thestring,
        XmNdefaultButtonType,defaultbutton,
        NULL );
    XmStringFree( thestring );

    if ( title  ==  NULL )
        XtVaSetValues( XtParent( dialog ),
        XmNtitle,"Program Question",
        NULL );
    else
        XtVaSetValues( XtParent( dialog ),
        XmNtitle,title,
        NULL );

    XtManageChild( dialog );

    XtUnmanageChild( (Widget) XmMessageBoxGetChild( dialog,
        XmDIALOG_HELP_BUTTON ) );
    XtAddCallback( dialog,XmNokCallback,
				(XtCallbackProc) okcallback,clientdata );
    XtAddCallback( dialog,XmNokCallback,
				(XtCallbackProc) closedialog,XtParent( dialog ));

    XtPopup( XtParent( dialog ), XtGrabNone );
    return( dialog );
}   /* questiondialog  */


/* *************************************************************
**
**  subr questiondialog_3 -- same as above, but allows 3 options and
**      customizations of all options
**      use "" for buttonname to keep default
**      usr NULL for callback to use a close callback
** patch 2: returns a Widget, not an int. (SGI compiler would error)
**
**************************************************************** */
static Widget questiondialog_3( Widget parent, char * message,
        unsigned char defaultbutton, char * oklabel,
        XtPointer okcallback, XtPointer okdata,
        char * cancellabel, XtPointer cancelcallback,
        XtPointer canceldata, char * helplabel, XtPointer helpcallback,
        XtPointer helpdata )
{
    XmString thestring;
    Widget dialog,p;
    Display *display;


    display=XtDisplay( parent );

    if ( XtIsShell( parent ) )
        p=parent;
    else
        p=XtParent( parent );

    dialog=XmCreateMessageDialog( p,"dialog",NULL,0 );
    thestring=XmStringCreateLtoR( message,"shellfont" );
    XtVaSetValues( dialog,
       XmNdialogType,XmDIALOG_QUESTION,
       XmNmessageString,thestring,
       XmNdefaultButtonType,defaultbutton,
       XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL,
       NULL );
    XmStringFree( thestring );

    XtVaSetValues( XtParent( dialog ),
        XmNtitle,"Program Question",
        NULL );

    if ( strcmp( oklabel,"" )  !=  0 ) {
        thestring=XmStringCreateLtoR( oklabel,"shellfont" );
        XtVaSetValues( XmMessageBoxGetChild( dialog,XmDIALOG_OK_BUTTON),
            XmNlabelString,thestring,
            NULL );
        XmStringFree( thestring );
    }

    if ( strcmp( cancellabel,"" )  !=  0 ) {
        thestring=XmStringCreateLtoR( cancellabel,"shellfont" );
        XtVaSetValues( XmMessageBoxGetChild( dialog,
            XmDIALOG_CANCEL_BUTTON ),
            XmNlabelString,thestring,
            NULL );
        XmStringFree( thestring );
    }

    if ( strcmp( helplabel,"" )  !=  0 ) {
        thestring=XmStringCreateLtoR( helplabel,"shellfont" );
        XtVaSetValues( XmMessageBoxGetChild( dialog,
            XmDIALOG_HELP_BUTTON ),
            XmNlabelString,thestring,
            NULL );
        XmStringFree( thestring );
    }

    XtVaSetValues( XtParent( dialog ),
        XmNtitle,"Program Question",
        NULL );

    if ( okcallback  !=  NULL )
        XtAddCallback( dialog,XmNokCallback,
					(XtCallbackProc) okcallback,okdata );
    else
        XtAddCallback( dialog,XmNokCallback,
					(XtCallbackProc) close_dialog,
            XtParent( dialog ) );
    if ( cancelcallback  !=  NULL )
        XtAddCallback( dialog,XmNcancelCallback,
					(XtCallbackProc) cancelcallback,
            canceldata );
    else
        XtAddCallback( dialog,XmNcancelCallback,
					(XtCallbackProc) close_dialog,
            XtParent( dialog ) );
    if ( helpcallback  !=  NULL )
        XtAddCallback( dialog,XmNhelpCallback,
					(XtCallbackProc) helpcallback,helpdata );
    else
        XtAddCallback( dialog,XmNhelpCallback,
					(XtCallbackProc) close_dialog,
            XtParent( dialog ) );

    XtManageChild( dialog );
    XtPopup( XtParent( dialog ), XtGrabNone );

    return( dialog );
}   /* questiondialog_3  */


/* show watch as busy indicator -- routines follow */
/* *************************************************************
**
**  subr removeshell -- removes a shell from the list of shells.
**
**
**************************************************************** */
static void removeshell( w,client,call )
Widget w;
struct shellrec *client;
XtPointer call;
{
    struct shellrec *cur;

    if ( client  ==  shelllist ) {
        shelllist=shelllist->next;
        XtFree( (void *) client );
    }
    else {
        for ( cur=shelllist; cur  !=  NULL && cur->next  !=  client;
            cur=cur->next );
        if ( cur->next  ==  client ) {
            cur->next = client->next;
            XtFree( (void *) client );
        }
        else
            XtWarning(
                "Closed window not found in list of shell widgets." );
    }
    return;
}   /* removeshell */


/* *************************************************************
**
**  subr addshell -- adds a new shell to the current list of shells.
**  returns -1 if out of memory.
**
**************************************************************** */
static int addshell( shell )
Widget shell;
{
    struct shellrec *cur;


    cur=(struct shellrec *) XtMalloc( sizeof( struct shellrec ) );
    if ( cur  ==  NULL ) {
        XtWarning( "Could not create window. Out of memory." );
        return( -1 );
    }

    cur->shell=shell;
    cur->next=shelllist;
    shelllist=cur;

    XtAddCallback( shell,XmNdestroyCallback,
			(XtCallbackProc) removeshell,cur );
    return 0;
}   /* addshell */


/* *************************************************************
**
**  subr busy -- shows the busy indicator.
**
**************************************************************** */
static void busy( w )
Widget w;
{
    struct shellrec *cur;


    for ( cur=shelllist; cur  !=  NULL; cur=cur->next )
        XDefineCursor( XtDisplay( cur->shell ),XtWindow( cur->shell ),
        watch );
    check_event( XtDisplay( shelllist->shell ) );
    return;
}   /*  busy */


/* *************************************************************
**
**  subr ready -- clears the busy indicator
**
**************************************************************** */
static void ready( w )
Widget w;
{
    struct shellrec *cur;


    for ( cur=shelllist; cur  !=  NULL; cur=cur->next )
      XUndefineCursor( XtDisplay( cur->shell ),XtWindow( cur->shell ) );

    check_event( XtDisplay( shelllist->shell ) );
    return;
}   /*  ready  */


/* routines for eosdis logo display      */
/* *************************************************************
**
**  subr getlogosize -- returns size of the system logo
**
**************************************************************** */
static void getlogosize( width,height )
int *width,*height;
{
    int dummy, result;

    /* reset HDF image routines and get logo dimensions */
    DFR8restart();
    result = DFR8getdims(  logopath,width,height,&dummy );
    
}   /*  getlogosize  */


/* *************************************************************
**
**  subr loadsystemlogo -- loads the system logo into memory.
**
**************************************************************** */

/* jhd, 2/10/98  */
/*
 * Name: loadsystemlogo
 *
 * Description:
 *    loads the system logo into memory. Uses HDF library routines to read in
 *  the HDF image file. The 8-bit HDF image file is tranlated into the
 *  appropriate
 *  type of X-image.
 *
 * Arguments:  The motif drawing area widget logo.
 * Return Values: void
 *
 * Warnings: None
 *
 * Global Variables Used: none
 * Revision History:
 *    12/30/97 Modified by Phil Durbin. Added handling for multiple
 * visual types
 *  and bit depths.
 */


static void loadsystemlogo( logo )
Widget logo;

{ int result,dummy,colorxlate[256],numcolors,i,i2;
  char palette[768]; /*The pallete is an 8-bit RGB list of colors */
  unsigned long plane_mask[256],pixels[256];
  Colormap colormap;
  XColor color;
  XGCValues gcstuff;
  Visual *vis_info;
  int pixlen=1,zz=0;
  char *hdf_image; /* always 8-bit image */

  /*get the depth of display and calculate pixel length */
  pixlen= (PlanesOfScreen( XtScreen( logo ) )/8);
 
  if ( systemlogo.image!=NULL ){  /* only run ONCE! */
    return;
  }

  /* reset HDF image routines and get logo dimensions */
  DFR8restart();
  result=DFR8getdims( logopath,&systemlogo.width,&systemlogo.height,
		     &dummy );
  if ( result==-1 ) {
    XtWarning( "Could not read system logo file." );
    return;
  }

  /* calculate the size of memory necessary based upon image height,width and   pixel size */
  systemlogo.image=XtMalloc( systemlogo.width * systemlogo.height*pixlen );
  /* calculate the temporary HDF image using width and depth; pixel length
 is implied 1 byte  (8-bit color) */
  hdf_image= XtMalloc( systemlogo.width * systemlogo.height );
  if ( systemlogo.image == NULL ) {
    XtWarning( "Could not display system logo. Out of memory." );
    return;
  }


  result=DFR8getimage( logopath,hdf_image,systemlogo.width,
		      systemlogo.height,&palette );
  if ( result==-1 ) {
    XtWarning( "Could not read system logo file." );
    XtFree( systemlogo.image );
    return;
  }

  /* get color palette */
  DFPreadref( logopath,0 );
  DFPgetpal( logopath,palette );
  
  /* scan image for pixel values */
  for ( i=0; i!=256; i++ )
    colorxlate[i]=-1;

  for ( i=0; i!=systemlogo.width * systemlogo.height; i++ ) 
    colorxlate[*(((char *)hdf_image+i))]=0;

  for ( numcolors=0,i=0; i!=256; i++ )
    if ( colorxlate[i]==0 )
      numcolors++; 

  /* get the standard colormap */
  colormap=DefaultColormapOfScreen( XtScreen( logo ) );
  
  /* allocate colors in palette and remap colors in image to
   * newly allocated pixel values for the X color map. Note
   * pallette is a 8-bit colors, RGB.
   */

  for ( i=0; i!=256; i++ )
    if ( colorxlate[i]==0 ) {
      color.flags=DoRed | DoGreen | DoBlue;
      /* each color is 3 bytes */ 
     color.red=(unsigned short)  *(palette+i*3) << 8;  
     color.green=(unsigned short)  *(palette+i*3+1) << 8;
     color.blue=(unsigned short) *(palette+i*3+2) << 8;
     
     /* On failure print  a warning  message, no need to stop though
     * The image is not that important.
     */
     if (XAllocColor( XtDisplay( logo ),colormap,&color )==0) printf("Unable to allocate color!\n");
      else {
 	colorxlate[i]=(int) color.pixel; /* store into translation table */
     }

    }

  /* now, using pixel translation map, map pixel values from HDF palette to 
   * X11 colormap translating the pixel size where appropriate and loading
   * image date file. 
   */
  for ( i=0; i!=(systemlogo.width * systemlogo.height); i++ ) {
    int k=i*pixlen;
    int tmp=0;
    unsigned short short_pix;
    tmp = (int) hdf_image[i];
    write_pixel(colorxlate[tmp],pixlen,&(systemlogo.image[k]));
  }

  free(hdf_image);

  /* create the image */
  systemlogo.ximage=XCreateImage( XtDisplay( logo ),
				 DefaultVisualOfScreen( XtScreen( logo ) ),
				 PlanesOfScreen( XtScreen( logo ) ),
				 ZPixmap,0,systemlogo.image,systemlogo.width,
				 systemlogo.height,8,0 ); 
  /* image load complete */
  /*Ensure that the Byte order is MSBFirst. (in 16 bit visuals this is
 not alwas the case! */
systemlogo.ximage->byte_order=MSBFirst;

  /* create the graphics context used for updates */
  gcstuff.function=GXcopy;
  systemlogo.gc=XCreateGC( XtDisplay( logo ),XtWindow( logo ),
			  GCFunction,&gcstuff );

}

/*
 * Name: drawlogo
 *
 * Description: 
 *   callback for update events. Draws logo to screen.
 *
 * Arguments:  widget and 2 XtPointers. Standard Xt callback.
 * Return Values: 
 *
 * Warnings: None
 *
 * Global Variables Used: none
 * Revision History:
 *   
 */
static void drawlogo( w,client,call )
Widget w;
XtPointer client,call;

{
  if ( XtIsRealized( w ) ) {
    XPutImage( XtDisplay( w ),XtWindow( w ),
	      systemlogo.gc,
	      systemlogo.ximage,
	      0,0,0,0,
	      systemlogo.width,
	      systemlogo.height );
  }
}


/* 
 * Name: write_pixel
 *
 * Description: 
 * Write a pixel value (int) to the appropriate location and
 * pixel size (int,3 byte, 2 -byte, or byte). Byte order is always 
 * MSBFirst.
 * This code was Gstolen from the loadimage2 library, value.c, doValToMem() 
 * function.
 *
 * Basically this copies the appropriate size pixel value (pvalue) into
 * the memory
 * area pointed to by image_loc. Although pvalue is an int, the target data may
 * be smaller.
 *
 * This one deserves a bit of explaining; We loop the number of bytes in the
 * size  of the target memory location (pix_size). For each byte we mask the 
 * pvalue with 0xff. This copies only the last byte in the integer pvalue and
 * assigns it to last untouched byte in image_loc. We shift pvalue  by 1 byte
 * , putting a new byte value in the last byte of pvalue. repeat until no
 * more
 *  bytes. 
 *
 * Arguments:  Pixel pvalue, int pix_size, char *image_loc
 * Return Values: 0 on success .
 *
 * Warnings: None
 *
 * Global Variables Used: none
 * Conditions:
 *      Precondition  1:
 *            The values are legitimate and space has been allocated for   
 *            image_loc. MSBFirst is assumed for the image byte order.
 *      Postcondition 1:
 *            The appropriate sized pixel value is placed into image_loc
 *
 * Revision History:
 *    12/30/97 Created from loadimage2 library, value.c, doValToMem() by Phil Durbin.
 */
 int write_pixel(Pixel pvalue, int pix_size, char *image_loc)
{ int a;

  for (a= pix_size - 1; a >= 0; a--) {
    *(image_loc + a)= pvalue & 0xff;
    pvalue >>= 8;
  }
  return(pvalue);

}

/*
 * Name: determine_Endianess()
 *
 * Description: 
 * Determine if cpu uses MSBFirst or LSBFirst; 
 * MSBFirst - Most signinficant byte first (Big endian systems usually)
 * LSBFirst - least significant byte first (little endian systems usually)
 * I don't know how portable this code is!
 *
 * Arguments:  None
 * Return Values: 0 on success .
 *
 * Warnings: None
 *
 * Global Variables Used: none
 * Revision History:
 *    12/30/97 Created by Phil Durbin.
 */
  int determine_Endianess()
{
  union { int integer_buf; char char_buf[sizeof(int)]; } buf;
  int last=sizeof(int)-1;  

  buf.integer_buf=1;

  /* if little endian (MSBFirst) char_buf[last] is 0, and if 1 LSBFirst . */
  if  (buf.char_buf[last]==1) return LSBFirst; 
  else if (buf.char_buf[last]==0) return MSBFirst;
  else return -1; /* Return unkown Endianess */

 
}


/* window.c -- window for asking for access key */
/* *************************************************************
**
**  subr close_start --sets startbox global to NULL when ims start
**      window is closed
**
**************************************************************** */
static void close_start( w,client,call )
Widget w;
XtPointer client,call;
{
    startbox=NULL;
    return;
}   /*  close_start  */


/* *************************************************************
**
**  subr show_start_window -- displays a window indicating ims will
**      be starting
**
**************************************************************** */
static void show_start_window( display )
Display *display;
{
    Widget shell,masterform,w,button;
    XmString thestring;


    shell=XtVaAppCreateShell( "ims","Access",
        topLevelShellWidgetClass,
        display,
        XmNiconName,"EOSDIS V0 IMS Start",
        XmNtitle,"EOSDIS V0 IMS Start",
        XmNbaseWidth,300,
        XmNbaseHeight,100,
        NULL );
    startbox=shell; /* patch 4 */

    /*  Motif1_1: avoid warnings by waiting to manage from until */
    /*            children are created. */
#ifndef Motif1_1
    masterform=XtVaCreateManagedWidget( (void *) shell,xmFormWidgetClass,
					(void *) shell,
#else
    masterform=XtVaCreateWidget( (void *) shell,xmFormWidgetClass,
					(void *) shell,
#endif
        XmNheight,100,
        XmNwidth,300,
        NULL );

    thestring=XmStringCreateLtoR( "Close","shellfont" );
    button=XtVaCreateManagedWidget( "close",xmPushButtonWidgetClass,
        masterform,XmNlabelString,thestring,
#ifdef Motif1_1
        XmNtopAttachment,XmATTACH_OPPOSITE_FORM,
        XmNtopOffset, -10-ITEMHEIGHT,
#endif
        XmNleftAttachment,XmATTACH_POSITION,
        XmNleftPosition,40,
        XmNrightAttachment,XmATTACH_POSITION,
        XmNrightPosition,60,
        XmNbottomAttachment,XmATTACH_FORM,
        XmNbottomOffset,10,
        NULL );

    XtAddCallback( button,XmNactivateCallback,
				(XtCallbackProc) close_dialog,shell );
    XtAddCallback( button,XmNactivateCallback,
				(XtCallbackProc) close_start,shell );
    thestring=XmStringCreateLtoR( "Starting the IMS.\n(Please wait.)",
        "shellfont" );

    w=XtVaCreateManagedWidget( "label",xmLabelWidgetClass,masterform,
        XmNlabelString,thestring,
            /*
        XmNtopAttachment,XmATTACH_WIDGET,
            */
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,10,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,10,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
#ifdef Motif1_1
        XmNbottomAttachment,XmATTACH_WIDGET,
        XmNbottomOffset,10,
        XmNbottomWidget,button,
#endif
        NULL );

    /*  Motif 1.1 -- manage form now that children are created. */
#ifdef Motif1_1
    XtManageChild( masterform );
#endif

    XtRealizeWidget( shell );
    return;
}   /*   show_start_window  */


/* *************************************************************
**
**  subr get_user_key -- creates dialog shell for requesting user key
**
**************************************************************** */
static void get_user_key( w,key_info,call )
Widget w;
struct keyrec *key_info;
XtPointer call;
{
    Widget topform,label,ok,cancel,msglabel,buttonbox;
    XmString thestring;
    int i,height,width;
    XmFontList fontlist;
    char title[255];


    if ( key_info->pass  ==  0 ) {
    height=205;
    width=435;
    }
    else {
        height=135;
        width=250;
    }

    if ( key_info->new  ==  False )
        (void) strcpy( title,"Change Access Key" );
    else
        (void) strcpy( title,"Add Access Key" );

    key_info->shell=XtVaCreatePopupShell( "keyshell",
        xmDialogShellWidgetClass,
        key_info->info->shell,
        XmNtitle,title,
        XmNbaseHeight,height,
        XmNbaseWidth,width,
        NULL );

    /* capture close for new users so we can cleanup their directory */
    if ( key_info->new  ==  True ) {
        XtVaSetValues( key_info->shell,XmNdeleteResponse,XmDO_NOTHING,
            NULL );
        XmAddWMProtocolCallback( key_info->shell,
            XmInternAtom( XtDisplay( key_info->shell ),
            "WM_DELETE_WINDOW", False ),
            cancel_new_user,
            key_info );
    }
    if ( key_info->pass  ==  0 )
        thestring=XmStringCreateLtoR( "In order to access the Version"
            " 0 IMS from this computer,\nan access key is required. "
            "The key can be any combination\nof characters at least 6 "
            "characters long. (Only the first\n12 characters entered "
            "will be used.)\n\nEnter your new access key.","shellfont");
    else
        thestring=XmStringCreateLtoR( "Reenter your new access key.",
            "shellfont" );

    /* Version 2: Avoid Motif 1.1 form warnings */
#ifndef Motif1_1
    topform=XtVaCreateManagedWidget( "keyform",xmFormWidgetClass,
        key_info->shell,
#else
    topform=XtVaCreateWidget( "keyform",xmFormWidgetClass,
        key_info->shell,
#endif
        XmNdialogStyle,
        XmDIALOG_FULL_APPLICATION_MODAL,
        XmNheight,height,
        XmNwidth,width,
        XmNautoUnmanage,False,
        NULL );

  msglabel=XtVaCreateWidget( "keylabel",xmLabelWidgetClass,topform,
        XmNlabelString,thestring,
        XmNalignment,XmALIGNMENT_BEGINNING,
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,10,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,10,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        NULL );
#ifdef Motif1_1
    /* set height of label */
    XtVaGetValues( msglabel,XmNfontList,&fontlist,NULL );
    XtVaSetValues( msglabel,
        XmNbottomAttachment,XmATTACH_OPPOSITE_FORM,
        XmNbottomOffset,-10-XmStringHeight( fontlist,thestring )-5,
        NULL );
#endif
    XtManageChild( msglabel );
    XmStringFree( thestring );

    thestring=XmStringCreateLtoR( "Access Key:","shellfont" );
    label=XtVaCreateWidget( "key_label",xmLabelWidgetClass,topform,
        XmNlabelString,thestring,
        XmNalignment,XmALIGNMENT_BEGINNING,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,msglabel,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,10,
#ifdef Motif1_1
        XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET,
        XmNbottomOffset,-10-ITEMHEIGHT,
        XmNbottomWidget,msglabel,
#endif
        NULL );
#ifdef Motif1_1
    XtVaGetValues( label,XmNfontList,&fontlist,NULL );
    XtVaSetValues( label,
        XmNrightAttachment,XmATTACH_OPPOSITE_FORM,
        XmNrightOffset,-10-XmStringWidth( fontlist,thestring ),
        NULL );
    XtManageChild( label );
#endif
    XmStringFree( thestring );

    key_info->key=XtVaCreateManagedWidget( "keyfield",xmTextWidgetClass,
        topform,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,msglabel,
        XmNleftAttachment,XmATTACH_WIDGET,
        XmNleftWidget,label,
        XmNleftOffset,10,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
#ifdef Motif1_1
        XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET,
        XmNbottomOffset,-10-ITEMHEIGHT,
        XmNbottomWidget,msglabel,
#endif
        NULL );

    XtAddCallback( key_info->key,XmNactivateCallback,
				(XtCallbackProc) next_field,NULL );
    if ( key_info->pass  ==  0 ) {
        XtAddCallback( key_info->key,XmNmodifyVerifyCallback,
         (XtCallbackProc) hide_accesskey,key_info->key1 );
        for ( i=0; i  !=  KEY_LEN+1; i++ )
            key_info->key1[i]='\0';
    }
    else {
        XtAddCallback( key_info->key,XmNmodifyVerifyCallback,
         (XtCallbackProc) hide_accesskey, key_info->key2 );
        for ( i=0; i  !=  KEY_LEN+1; i++ )
            key_info->key2[i]='\0';
    }

    buttonbox=XtVaCreateManagedWidget( "buttonbox",xmFormWidgetClass,
        topform,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,10,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,key_info->key,
        XmNbottomAttachment,XmATTACH_FORM,
        XmNbottomOffset,10,
        NULL );

    thestring=XmStringCreateLtoR( "OK","shellfont" );
    ok=XtVaCreateManagedWidget( "okbutton",xmPushButtonWidgetClass,
        buttonbox,
        XmNlabelString,thestring,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,0,
        XmNrightAttachment,XmATTACH_POSITION,
        XmNrightPosition,25,
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,0,
#ifdef Motif1_1
        XmNbottomAttachment,XmATTACH_FORM,
        XmNbottomOffset,10,
#endif
        NULL );
    XmStringFree( thestring );
    XtAddCallback( ok,XmNactivateCallback,
			(XtCallbackProc) check_len,key_info );

    thestring=XmStringCreateLtoR( "Cancel","shellfont" );
    cancel=XtVaCreateManagedWidget( "cancelbutton",
        xmPushButtonWidgetClass,
        buttonbox,
        XmNlabelString,thestring,
        XmNleftAttachment,XmATTACH_POSITION,
        XmNleftPosition,75,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,0,
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,0,
#ifdef Motif1_1
        XmNbottomAttachment,XmATTACH_FORM,
        XmNbottomOffset,10,
#endif
        NULL );
    XmStringFree( thestring );
    if ( key_info->new  ==  False )
        XtAddCallback( cancel,XmNactivateCallback,
					(XtCallbackProc) close_dialog,
        key_info->shell );
    else
        XtAddCallback( cancel,XmNactivateCallback,
					(XtCallbackProc) cancel_new_user,
            key_info );

    /* Version 2: Avoid Motif 1.1 form warnings */
#ifdef Motif1_1
    XtManageChild( topform );
#endif

    XtPopup( key_info->shell,XtGrabNone );
    return;
}   /* get_user_key */


/* *************************************************************
**
**  subr make_getuser -- creates the user info window
**  if newuser is true, a new user window is created,
**      otherwise a registered user window is created
**
**  Version 2: title variable used both as char * and Widget.
**  Fix by using Widget title and char *wintitle
**************************************************************** */
static void make_getuser( Widget parent, Boolean newuser, int width,
    int height, char * wintitle )
{
    XmString thestring;
    Widget topform,label,button,buttonbox,title; /* Version 2 */
    int i;
    struct welcomerec *startinfo;
    Display *display;
    char msg[2048];


    display=XtDisplay( parent );

    startinfo=(struct welcomerec *) XtMalloc( sizeof( struct
        welcomerec ) );
    if ( startinfo  ==  NULL )
        XtError( "Could not allocate memory for IMS startup." );

    if ( newuser  ==  True ) {
        /*  Changed original code to disallow auto reg of new users -
            mmk(2/1/1995)
        Distributed code follows:
        _* set key so OK callback knows its a new user *_
        */
        (void) strcpy( startinfo->key,"new" );
        startinfo->isNew=True;
        /*  Site specific code follows:
        */
        (void) strcpy( msg,"Automatic registration is not supported "
            "at the Alaska\n" );
        (void) strcat( msg,"SAR Facility DAAC. Please contact ASF User "
            "Services at\n" );
        (void) strcat( msg,"907-474-6166 or uso@eosims.asf.alaska.edu "
            "for more\n" );
        (void) strcat( msg,"information, or try another DAAC.\n" );

        (void) pausedialog( parent,msg, NULL, NULL, NULL );

        startinfo->isNew=True;
        return;
    }

    /* create a shell*/
    startinfo->shell=XtVaCreatePopupShell( "usershell",
        xmDialogShellWidgetClass,
        /* should use XtParent(parent) instead of parent widget -
            Jennifer Ting */
        XtParent(parent),
        XmNheight,height,
        XmNwidth,width,
        XmNtitle,wintitle, /* Version 2 */
        XmNdeleteResponse,XmDO_NOTHING,
        NULL );

    XtRealizeWidget( startinfo->shell );

    XmAddWMProtocolCallback( startinfo->shell,
        XmInternAtom( display,"WM_DELETE_WINDOW",False ),
        quit_ims,
        parent );

    if ( addshell( startinfo->shell )  ==  -1 )
        XtError( "Could not allocate memory for IMS startup." );

    /* Version 2: Avoid Motif 1.1 form warnings */
#ifndef Motif1_1
    topform=XtVaCreateManagedWidget( "topform",xmFormWidgetClass,
#else
    topform=XtVaCreateWidget( "topform",xmFormWidgetClass,
#endif
        startinfo->shell,
        XmNheight,height,
        XmNwidth,width,
        XmNdialogStyle,
        XmDIALOG_FULL_APPLICATION_MODAL,
        NULL );

    thestring=XmStringCreateLtoR( "Please enter your identification:",
        "shellfont" );
    title=XtVaCreateManagedWidget( "usertitle",xmLabelWidgetClass,
        topform,
        XmNlabelString,thestring,
        XmNalignment,XmALIGNMENT_CENTER,
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,10,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,10,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        XmNalignment,XmALIGNMENT_CENTER,
#ifdef Motif1_1
        XmNbottomAttachment,XmATTACH_OPPOSITE_FORM,
        XmNbottomOffset,-ITEMHEIGHT-10,
#endif
        NULL );
    XmStringFree( thestring );

    thestring=XmStringCreateLtoR( "First Name:","shellfont" );
    label=XtVaCreateManagedWidget( "fname_label",xmLabelWidgetClass,
        topform,
        XmNlabelString,thestring,
        XmNwidth, 90,
        XmNalignment,XmALIGNMENT_BEGINNING,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,title,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,10,
#ifdef Motif1_1
        XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET,
        XmNbottomOffset,-10-ITEMHEIGHT,
        XmNbottomWidget,title,
        XmNrightAttachment,XmATTACH_POSITION,
        XmNrightPosition,33,
#endif
        NULL );
    XmStringFree( thestring );

    startinfo->fname=XtVaCreateManagedWidget( "fname",xmTextWidgetClass,
        topform,
        XmNmaxLength,FNAME_LEN,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopWidget,title,
        XmNtopOffset,10,
        XmNleftAttachment,XmATTACH_WIDGET,
        XmNleftOffset,10,
        XmNleftWidget,label,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
#ifdef Motif1_1
        XmNbottomAttachment,
        XmATTACH_OPPOSITE_WIDGET,
        XmNbottomOffset,-ITEMHEIGHT-10,
        XmNbottomWidget,title,
#endif
        NULL );
    XtAddCallback( startinfo->fname,XmNactivateCallback,
				(XtCallbackProc) next_field,NULL
        );
    thestring=XmStringCreateLtoR( "Last Name:","shellfont" );
    label=XtVaCreateManagedWidget( "lname_label",xmLabelWidgetClass,
        topform,
        XmNlabelString,thestring,
        XmNwidth, 90,
        XmNalignment,XmALIGNMENT_BEGINNING,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopWidget,startinfo->fname,
        XmNtopOffset,10,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,10,
#ifdef Motif1_1
        XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET,
        XmNbottomOffset,-10-ITEMHEIGHT,
        XmNbottomWidget,startinfo->fname,
        XmNrightAttachment,XmATTACH_POSITION,
        XmNrightPosition,33,
#endif
        NULL );
    XmStringFree( thestring );

    startinfo->lname=XtVaCreateManagedWidget( "lname",xmTextWidgetClass,
        topform,
        XmNmaxLength,LNAME_LEN,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,startinfo->fname,
        XmNleftAttachment,XmATTACH_WIDGET,
        XmNleftOffset,10,
        XmNleftWidget,label,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
#ifdef Motif1_1
        XmNbottomAttachment,
        XmATTACH_OPPOSITE_WIDGET,
        XmNbottomOffset,-10-ITEMHEIGHT,
        XmNbottomWidget,startinfo->fname,
#endif
        NULL );
    XtAddCallback( startinfo->lname,XmNactivateCallback,
				(XtCallbackProc) next_field,NULL
        );

    if ( newuser  ==  False ) {   /* ask old users for key */
        thestring=XmStringCreateLtoR( "Access Key:","shellfont" );
        label = XtVaCreateManagedWidget( "access_label",
            xmLabelWidgetClass,topform,
            XmNlabelString,thestring,
            XmNalignment,XmALIGNMENT_BEGINNING,
            XmNwidth, 90,
            XmNtopAttachment,XmATTACH_WIDGET,
            XmNtopWidget,startinfo->lname,
            XmNtopOffset,10,
            XmNleftAttachment,XmATTACH_FORM,
            XmNleftOffset,10,
#ifdef Motif1_1
            XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET,
            XmNbottomOffset,-10-ITEMHEIGHT,
            XmNbottomWidget,startinfo->lname,
            XmNrightAttachment,XmATTACH_POSITION,
            XmNrightPosition,33,
#endif
            NULL );
        XmStringFree( thestring );

        startinfo->accesskey=
            XtVaCreateManagedWidget( "key",xmTextWidgetClass,
            topform,
            XmNmaxLength,KEY_LEN,
            XmNtopAttachment,XmATTACH_WIDGET,
            XmNtopOffset,10,
            XmNtopWidget,startinfo->lname,
            XmNleftAttachment,XmATTACH_WIDGET,
            XmNleftOffset,10,
            XmNleftWidget,label,
            XmNrightAttachment,XmATTACH_FORM,
            XmNrightOffset,10,
#ifdef Motif1_1
            XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET,
            XmNbottomOffset,-10-ITEMHEIGHT,
            XmNbottomWidget,startinfo->lname,
#endif
            NULL );
        XtAddCallback( startinfo->accesskey,XmNactivateCallback,
            (XtCallbackProc) next_field,NULL );
        XtAddCallback( startinfo->accesskey,XmNmodifyVerifyCallback,
            (XtCallbackProc) hide_accesskey,startinfo->key );
        for ( i=0; i  !=  KEY_LEN+1; i++ )
            startinfo->key[i]='\0';

        thestring=XmStringCreateLtoR( "Change access key after "
             "validation","shellfont" );
        startinfo->changeflag=
        XtVaCreateManagedWidget( "changeflag",
            xmToggleButtonWidgetClass,
            topform,
            XmNlabelString,thestring,
            XmNalignment,XmALIGNMENT_BEGINNING,
            XmNtopAttachment,XmATTACH_WIDGET,
            XmNtopOffset,10,
            XmNtopWidget,startinfo->accesskey,
            XmNleftAttachment,XmATTACH_FORM,
            XmNleftOffset,10,
            XmNrightAttachment,XmATTACH_FORM,
            XmNrightOffset,10,
#ifdef Motif1_1
            XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET,
            XmNbottomOffset,-10-ITEMHEIGHT,
            XmNbottomWidget,startinfo->accesskey,
#endif
            NULL );
        XmStringFree( thestring );
    }

    /* buttons: use additional form for size */
    if ( newuser  ==  False )
        buttonbox=XtVaCreateManagedWidget( "buttonbox",
        xmFormWidgetClass,topform,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,startinfo->changeflag,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,10,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        XmNbottomAttachment,XmATTACH_FORM,
        XmNbottomOffset,10,
        NULL );
    else
        buttonbox=XtVaCreateManagedWidget( "buttonbox",
            xmFormWidgetClass,topform,
            XmNtopAttachment,XmATTACH_WIDGET,
            XmNtopOffset,10,
            XmNtopWidget,startinfo->lname,
            XmNleftAttachment,XmATTACH_FORM,
            XmNleftOffset,10,
            XmNrightAttachment,XmATTACH_FORM,
            XmNrightOffset,10,
            XmNbottomAttachment,XmATTACH_FORM,
            XmNbottomOffset,10,
            NULL );

    thestring=XmStringCreateLtoR( "OK","shellfont" );
    button=XtVaCreateManagedWidget( "userok",
        xmPushButtonWidgetClass,buttonbox,
        XmNlabelString,thestring,
        XmNleftAttachment,XmATTACH_POSITION,
        XmNleftPosition,0,
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,0,
        XmNrightAttachment,XmATTACH_POSITION,
        XmNrightPosition,30,
        XmNbottomAttachment,XmATTACH_FORM,
        XmNbottomOffset,10,
        NULL );
    XmStringFree( thestring );
    XtAddCallback( button,XmNactivateCallback,
			(XtCallbackProc) validate_user,startinfo );

    thestring=XmStringCreateLtoR( "Cancel","shellfont" );
    button=XtVaCreateManagedWidget("canceluser",xmPushButtonWidgetClass,
        buttonbox,
        XmNlabelString,thestring,
        XmNleftAttachment,XmATTACH_POSITION,
        XmNleftPosition,33,
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,0,
        XmNrightAttachment,XmATTACH_POSITION,
        XmNrightPosition,63,
        XmNbottomAttachment,XmATTACH_FORM,
        XmNbottomOffset,10,
        NULL );
    XmStringFree( thestring );
    XtAddCallback( button,XmNactivateCallback,
				(XtCallbackProc) close_dialog,
        startinfo->shell );

    thestring=XmStringCreateLtoR( "Exit","shellfont" );
    button=XtVaCreateManagedWidget( "exit",xmPushButtonWidgetClass,
        buttonbox,
        XmNlabelString,thestring,
        XmNleftAttachment,XmATTACH_POSITION,
        XmNleftPosition,66,
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,0,
        XmNrightAttachment,XmATTACH_POSITION,
        XmNrightPosition,100,
        XmNbottomAttachment,XmATTACH_FORM,
        XmNbottomOffset,10,
        NULL );
    XmStringFree( thestring );
    XtAddCallback( button,XmNactivateCallback,
				(XtCallbackProc) quit_ims,startinfo->shell
        );
#ifndef Motif1_1
    XtVaSetValues( topform,XmNinitialFocus,startinfo->fname,NULL );
#else
    /* Version 2: Avoid Motif 1.1 form warnings */
    XtManageChild( topform );
#endif
    XtPopup( startinfo->shell,XtGrabNone );
    return;
}   /*  make_getuser  */


/* *************************************************************
**
**  subr make_welcome - creates the welcome window
**
**************************************************************** */
static void make_welcome( parent )
Widget parent;
{
    XmString thestring;
    Widget topform,logo,title,label,guest,user,new,exit;
    int width,height;
    Display *display;


    display=XtDisplay( parent );

    /* capture window manager close. */
    XtVaSetValues( parent,XmNdeleteResponse,XmDO_NOTHING,NULL );
    XmAddWMProtocolCallback( parent,
        XmInternAtom( display,"WM_DELETE_WINDOW",False ),
        quit_ims,
        parent );

    if ( addshell( parent )  ==  -1 )
        XtError( "Could not allocate memory for IMS startup." );

    /* Version 2: Avoid Motif 1.1 form warnings */
#ifndef Motif1_1
    topform=XtVaCreateManagedWidget( "topform",xmFormWidgetClass,
        parent,NULL );
#else
    topform=XtVaCreateWidget( "topform",xmFormWidgetClass,parent,NULL );
#endif

    getlogosize( &width,&height );

    logo=XtVaCreateManagedWidget( "logo",xmDrawingAreaWidgetClass,
        topform,
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,10,
        XmNleftAttachment,XmATTACH_FORM,
        XmNleftOffset,10,
        XmNheight,height,
        XmNwidth,width,
        NULL );

    thestring=XmStringCreateLtoR( gui_welcome_message,"titlefont" );
    title=XtVaCreateManagedWidget( "title",xmLabelWidgetClass,topform,
        XmNlabelString,thestring,
        XmNalignment,XmALIGNMENT_CENTER,
        XmNtopAttachment,XmATTACH_FORM,
        XmNtopOffset,10,
        XmNleftAttachment,XmATTACH_WIDGET,
        XmNleftOffset,10,
        XmNleftWidget,logo,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        XmNalignment,XmALIGNMENT_CENTER,
        NULL );
    XmStringFree( thestring );

    thestring=XmStringCreateLtoR( "Press a button to start:",
        "shellfont" );
    label=XtVaCreateManagedWidget( "label",xmLabelWidgetClass,topform,
        XmNlabelString,thestring,
        XmNalignment,XmALIGNMENT_CENTER,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,title,
        XmNleftAttachment,XmATTACH_WIDGET,
        XmNleftOffset,10,
        XmNleftWidget,logo,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        XmNalignment,XmALIGNMENT_CENTER,
        NULL );
    XmStringFree( thestring );

    thestring=XmStringCreateLtoR( "Guest User","shellfont" );
    guest=XtVaCreateManagedWidget( "guestbutton",
        xmPushButtonWidgetClass,topform,
        XmNlabelString,thestring,
        XmNalignment,XmALIGNMENT_CENTER,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,label,
        XmNleftAttachment,XmATTACH_WIDGET,
        XmNleftOffset,10,
        XmNleftWidget,logo,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        XmNalignment,XmALIGNMENT_CENTER,
        NULL );
    XmStringFree( thestring );
    XtAddCallback( guest,XmNactivateCallback,
				(XtCallbackProc) warn_guest,parent );

    thestring=XmStringCreateLtoR( "New User","shellfont" );
    new=XtVaCreateManagedWidget( "newbutton",
        xmPushButtonWidgetClass,topform,
        XmNlabelString,thestring,
        XmNalignment,XmALIGNMENT_CENTER,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,guest,
        XmNleftAttachment,XmATTACH_WIDGET,
        XmNleftOffset,10,
        XmNleftWidget,logo,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        XmNalignment,XmALIGNMENT_CENTER,
        NULL );
    XmStringFree( thestring );
    XtAddCallback( new,XmNactivateCallback,
				(XtCallbackProc) start_new,topform );

    thestring=XmStringCreateLtoR( "Registered User","shellfont" );
    user=XtVaCreateManagedWidget( "guestbutton",xmPushButtonWidgetClass,
        topform,
        XmNlabelString,thestring,
        XmNalignment,XmALIGNMENT_CENTER,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,new,
        XmNleftAttachment,XmATTACH_WIDGET,
        XmNleftOffset,10,
        XmNleftWidget,logo,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        XmNalignment,XmALIGNMENT_CENTER,
        NULL );
    XmStringFree( thestring );
    XtAddCallback( user,XmNactivateCallback,
				(XtCallbackProc) start_registered,topform );

    thestring=XmStringCreateLtoR( "Exit","shellfont" );
    exit=XtVaCreateManagedWidget( "guestbutton",xmPushButtonWidgetClass,
        topform,
        XmNlabelString,thestring,
        XmNalignment,XmALIGNMENT_CENTER,
        XmNtopAttachment,XmATTACH_WIDGET,
        XmNtopOffset,10,
        XmNtopWidget,user,
        XmNleftAttachment,XmATTACH_WIDGET,
        XmNleftOffset,10,
        XmNleftWidget,logo,
        XmNrightAttachment,XmATTACH_FORM,
        XmNrightOffset,10,
        XmNalignment,XmALIGNMENT_CENTER,
        NULL );
    XmStringFree( thestring );
    XtAddCallback( exit,XmNactivateCallback,
				(XtCallbackProc) quit_ims,parent );

    /* Version 2: Avoid Motif 1.1 form warnings */
#ifdef Motif1_1
    XtManageChild( topform );
#endif

    XtRealizeWidget( parent );

    /* now that we're realized, load the logo and set callback */
    loadsystemlogo( logo );
    XtAddCallback( logo,XmNexposeCallback,
				(XtCallbackProc) drawlogo,NULL );
    return;
}   /*  make_welcome   */


/* *************************************************************
**
**  originally update_user.c
**  Purpose: Searches DB of users for key that matches input
**  Changes: Original   -  TBS   - Jeff Cleveland, LaRC DAAC
**           Solaris/QI - 960318 - Mike Kolar, ASF DAAC
**
**************************************************************** */

/* *************************************************************
**
**  subr add_user -- Adds a new user to the database
**  Input:    fname, lname, key, directory, origkey
**  Output:   origkey (original key (encrypted)
**
**************************************************************** */
static int add_user( fname,  lname,  key,  directory,  origkey )
char          *fname, *lname, *key,             *origkey;
char                                *directory;
{
    int   errSignal = 0;
    char  encrypted_key[ENCRYPTED_KEY_LEN+1], *dir,
          trunc_key[KEY_LEN+1], trunc_fname[FNAME_LEN+1],
          trunc_lname[LNAME_LEN+1], quota[BIG_STRING];
    static char *progName = "add_user";
    int  status;


    /* Truncate names to NAME_LEN   */
    truncate( trunc_fname,fname,FNAME_LEN );
    truncate( trunc_lname,lname,LNAME_LEN );

    /*  Truncate key to KEY_LEN */
    truncate( trunc_key,key,KEY_LEN );

    /*  Encrypt key     */
    (void) strcpy( encrypted_key, IK_MakeAuthenticator(
        trunc_fname ? trunc_fname:" ",
        trunc_lname ? trunc_lname:" ",
        trunc_key ) );

    /*  convert quota for insertion */
    (void) sprintf( quota, "%f", DEFAULT_DISK_QUOTA );

    /*  strip basedir off pathname. patch 1: strstr won't
        work if a person's name contained in the path; use
        strrchr instead. (Thanks BB@MSFC)  */
    dir = strrchr( directory,(int) '/' );
    if ( dir  ==  NULL )
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "%s: Missing directory spec (slash).\n",
            progName );
        errSignal++;
    }
    else
        dir++; /* skip leading / */

    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*  Prep SQL command to add the user    */
    /*  not done now as not enough information  d. pass  11/13/96 */
    /* *******
    (void) sprintf( insertCommand,
        "insert into
        "values ('%s', '%s', '%s','NEW', '%s', %s, getdate(),"
        " getdate())",trunc_fname, trunc_lname,
        encrypted_key,dir,quota );
    IMS_SETCMD (qDesc, insertCommand);

    if( ( status = ims_qiNextRow (qDesc) ) != IMS_ENDOFTRANSACTION )
        (not correct)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "%s: DB insert error [%d].\n", progName, status );
        errSignal++;
    }
    *********** */
    errSignal = 1;
    (void) ims_msg( msgDesc, IMS_ERROR,
        "%s: Add user capability not implimented.", progName );

    if( errSignal ) return( IMS_ERROR );
    return( IMS_OK ) ;
}   /*  add_user  */


/* *************************************************************
**
**  subr update_name --  Update a user's name in the database
**  Input:    fname, lname, orig_key, new_fname, new_lname
**
**
**************************************************************** */
static int  update_name(  fname,  lname,  orig_key,  new_fname,
    new_lname )
    char      *fname, *lname, *orig_key, *new_fname, *new_lname;
{
    int   status = 0, errSignal = 0;
    char  updateCommand[1024];
    static char *progName = "update_name";


    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /* Init SQL command to update the user's name   */
    (void) sprintf( updateCommand,
        "update user_profile  set "
        "first_name='%s', last_name='%s' "
        "where user_id = '%s'",
        fname, lname, user_id );
    IMS_SETCMD (qDesc, updateCommand);

    /* Gen msg if something went wrong; set error signal
        for later.  */
    if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "%s: DB update name error [%d].\n", progName, status );
        errSignal++;
    }
    if (IMS_AFFECTED (qDesc) < 1){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "%s: DB update row error [%d].\n", progName, status );
        errSignal++;
    }

    if( errSignal ) return( IMS_ERROR );
    return( IMS_OK ) ;
}   /*  update_name   */


/* *************************************************************
**
**  subr touch_user -- Update last login field
**  Input:    fname, lname, key
**  Output:   0=successful, -1=exit due to DB error
**
**************************************************************** */
static int touch_user( fname,  lname,  key )
    char            *fname, *lname, *key;
{
    int    status, errSignal;
    char   updateCommand[256];
    static char *progName = "touch_user";
    int  login_counter;
    char trunc_fname[FNAME_LEN+1],trunc_lname[LNAME_LEN+1];
    short  stats_found; /* if row for this user_id, then set true */


    status = 0;
    errSignal = 0;
    truncate( trunc_fname,fname,FNAME_LEN );
    truncate( trunc_lname,lname,LNAME_LEN );

    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*  need to first get the login_counter, then to increment
        it by 1.  the user_id value was obtained by find_user */
    (void) sprintf( updateCommand,
        "select login_counter  from  user_access_statistics  "
        "  where  user_id = '%s'", user_id );

    IMS_SETCMD (qDesc, updateCommand);
    stats_found = IMS_FALSE;
    while( ( status = ims_qiNextRow (qDesc) ) != IMS_ENDOFTRANSACTION ){
        if (status < IMS_OK){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "%s: DB read error [%d].\n", progName, status );
            errSignal++;
        }
        else  if (status  !=  IMS_ENDOFQUERY) {
            /*
            **  read data into local variable
            */
            (void) memcpy( (char *) &login_counter,
                qDesc->valAddr[0], qDesc->valLength[0]+1);
            stats_found = IMS_TRUE;
        }
    }
    if(  errSignal  >  0  ) return( IMS_ERROR );

    /*
    ** Reset Descriptor and get update
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "%s: Could not reset the query descriptor.",
            progName );
        return (status);
    }

    if(  stats_found  ){
        login_counter++;
        (void) sprintf( updateCommand,
            "update user_access_statistics  set last_login_time="
            "getdate(),  login_counter=%d  where  user_id = '%s'",
            login_counter, user_id );
    }
    else{
        /*
        **  put in a new row to the table
        */
        (void) sprintf( updateCommand,
            "insert into user_access_statistics( user_id, "
            "first_login_time, last_login_time, login_counter ) "
            "  values( '%s', getdate(), getdate(), 1 )",
            user_id );
    }

    /*  Gen msg if something went wrong; set error signal
        for later.  */
    if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "%s: DB update touch error [%d].\n", progName, status );
        errSignal++;
    }
    if (IMS_AFFECTED (qDesc) < 1){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "%s: DB update row error [%d].\n", progName, status );
        errSignal++;
    }

    if( errSignal >  0  ) return( IMS_ERROR );
    return( IMS_OK );
}   /*  touch_user  */


/* *************************************************************
**
**  subr update_key -- Update user's (encrypted) access
**  Input:    fname, lname, key
**  Output:   0=successful, -1=exit due to DB error
**
**************************************************************** */
static int update_key( fname,   lname,  oldkey,  newkey )
    char             *fname, *lname, *oldkey, *newkey;
{
    int  status = 0, errSignal = 0;
    char trunc_fname[FNAME_LEN+1],trunc_lname[LNAME_LEN+1],
        trunc_new_key[KEY_LEN+1],
        trunc_old_key[ENCRYPTED_KEY_LEN+1],
        new_enc_key[ENCRYPTED_KEY_LEN+1],
        selectCommand[256];

    static char *progName = "update_key";


    /* truncate to max length - patch 1: fix truncation */
    truncate( trunc_old_key,oldkey,ENCRYPTED_KEY_LEN );
    truncate( trunc_new_key,newkey,KEY_LEN );
    truncate( trunc_fname,fname,FNAME_LEN );
    truncate( trunc_lname,lname,LNAME_LEN );

    /*  Encrypt keys     */
    (void) strcpy( new_enc_key, IK_MakeAuthenticator(
        trunc_fname ? trunc_fname:" ",
        trunc_lname ? trunc_lname:" ",
        trunc_new_key ) );

    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    (void) sprintf( selectCommand,
        "update user_profile  set  auth_key = '%s', "
        " crypt_key = '%s'  where  user_id = '%s'",
        trunc_new_key, new_enc_key, user_id );
    IMS_SETCMD (qDesc, selectCommand);

    /* Gen msg if something went wrong; set error
        signal for later.  */
    if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "%s: DB update touch error [%d].\n", progName, status );
        errSignal++;
    }
    if (IMS_AFFECTED (qDesc) < 1){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "%s: DB update row error [%d].\n", progName, status );
        errSignal++;
    }

    if( errSignal ) exit( IMS_ERROR );
    return( IMS_OK );
}   /*  update_key  */


/* *************************************************************
**
**  subr find_user -- Searches for user specified in parameters
**  Input:    firstname, lastname, userkey
**  Output:   0=user found, 1=user not found,-1=system error
**            userdir,origkey,diskquota when 0 returned
**
**************************************************************** */
static int find_user( fname, lname, key, userdir, origkey, diskquota )
    char        *fname,*lname,*key,*userdir,*origkey;
    float       *diskquota;
{
    int   status;
    float  disk_quota;

    char   trunc_key[KEY_LEN+1];
    char   encrypted_key[ENCRYPTED_KEY_LEN+1], trunc_fname[FNAME_LEN+1];
    char   trunc_lname[LNAME_LEN+1];
    int    i;

    /*  Prep the SQL query clause for a remote_user.    */
    char  selectCommand[256];


    /*  Truncate names to NAME_LEN  */
    truncate( trunc_fname,fname,FNAME_LEN );/* patch 1: fix truncation*/
    truncate( trunc_lname,lname,LNAME_LEN );/* patch 1: fix truncation*/

    /*   Truncate key to KEY_LEN    */
    truncate( trunc_key,key,KEY_LEN );    /* patch 1: fix truncation */

    /*   Encrypt key  */
    (void) strcpy( encrypted_key, IK_MakeAuthenticator(
        trunc_fname ? trunc_fname:" ",
        trunc_lname ? trunc_lname:" ",
        trunc_key ) );

    /*
    ** Reset the query descriptor.
    */
    if( (status = ims_qiResetDesc (qDesc))  <  IMS_OK){
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*  Go see if the user is registered  */
    (void) sprintf( selectCommand,
        "select user_id, access_dir, access_quota_kbytes, auth_key  "
        "  from  user_profile  where  "
        " first_name = '%s' and last_name = '%s' and crypt_key = '%s'",
        trunc_fname, trunc_lname, encrypted_key );

    IMS_SETCMD (qDesc, selectCommand);

    i = 0;
    while( ( status = ims_qiNextRow (qDesc) ) !=  IMS_ENDOFTRANSACTION){
        if (status != IMS_ENDOFQUERY  &&  status  >=  IMS_OK ){
            /*
            **  have row: store the values
            */
            (void) memcpy((char *) user_id, qDesc->valAddr[0],
                qDesc->valLength[0]+1);
            user_id[qDesc->valLength[0]] = '\0';
            (void) trim( user_id );

            if( qDesc->valLength[1]  ==  0  ){
                (void) strcpy( userdir, userpath );
                (void) strcat( userdir, "/" );
                (void) strcat( userdir, user_id );
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "access_dir in table user_profile is null for "
                    "user id %s.", user_id );
                return( IMS_FATAL );
            }
            else{
                (void) memcpy((char *) userdir,
                    qDesc->valAddr[1], qDesc->valLength[1]+1);
                userdir[qDesc->valLength[1]] = '\0';
                (void) trim( userdir );
            }
            /*
            **  check for existence of directory.  if it does not
            **      exist, error return.
            */
            if(  access( userdir, F_OK )  ==  -1  ){
                /* does not exist  */
                (void)  ims_msg( msgDesc, IMS_ERROR,
                    "User id '%s' has non-existent access_dir"
                    " in table user_profile: %s.", user_id, userdir);
                return( IMS_FATAL );
            }

            if( qDesc->valLength[2]  ==  0  )
                disk_quota = DEFAULT_DISK_QUOTA;
            else{
                (void) memcpy( (char *) &disk_quota,
                    qDesc->valAddr[2], qDesc->valLength[2]+1);
            }
            *diskquota = disk_quota;

            if( qDesc->valLength[3]  ==  0  )  origkey[0] = '\0';
            else{
                (void) memcpy((char *) origkey,
                    qDesc->valAddr[3], qDesc->valLength[3]+1);
                origkey[qDesc->valLength[3]] = '\0';
                (void) trim( origkey );
            }
        }
    }
    if( IMS_AFFECTED( qDesc ) <  1 ){
        status = IMS_ERROR;
    }
    else{
        status = IMS_OK;
    }
    return( status );
}   /*  find_user  */


/* *************************************************************
**
**  subr trim.c -- routine to remove white space from strings.
**  returns pointer to string passed in
**
**************************************************************** */
static char *trim( string )
char *string;
{
    int i;

    if ( string[0]  !=  '\0' ) {
        for ( i=strlen(string)-1; string[i]  ==  ' ' && i  !=  0; i-- )
            if ( string[i-1]  !=  ' ' )
                string[i]='\0';
        if ( i  ==  0 && string[0]  ==  ' ' )
            string[0]='\0';
    }
    return( string );
}   /*  trim  */


/* ******************************************************************
**
** execCmd ()
**
** Execute a query. This function should only be used for commands
** that return one or no rows.
**
******************************************************************** */
static int execCmd (
    IMS_MSG_STRUCT *msgDesc_l,
    IMS_QI_DESC_OBJ *qDesc_l)
{
    int status;


    while ((status = ims_qiNextRow (qDesc_l)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            return (status);
        }
    }

    /*
    ** Check the stored procedure status returned value.
    */
    if (checkRetStatus (msgDesc_l, qDesc_l) < IMS_OK)
    {
        return (IMS_ERROR);
    }
    return (IMS_OK);
}  /* execCmd */


/*******************************************************************
**
** checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
******************************************************************** */
static int checkRetStatus (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc)
{
    int procReturn;
    int severity;

    /*
    ** Check to see if the Sybase procedure returned a status. If it did
    ** and it is not 0 (the OK value for a return), deal with the error.
    ** Return status of less than -100 correspond to message facility
    ** severity levels modulo 100.
    */
    if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
    {
        if ((procReturn = IMS_PROCRETURN (qDesc)) < 0)
        {
            if (procReturn == -103)
            {
                severity = IMS_FATAL;
            }
            else if (procReturn == -102)
            {
                severity = IMS_ERROR;
            }
            else if (procReturn == -101)
            {
                severity = IMS_WARNING;
            }
            else
            {
                severity = IMS_ERROR;
            }
            (void) ims_msg (msgDesc, severity,
                "Sybase procedure '%s' returned a status of %d",
                qDesc->cmd, procReturn);
            return (severity);
        }
    }
    return (IMS_OK);
}  /* checkRetStatus */
