/*---------------------------------------------------------------------
 * $Date$             $Revision$
 *---------------------------------------------------------------------
 * 
 *
 *             Copyright (c) 1991, Visual Edge Software Ltd.
 *
 * ALL  RIGHTS  RESERVED.  Permission  to  use,  copy,  modify,  and
 * distribute  this  software  and its documentation for any purpose
 * and  without  fee  is  hereby  granted,  provided  that the above
 * copyright  notice  appear  in  all  copies  and  that  both  that
 * copyright  notice and this permission notice appear in supporting
 * documentation,  and that  the name of Visual Edge Software not be
 * used  in advertising  or publicity  pertaining to distribution of
 * the software without specific, written prior permission. The year
 * included in the notice is the year of the creation of the work.
 *-------------------------------------------------------------------*/

/*****************************************************************************/
/*				UxXt.h				             */
/*****************************************************************************/

#ifndef	_UX_XT_H_
#define	_UX_XT_H_

#if defined(EXTERN_C_WRAPPERS) && defined(__cplusplus)
#define EXTERNC extern "C"
#else
#define EXTERNC extern
#endif /* EXTERN_C_WRAPPERS */

#include <stdlib.h>
#include <string.h>
#include <Xm/Xm.h>

#ifdef UIL_CODE
#include <Mrm/MrmPublic.h>
#endif /* UIL_CODE */

#if defined(__STDC__) && !defined(SOLARIS) && !defined(sun4) && !defined(univel) && !defined(motor88) && !defined(__sgi) && !defined(ncrix86) && !defined(sco) && !defined(osf1) && !defined(linux) && !defined(_INCLUDE_HPUX_SOURCE)
typedef char *caddr_t;
#endif

/*-----------------------------------------------------
 * UXORB_HEADER, if defined, is the include form for
 * the header that defines the CORBA Environment type
 * and exception type codes.
 *
 * You can specify a file with a compile option like
 * 	-DUXORB_HEADER='<SomeOrb.h>'
 *-----------------------------------------------------*/
#ifdef UXORB_HEADER
#include UXORB_HEADER
#else
	/*
	 * In the absence of an ORB implementation,
	 * these minimal definitions satisfy our method dispatch code.
	 */
	typedef enum {
		NO_EXCEPTION,
		USER_EXCEPTION,
		SYSTEM_EXCEPTION
	} exception_type;

	typedef struct Environment {
		exception_type	_major;
	} Environment;
#endif  /* UXORB_HEADER */

/*
 * UxEnv is provided as a convenience for use in interface methods.
 */
extern	Environment	UxEnv;


/* The following macros are used in converting string values to the form
   required by the widgets */

#define	RES_CONVERT( res_name, res_value) \
	XtVaTypedArg, (res_name), XmRString, (res_value), strlen(res_value) + 1

#define	UxPutStrRes( wgt, res_name, res_value ) \
	XtVaSetValues( wgt, RES_CONVERT( res_name, res_value ), NULL )



#ifndef	UXPROTO
#	ifdef _NO_PROTO
#		define UXPROTO(x) ()
#	else /* _NO_PROTO */
#		define UXPROTO(x) x
#	endif /* _NO_PROTO */
#endif /* UXPROTO */

extern char *UxConcatStrings UXPROTO(( char*, char* ));

#ifndef UX_INTERPRETER	/* Omit this section when interpreting the code */

/* The following macros are supplied for compatibility with swidget code */
#define	swidget			Widget
#define	UxIsValidSwidget(sw)	((sw) != NULL)
#define NO_PARENT             	((Widget) NULL)
#define UxThisWidget		(UxWidget)
#define UxWidgetToSwidget(w)    (w)
#define UxGetWidget(sw)         (sw)


/* Macros needed for the method support code */
#define	UxMalloc(a)		(malloc(a))
#define	UxRealloc(a,b)		(realloc((a), (b)))
#define	UxCalloc(a,b)		(calloc((a), (b)))
#define UxFree(a)		(free(a))
#define UxStrEqual(a,b)		(!strcmp((a),(b)))
#define UxGetParent(a)		(XtParent((a)))

#define	no_grab			XtGrabNone
#define	nonexclusive_grab	XtGrabNonexclusive
#define	exclusive_grab		XtGrabExclusive


/* The following global variables are defined in the main() function */
extern  XtAppContext	UxAppContext;
extern  Widget		UxTopLevel;
extern  Display		*UxDisplay;
extern  int		UxScreen;


/* The following are error codes returned by the functions in UxXt.c */
#define UX_ERROR           -1
#define UX_NO_ERROR        0

#ifdef UIL_CODE
#ifdef _NO_PROTO
extern	void    	UxMrmFetchError();
extern	MrmHierarchy    UxMrmOpenHierarchy();
extern	void    	UxMrmRegisterClass();
#else
extern	void    	UxMrmFetchError(MrmHierarchy, char *, Widget, Cardinal);
extern	MrmHierarchy    UxMrmOpenHierarchy( char *);
extern	void    	UxMrmRegisterClass( char *, Widget (*)(Widget, String, Arg *, Cardinal));
#endif /* _NO_PROTO */
#endif /* UIL_CODE */



/* The following are declarations of the functions in UxXt.c */

#ifdef _NO_PROTO

extern  int		UxPopupInterface();
extern  int		UxPopdownInterface();
extern  int		UxDestroyInterface();
extern  int		UxPutContext();
extern  void *		UxGetContext();
extern  void *          UxNewContext();
extern  void		UxFreeClientDataCB();
extern  void		UxLoadResources();
extern  XmFontList	UxConvertFontList();
extern  Pixmap		UxConvertPixmap();
extern  Pixmap		UxConvert_bitmap();
extern	wchar_t *	UxConvertValueWcs();
extern	void            UxDestroyContextCB();
extern	XtArgVal	UxRemoveValueFromArgList();
extern	Widget		UxChildSite();
extern	Widget  	UxRealWidget( );
extern	void    	UxDeleteContextCB( );
extern	int     	UxGetClassCode();
extern	int     	UxMessageIndex ();
extern	int     	UxGetIfClassCode();

#else

extern  int		UxPopupInterface( Widget wgt, XtGrabKind grab_flag );
extern  int		UxPopdownInterface( Widget wgt );
extern  int		UxDestroyInterface( Widget wgt);
extern  void		UxFreeClientDataCB( Widget wgt, XtPointer client_data,
						 XtPointer call_data );
extern  void		UxLoadResources( char *fname );
extern  XmFontList	UxConvertFontList( char *fontlist_str );
extern  Pixmap		UxConvertPixmap( char *file_name );
extern  Pixmap		UxConvert_bitmap( char *file_name );
extern	wchar_t *	UxConvertValueWcs( char *value_str );

extern  void            UxDestroyContextCB(Widget, XtPointer, XtPointer);
extern	XtArgVal	UxRemoveValueFromArgList( Arg *args,
						Cardinal *ptr_num_args,
						String res_name );

/*-----------------------------------------------------
 * Functions that need to be compiled a C functions
 * when compiling with C++.
 *-----------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

extern	void    	UxDeleteContextCB( Widget, XtPointer, XtPointer);
extern  int		UxPutContext( Widget wgt, void * context );
extern  void *		UxGetContext( Widget wgt );
extern  void *          UxNewContext( size_t size, int isSubclass);
extern	Widget  	UxRealWidget( Widget );
extern	Widget		UxChildSite( Widget );
extern	int     	UxGetClassCode(swidget sw);
extern	int     	UxMessageIndex ( char* name);
extern	int     	UxGetIfClassCode( Widget wgt );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _NO_PROTO */

/*-----------------------------------------------------
 * The interface base class, C and C++ versions.
 * These declarations are identical to those in UxLib.h.
 *-----------------------------------------------------*/

#ifdef __cplusplus

  class _UxCInterface 
  {
  public:

	virtual ~_UxCInterface() {}
	virtual swidget childSite (Environment * pEnv);
	virtual swidget UxChildSite (swidget sw);
	virtual void _set_x (Environment*, int);
	virtual int  _get_x (Environment*);
	virtual void _set_y (Environment*, int);
	virtual int  _get_y (Environment*);
	virtual void _set_width (Environment*, int);
	virtual int  _get_width (Environment*);
	virtual void _set_height (Environment*, int);
	virtual int  _get_height (Environment*);
	virtual void UxManage (Environment*);
	virtual swidget get_UxThis() {return UxThis;}

  protected:
	swidget	UxThis;
  };

#  define CPLUS_ADAPT_CONTEXT(CLASS) \
        static inline \
                CLASS* UxGetContext(CLASS*self) {return self;} \
        static inline\
                void* UxGetContext(swidget any) {return ::UxGetContext(any);}


/* Corba - to - C++ translation for method calls. */

inline int Interface__get_x(swidget UxThis, Environment* pEnv) 
{
	return ((_UxCInterface*)UxGetContext(UxThis))->_get_x(pEnv);
}

inline void  Interface__set_x(swidget UxThis, Environment *pEnv, int value) 
{
	((_UxCInterface*)UxGetContext(UxThis))->_set_x(pEnv, value);
}

inline int Interface__get_y(swidget UxThis, Environment* pEnv) 
{
	return ((_UxCInterface*)UxGetContext(UxThis))->_get_y(pEnv);
}

inline void Interface__set_y(swidget UxThis, Environment *pEnv, int value) 
{
	((_UxCInterface*)UxGetContext(UxThis))->_set_y(pEnv, value);
}

inline int Interface__get_width(swidget UxThis, Environment *pEnv) 
{
	return ((_UxCInterface*)UxGetContext(UxThis))->_get_width(pEnv);
}

inline void Interface__set_width(swidget UxThis, Environment *pEnv, int value) 
{
	((_UxCInterface*)UxGetContext(UxThis))->_set_width(pEnv, value);
}

inline int Interface__get_height(swidget UxThis, Environment *pEnv) 
{
	return 
	  ((_UxCInterface*)UxGetContext(UxThis))->_get_height(pEnv);
}

inline void Interface__set_height(swidget UxThis, Environment *pEnv, int value) 
{
	((_UxCInterface*)UxGetContext(UxThis))->_set_height(pEnv, value);
}

inline void Interface_UxManage(swidget UxThis, Environment *pEnv) 
{
	((_UxCInterface*)UxGetContext(UxThis))->UxManage(pEnv);
}

#else  /* C interface to the base class. */

#  define Interface__get_x( UxThis, pEnv ) \
	((int(*)())UxMethodLookup(UxThis, UxInterface__get_x_Id,\
			UxInterface__get_x_Name)) \
		( UxThis, pEnv )

#  define Interface__set_x( UxThis, pEnv, value ) \
	((void(*)())UxMethodLookup(UxThis, UxInterface__set_x_Id,\
			UxInterface__set_x_Name)) \
		( UxThis, pEnv, value )

#  define Interface__get_y( UxThis, pEnv ) \
	((int(*)())UxMethodLookup(UxThis, UxInterface__get_y_Id,\
			UxInterface__get_y_Name)) \
		( UxThis, pEnv )

#  define Interface__set_y( UxThis, pEnv, value ) \
	((void(*)())UxMethodLookup(UxThis, UxInterface__set_y_Id,\
			UxInterface__set_y_Name)) \
		( UxThis, pEnv, value )

#  define Interface__get_width( UxThis, pEnv ) \
	((int(*)())UxMethodLookup(UxThis, UxInterface__get_width_Id,\
			UxInterface__get_width_Name)) \
		( UxThis, pEnv )

#  define Interface__set_width( UxThis, pEnv, value ) \
	((void(*)())UxMethodLookup(UxThis, UxInterface__set_width_Id,\
			UxInterface__set_width_Name)) \
		( UxThis, pEnv, value )

#  define Interface__get_height( UxThis, pEnv ) \
	((int(*)())UxMethodLookup(UxThis, UxInterface__get_height_Id,\
			UxInterface__get_height_Name)) \
		( UxThis, pEnv )

#  define Interface__set_height( UxThis, pEnv, value ) \
	((void(*)())UxMethodLookup(UxThis, UxInterface__set_height_Id,\
			UxInterface__set_height_Name)) \
		( UxThis, pEnv, value )


#  define Interface_UxManage( UxThis, pEnv ) \
	do {\
		swidget _sw_ = UxThis;\
		((void(*)())UxMethodLookup(_sw_, UxInterface_UxManage_Id,\
			UxInterface_UxManage_Name)) \
		( _sw_, pEnv ); \
	} while (0)

  extern int	UxInterface__set_height_Id;
  extern char*	UxInterface__set_height_Name;
  extern int	UxInterface_UxManage_Id;
  extern char*	UxInterface_UxManage_Name;
  extern int	UxInterface__get_x_Id;
  extern char*	UxInterface__get_x_Name;
  extern int	UxInterface__get_y_Id;
  extern char*	UxInterface__get_y_Name;
  extern int	UxInterface__set_x_Id;
  extern char*	UxInterface__set_x_Name;
  extern int	UxInterface__set_y_Id;
  extern char*	UxInterface__set_y_Name;
  extern int	UxInterface__get_width_Id;
  extern char*	UxInterface__get_width_Name;
  extern int	UxInterface__set_width_Id;
  extern char*	UxInterface__set_width_Name;
  extern int	UxInterface__get_height_Id;
  extern char*	UxInterface__get_height_Name;

  /*
   * This is used to get a class ID for each top-level interface class.
   * It ensures that the interface base class is registered.
   */
  int	UxNewInterfaceClassId ();

  /*----------------------------------------------------------------------
   * Generated code uses these macros when setting component properties.
   *----------------------------------------------------------------------*/

/* Horrible patch for sun4's cc incapacity to concatenate strings. */
#if defined(sun4)
#       define CONCAT(a,b) UxConcatStrings(a,b)
#else
#       define CONCAT(a,b) a b
#endif

#	if defined(__STDC__) || defined(UX_INTERPRETER)
#		define UxPUT_PROPERTY(sw, propname, valType, value) \
 		((void (*)(Widget, Environment *, valType)) \
			UxMethodLookup((sw), -1, "_set_" # propname)) \
			(sw, &UxEnv, (value))
#	else /* (__STDC__) || (UX_INTERPRETER) */
#		define UxPUT_PROPERTY(sw, propname, valType, value) \
 		((void (*)()) UxMethodLookup((sw), -1, CONCAT("_set_","propname"))) \
        		(sw, &UxEnv, (value))
#	endif /* (__STDC__) || (UX_INTERPRETER) */

#       if defined(__STDC__) || defined(UX_INTERPRETER)
#define UxPUT_CALLBACK(sw, cbName, cbVal, clientData) \
 	((void (*)(swidget, Environment *, XtCallbackProc, void *)) \
		UxMethodLookup((sw), -1, cbName)) \
        	((sw), &UxEnv, (cbVal), (clientData)) 
#else
#define UxPUT_CALLBACK(sw, cbName, cbVal, clientData) \
        ((void (*)()) \
                UxMethodLookup((sw), -1, cbName)) \
                ((sw), &UxEnv, (cbVal), (clientData))
#endif

#endif /* ! __cplusplus */

/*----------------------------------------------------------------------
 * End of interface base class definitions.
 *----------------------------------------------------------------------*/

#endif /* ! UX_INTERPRETER */

/*------------------------------------------------------------------------
 * UxAdapterSwidget
 * <Creates a component swidget for any Widget.>
 * INPUT:	Widget w:	-- any Widget
 *		swidget parent;	-- the swidget representing its parent.
 *		char * name;	-- the name for the swidget
 *		int  clsCode	-- the class code for the component
 *		void * cmpntRef -- user field for the external object
 *		void * context	-- the UIM/X context ptr of the component
 * RETURNS:	
 *	The Widget supplied.
 *
 * See UxLib.h.  In Xt-mode, this function serves to attach the cmpntRef,
 * context, and interface class code, to the supplied Widget.
 *------------------------------------------------------------------------*/

#ifdef _NO_PROTO 
extern swidget	UxAdapterSwidget ();
void*	UxGetComponentRef ();
void	UxPutComponentRef ();
int     UxPutClassCode();
#else

/*-----------------------------------------------------
 * Functions that need to be compiled a C functions
 * when compiling with C++.
 *-----------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

swidget	UxAdapterSwidget(Widget w,
			 Widget parent,
			 char*  name,
			 int	 clsCode,
			 void*  cmpnt,
			 void*  context);

void*	UxGetComponentRef (Widget adapter);
int     UxPutClassCode( Widget wgt, int id);
void	UxPutComponentRef (Widget adapter, void* ref);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _NO_PROTO */

#define	UxNO_CLSCODE	-1
#define	UxNO_CONTEXT	0

/* Xt-code components cannot be used at design-time. */
 
#define	UxAdapterDesignMethods(a,b,c,d)	

#endif /* ! _UX_XT_H_ */

