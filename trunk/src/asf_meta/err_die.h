/* Header for the err_die function.  */

#ifndef ERR_DIE_H

void err_die(const char *error_message, ...)/* ; is coming, don't worry.  */
/* The GNU C compiler can give us some special help if compiling with
   -Wformat or a warning option that implies it.  */
#ifdef __GNUC__
     /* Function attribute format says function is variadic al la
        printf, with argument 1 its format spec and argument 2 its
        first optional argument corresponding to the spec.  Function
        attribute noreturn says function doesn't return.  */
     __attribute__ ((format (printf, 1, 2), noreturn))
#endif /* __GNUC__ */ 
; /* <-- Semicolon for err_die prototype.  */

#endif /* ERR_DIE_H */
