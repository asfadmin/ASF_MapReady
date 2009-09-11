/* Modified for the Alaska SAR Facility SAR Processing Subsystem */

#ifndef	_YYLEX_H
#define _YYLEX_H

/*  Complete context of Scanner */
typedef struct {
    int				yy_init;
    int				yy_start;
    void*			yyin;
    void*			yyout;

    char*			yytext;
    int				yyleng;
    int				yylineno;
    int				yy_flex_debug;

    int				yy_start_stack_ptr;
    int				yy_start_stack_depth;
    int*			yy_start_stack;

    void*			yy_current_buffer;
    char			yy_hold_char;
    int				yy_n_chars;
    char*			yy_c_buf_p;

    int				yy_did_buffer_switch_on_eof;
    int				yy_last_accepting_state;
    char*			yy_last_accepting_cpos;

    void*			yy_state_buf;
    void*			yy_state_ptr;

    char*			yy_full_match;
    int*			yy_full_state;
    int				yy_full_lp;
    int				yy_lp;
    int				yy_looking_for_trail_begin;
    int				yy_more_flag;
    int				yy_more_len;

} yylex_t;

#endif	/*!_YYLEX_H */
