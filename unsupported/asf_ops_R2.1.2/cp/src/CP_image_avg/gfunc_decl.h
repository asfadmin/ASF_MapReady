/*      FUNCTION DECLARATIONS for the QA app       */

#ifndef _QFUNC_DECL_H
#define _QFUNC_DECL_H

static char sccsid_qfunc_decl_h[] =
	"@(#)gfunc_decl.h	1.3 96/12/30 14:15:28";

void sig_term() ;


/*  qfunctions.c */

void   set_up_log();
void   set_up_environment() ;

void   set_stretch_button_sensitivity() ;
void   set_zoom_button_sensitivity() ;

void   change_all_sensitivity() ;

void   read_ceos_file() ;
void   read_leader_file() ;

void   interpret_data_set_summary_record() ;
void   interpret_data_histogram_record () ;
void   interpret_data_summary_quality_record () ;

int    compute_record_offset () ;

void   create_histogram_array_from_clipped_picture() ;
void   create_histogram_array_from_complex_picture() ;

void   set_up_picture() ; 
char*  get_information_from_file() ;

byte*  set_up_unsigned_image() ;
byte*  read_averaged_file() ;

byte*  set_up_complex_image() ;

void   cut_y_by_4 () ;

void   parse_command_line() ;
void   cut_path_off_filename() ;
void   print_usage() ;

byte   complex_average_around() ;
byte   average_around() ;

int    read_n_bytes() ;

void   change_stretching() ;

void   make_announcement() ;

void   draw_histogram() ;
void   draw_histogram_delimiting_lines () ;

void   set_up_new_swath() ;
void   set_up_new_16_swath() ;
void   set_up_new_complex_swath() ;

void   set_up_box_gc () ;

byte*  read_subset_of_image() ;

void   move_zoom_box() ;
void   accept_zoom_location() ;

void   add_colons_to() ;
void format_description(char *);

/* qcallbacks.c */

void file_cb(), open_cb(), help_cb(),      
     zoom_box_cb(), 
     accept_cb(), reject_cb(), hold_cb(),
     high_up_cb(), high_fast_up_cb(),
     high_down_cb(), high_fast_down_cb(),
     low_up_cb(), low_fast_up_cb(),
     low_down_cb(), low_fast_down_cb(),
     low_stretch_text_change_cb(),
     high_stretch_text_change_cb() ;

/* qleft.c */

Widget set_up_left() ;

/* qmenubar.c */

Widget set_up_menubar() ;

/* qright.c */

Widget set_up_right() ;

/* qinfo_widgets.c */

Widget initialize_major_info_widget() ;
void   initialize_info_label_values() ;
void   set_up_histogram_dependant_labels () ;

int    figure_out_scale_factor() ;

void   read_first_record_byte_data () ;

void   put_together_average_file() ;
void   alter_first_record() ;
void   write_new_file() ;

void   set_up_averaged_complex_global_variables() ;
void   set_up_unaveraged_complex_global_variables() ;

byte   *read_unaveraged_complex_file() ;
byte   *read_averaged_complex_file() ;

void   manipulate_xcolors() ;
void   dither_image() ;

static void LeftToRight() ;
static void RightToLeft() ;

static unsigned int tone_scale_adjust();

byte   *read_subset_of_complex_image() ;
void   detect_data_in_line() ;
void   change_pixmaps() ;

void   find_background_pixel_value() ;
void   remove_background_from_x_image() ;

void   copy_and_alter_bytes() ;
void   clean_up_after_avgfile() ;
void   handle_typed_file() ;
void   null_terminate_properly() ;

void   zoom_in_without_box() ;
void   zoom_in_with_box() ;
void   zoom_back_out() ;

#endif /* _QFUNC_DECL_H */
