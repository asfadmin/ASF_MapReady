/*      GLOBAL VARIABLE DECLARATIONS for the QA app     */

#ifndef _QGLOBAL_VAR_H
#define _QGLOBAL_VAR_H

static char sccsid_qglobal_var_h[] =
	"@(#)gglobal_var.h	1.2 96/03/14 15:01:54";

/*      all global variables begin with an "_". */
EXTERN_IF_NOT_IN_MAIN   char     _qc_log_prefix[50];

EXTERN_IF_NOT_IN_MAIN   byte     *_small_picture;

EXTERN_IF_NOT_IN_MAIN   double   _pixel_spacing;
EXTERN_IF_NOT_IN_MAIN   double   _line_spacing;

/* Widgets for layout */
EXTERN_IF_NOT_IN_MAIN   Widget   _toplevel, _pic_label;

EXTERN_IF_NOT_IN_MAIN   Widget   _right_outer_frame, 
                                 _menubar,
                                 _left_scrolled_window,
                                 _graph_drawing_area,
                                 _i_graph_drawing_area,
                                 _q_graph_drawing_area,
                                 _major_info_frame;


/* so you can change the sensitivity of the stretch buttons */
EXTERN_IF_NOT_IN_MAIN   Widget   _low_down_pb, _low_fast_down_pb, 
                                 _low_up_pb, _low_fast_up_pb,
                                 _high_up_pb, _high_fast_up_pb,
                                 _high_down_pb, _high_fast_down_pb,

                                 /* and for the zoom buttons */
                                 _zoom_in_pb, _zoom_out_pb,
                                 _zoom_box_pb,

                                 _accept_pb, _reject_pb, _hold_pb;

/* for the zooming */

EXTERN_IF_NOT_IN_MAIN   int      _x_zoom_offset, _y_zoom_offset;

EXTERN_IF_NOT_IN_MAIN   int      _old_covered_x, _old_covered_y;

EXTERN_IF_NOT_IN_MAIN   int      _zoom_box_width, _zoom_box_height;

EXTERN_IF_NOT_IN_MAIN   int      _background_colorcell;

/* for the box drawing using a GC */
EXTERN_IF_NOT_IN_MAIN   GC       _pixmap_gc;
EXTERN_IF_NOT_IN_MAIN   Pixmap   _spare_pixmap;

/* for the stretching */

EXTERN_IF_NOT_IN_MAIN   int      _fast_change_amount, 
                                 _slow_change_amount,
                                 _state;

EXTERN_IF_NOT_IN_MAIN   char     *_filename, 
                                 *_leader_file, 
                                 *_resource_file,
                                 *_average_in_file,
                                 *_output_to_file;


EXTERN_IF_NOT_IN_MAIN   Widget   _horizontal_sb, _vertical_sb, 
                                 _clip_window;

/* for the messy scaling process:  */
EXTERN_IF_NOT_IN_MAIN   int       _image_scale_factor;
EXTERN_IF_NOT_IN_MAIN   double    _complex_multiply_factor,
                                  _complex_scale_parameter;

EXTERN_IF_NOT_IN_MAIN   int       _verbose_option;


EXTERN_IF_NOT_IN_MAIN   int       _left_border_pixels, 
                                  _left_pixel_pad,
                                  _right_border_pixels,
                                  _records_count,   
                                  _records_length,
                                  _bits_per_sample,
                                  _byte_factor,
                                  _lines_per_channel,
                                  _is_sys_pp,
                                  _is_sys_ramp,
                                  _data_byte_count,
                                  _pixels_per_line,
                                  _top_border_lines,
                                  _bottom_border_lines;

/* X variables, initialized in set_up_environment() */
EXTERN_IF_NOT_IN_MAIN   int           _screen_number;
EXTERN_IF_NOT_IN_MAIN   Display       *_display_pointer;
EXTERN_IF_NOT_IN_MAIN   XColor        *_xcolors;
EXTERN_IF_NOT_IN_MAIN   Colormap      _colormap;

EXTERN_IF_NOT_IN_MAIN   Pixmap        _image_pixmap;

/* for stretching */
EXTERN_IF_NOT_IN_MAIN   int           _low_pixel_stretch_value, 
                                      _high_pixel_stretch_value,
                                      _i_low_pixel_stretch_value, 
                                      _i_high_pixel_stretch_value,
                                      _q_low_pixel_stretch_value, 
                                      _q_high_pixel_stretch_value;

/* for the minor info labels */
EXTERN_IF_NOT_IN_MAIN   char          *_production_request_id_string,
                                      *_order_id_string,    /* job_id now */
                                      *_scene_id_string,
                                      *_satellite_string,
                                      *_revolution_string,
                                      *_sequence_number_string,
                                      *_sys_id_string,
                                      *_frame_string,
                                      *_latitude_string,
                                      *_longitude_string,
                                      *_time_string;


EXTERN_IF_NOT_IN_MAIN   char          *_format_type;


EXTERN_IF_NOT_IN_MAIN   Widget        type_value_label,
                                      _psf_value_text, 
                                      image_size_x_value_label, 
                                      image_size_y_value_label, 
                                      _dmin_value_text, 
                                      _dmax_value_text, 
                                      _mean_value_text, 
                                      _sdev_value_text, 
                                      _hmin_value_text, 
                                      _atmin_value_text, 
                                      _hmax_value_text,
                                      _atmax_value_text, 
                                      _zpix_value_text, 
                                      _satp_value_text, 
                                      _low_stretch_value_text, 
                                      _high_stretch_value_text, 
                                      _i_dmin_value_text, 
                                      _i_dmax_value_text, 
                                      _i_mean_value_text, 
                                      _i_sdev_value_text, 
                                      _i_zpix_value_text, 
                                      _i_satp_value_text, 
                                      _i_hmin_value_text, 
                                      _i_atmin_value_text, 
                                      _i_hmax_value_text,
                                      _i_atmax_value_text, 
                                      _q_dmin_value_text, 
                                      _q_dmax_value_text, 
                                      _q_mean_value_text, 
                                      _q_sdev_value_text, 
                                      _q_zpix_value_text, 
                                      _q_satp_value_text, 
                                      _q_hmin_value_text, 
                                      _q_atmin_value_text, 
                                      _q_hmax_value_text,
                                      _q_atmax_value_text, 
                                      major_info_scrolled_list;

/* the minor info label values */

EXTERN_IF_NOT_IN_MAIN    int          _image_size_x, _image_size_y;
EXTERN_IF_NOT_IN_MAIN    double       _snr, _ber;
                                       
/* for the histogram */

EXTERN_IF_NOT_IN_MAIN    int          _histogram_array[256],
                                      _histogram_size,
                                      _i_histogram_array[256],
                                      _i_histogram_size,
                                      _q_histogram_array[256],
                                      _q_histogram_size,
                                      _number_of_histogram_records,
                                      _total_number_of_histogram,
                                      _image_type;

EXTERN_IF_NOT_IN_MAIN    char         _histogram_description[33],
                                      _i_histogram_description[33],
                                      _q_histogram_description[33];

EXTERN_IF_NOT_IN_MAIN    int          _samples_per_group;

EXTERN_IF_NOT_IN_MAIN    int          _valid_leader_file;


EXTERN_IF_NOT_IN_MAIN    float        _data_mean,
                                      _data_sdev,
                                      _i_data_mean,
                                      _i_data_sdev,
                                      _q_data_mean,
                                      _q_data_sdev;

EXTERN_IF_NOT_IN_MAIN    int          _histogram_min,
                                      _histogram_max,
                                      _histogram_max_location,
                                      _histogram_min_location,
                                      _i_histogram_min,
                                      _i_histogram_max,
                                      _i_histogram_max_location,
                                      _i_histogram_min_location,
                                      _q_histogram_min,
                                      _q_histogram_max,
                                      _q_histogram_max_location,
                                      _q_histogram_min_location,
                                      _data_min, 
                                      _data_max;

EXTERN_IF_NOT_IN_MAIN    GC           _histogram_gc;


/* this is set up in set_up_picture, and contains values
   for many of the minor info labels */
EXTERN_IF_NOT_IN_MAIN   XImage        *_x_image;

EXTERN_IF_NOT_IN_MAIN   int           _colormap_option;

EXTERN_IF_NOT_IN_MAIN   byte          *_unaltered_bytes;

#endif /* _QGLOBAL_VAR_H */
