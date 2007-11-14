#include "asf_view.h"
#include "plan.h"

// 
void setup_planner()
{
  show_widget("planner_notebook", TRUE);
  show_widget("viewer_notebook", FALSE);

  // populate the "Satellite/Beam" dropdown from the
  // "beam_modes.txt" file
  int n_beam_modes;
  char **modes = get_all_beam_modes(&n_beam_modes);

  int i;
  clear_combobox("satellite_combobox");
  for (i=0; i<n_beam_modes; ++i)
    add_to_combobox("satellite_combobox", modes[i]);

  set_combo_box_item_checked("satellite_combobox", 0);
  set_combo_box_item_checked("orbit_direction_combobox", 0);

  for (i=0; i<n_beam_modes; ++i)
    FREE(modes[i]);
  FREE(modes);
}

SIGNAL_CALLBACK void on_plan_button_clicked(GtkWidget *w)
{


//  plan(satellite, beam_mode, startdate, enddate, max_lat, min_lat,
//       clat, clon, pass_type, aoi, tle_filename, outfile);
}
