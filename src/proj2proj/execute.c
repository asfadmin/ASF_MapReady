#include "proj2proj.h"

SIGNAL_CALLBACK void
on_forward_button_clicked(GtkWidget * widget)
{
  printf("Forward!!!\n");
}

SIGNAL_CALLBACK void
on_backward_button_clicked(GtkWidget * widget)
{
  printf("Back!!!\n");
}
