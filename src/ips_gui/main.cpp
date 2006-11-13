#include <iostream>
#include <stdexcept>
#include <sstream>

#include <libglademm/xml.h>
#include <gtkmm.h>

void set_font ();

#include "ips.h"
int main(int argc, char* argv[])
{
    try
    {
        Gtk::Main kit(argc, argv);
        Glib::RefPtr<Gnome::Glade::Xml> refXml = 
            Gnome::Glade::Xml::create("ips.glade");
        IPSWin ips(refXml);
        kit.run(ips.get_window());
        return 0;
    }
    catch (std::exception const& ex)
    {
        std::cerr << "Error: " << ex.what() << std::endl;
        return 1;
    }
}
