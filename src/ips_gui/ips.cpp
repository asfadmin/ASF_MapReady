#include <iostream>
#include <stdexcept>
#include <sstream>

#include <libglademm/xml.h>
#include <gtkmm.h>
#include "ips.h"

IPSWin::
IPSWin(Glib::RefPtr<Gnome::Glade::Xml> refXml)
    : ipswin_(0)
{
    refXml->get_widget("ips_window", ipswin_);
    if (!ipswin_)
        throw std::runtime_error("Couldn't find ips_window in ips.glade");
}

IPSWin::
~IPSWin()
{
    delete ipswin_;
}
