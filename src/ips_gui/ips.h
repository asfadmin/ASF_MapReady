#include <iostream>
#include <stdexcept>
#include <sstream>

#include <libglademm/xml.h>
#include <gtkmm.h>

class IPSWin : public sigc::trackable
{
public:
    explicit IPSWin(Glib::RefPtr<Gnome::Glade::Xml> refXml);
    ~IPSWin();

    Gtk::Window&  get_window() const { return *ipswin_; }

private:
    Gtk::Window*  ipswin_;
};
