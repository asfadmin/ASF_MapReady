import java.awt.* ;
import java.awt.event.*;
import java.io.* ;
import java.util.* ;

public class SelectArea extends Dialog implements ActionListener,
                                                  WindowListener {
   protected String area;
   protected Vector area_list;
   protected java.awt.List area_list_wid;
   protected TextField info_area;
   protected Button cancelButton, okButton;
   protected Area selected_item;

   public Area getSelectedItem() {
      return selected_item;
   }

   public SelectArea (String title, Frame f) {
      super(f, title, true);
         /* Structure window */
      Panel top, bottom, button_row;
      top = new Panel(new BorderLayout());
      bottom = new Panel(new BorderLayout());
      button_row = new Panel();
      top.add(bottom, BorderLayout.SOUTH);

      top.add(new Label ("Please select the region"), BorderLayout.NORTH);

      /* Obtain the possible areas */
      Spawner S = new Spawner ( "list_sites.sql");
      AreaReader r = new AreaReader ( S.In);
      area_list= r.getElements();
         
      /* Display the Area's names */
      area_list_wid = new java.awt.List ( area_list.size() );
      for (int a = 0; a < area_list.size(); a++)
         { area_list_wid.add(((Area)area_list.elementAt(a)).cs_name); }
      top.add(area_list_wid, BorderLayout.CENTER);

      /* Create a info window */
      info_area = new TextField();
      info_area.setEditable(false);
   
      bottom.add(info_area, BorderLayout.CENTER);
      cancelButton = new Button ( "Cancel");
      okButton = new Button("Ok");
      button_row.add(okButton);
      button_row.add(cancelButton);
      bottom.add(button_row, BorderLayout.SOUTH);
      add(top);
      cancelButton.addActionListener(this);
      okButton.addActionListener(this);
   }

   public void actionPerformed (ActionEvent actionEvt) {
      if (actionEvt.getSource() == cancelButton) {
         // close the window
         processEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
      }
      else if (actionEvt.getSource() == okButton) {
         int item = area_list_wid.getSelectedIndex();
         selected_item = (Area)area_list.elementAt(item);
         if (selected_item != null) {
	    processEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
         }
      }
      else if (actionEvt.getSource() == area_list_wid) {
         System.err.println("List selected.");
         int item = area_list_wid.getSelectedIndex();
         selected_item = (Area)area_list.elementAt(item);
         info_area.setText(selected_item.cs_desc);
      }
   }

  // What to do if the windowListener hears something
   public void windowClosing(WindowEvent we) {
      this.dispose(); 
   }
   public void windowActivated(WindowEvent we) { }
   public void windowDeactivated(WindowEvent we) { }
   public void windowDeiconified(WindowEvent we) { }
   public void windowClosed(WindowEvent we) { }
   public void windowIconified(WindowEvent we) { }
   public void windowOpened(WindowEvent we) { }


   public static void main ( String [] args) {
   /*
      SelectArea f = new SelectArea (" Foo! ");
      f.resize(200,300);
      f.setLocation(200,200);
      f.show();
   */
   }
}

