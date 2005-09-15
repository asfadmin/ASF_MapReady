import java.awt.* ;
import java.awt.event.*;
import java.io.* ;
import java.util.* ;

public class SelectArea extends Dialog implements ActionListener,
                                                  ItemListener,
                                                  WindowListener {
   protected Vector area_list;
   protected java.awt.List area_list_wid;
   protected TextField info_area;
   protected Button cancelButton, okButton;
   protected Area selected_item;

   /*******************************************************************
    * Constructor:
    * Create a window with a list of calibration sites available in the
    * database (nabbed with list_sites.sql script)                    */
   public SelectArea (String title, Frame f) {
      super(f, title, true);
      /* Structure window */
      Panel top, bottom, button_row;
      top = new Panel(new BorderLayout());
      bottom = new Panel(new BorderLayout());
      button_row = new Panel();
      top.add(bottom, BorderLayout.SOUTH);

      top.add(new Label ("Select calibration site"), BorderLayout.NORTH);

      // Obtain the possible areas
      Spawner S = new Spawner ("list_sites.sql");
      AreaReader r = new AreaReader (S.In);
      area_list = r.getElements();
         
      // Display the Area's names
      area_list_wid = new java.awt.List ( area_list.size() );
      for (int ii = 0; ii < area_list.size(); ii++) {
         area_list_wid.add(((Area)area_list.elementAt(ii)).cs_name);
      }
      top.add(area_list_wid, BorderLayout.CENTER);

      // Create a text box to display database comments
      info_area = new TextField();
      info_area.setEditable(false);
      bottom.add(info_area, BorderLayout.CENTER);

      // Add buttons to cancel or execute
      cancelButton = new Button ( "Cancel");
      okButton = new Button("Ok");
      button_row.add(okButton);
      button_row.add(cancelButton);
      bottom.add(button_row, BorderLayout.SOUTH);
      add(top);

      // Activate listeners
      area_list_wid.addItemListener(this);
      cancelButton.addActionListener(this);
      okButton.addActionListener(this);
      addWindowListener(this);
   }

   /****************************************************
    * getSelectedItem:
    * Return the site selected from the list of sites */
   public Area getSelectedItem() {
      return selected_item;
   }

   /*****************************************************
    * What to do if the ActionListener hears something */
   public void actionPerformed (ActionEvent actionEvt) {
      // close the window
      if (actionEvt.getSource() == cancelButton) {
         processEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
         System.exit(0);
      }
      // Get the info for the selected item in the list & close the window
      else if (actionEvt.getSource() == okButton) {
         int item = area_list_wid.getSelectedIndex();
         selected_item = (Area)area_list.elementAt(item);
         if (selected_item != null) {
            processEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
         }
      }
   }

   /***************************************************
    * What to do if the ItemListener hears something */
   public void itemStateChanged (ItemEvent itemEvt) {
      // Display comments about selected list item in the comments box
      if (itemEvt.getStateChange() == ItemEvent.SELECTED) {
         int item = area_list_wid.getSelectedIndex();
         selected_item = (Area)area_list.elementAt(item);
         info_area.setText(selected_item.cs_desc);
      }
   }

   /*****************************************************
    * What to do if the windowListener hears something */
   public void windowClosing(WindowEvent we) {
      this.dispose(); 
   }
   public void windowActivated(WindowEvent we) { }
   public void windowDeactivated(WindowEvent we) { }
   public void windowDeiconified(WindowEvent we) { }
   public void windowClosed(WindowEvent we) { }
   public void windowIconified(WindowEvent we) { }
   public void windowOpened(WindowEvent we) { }

}

