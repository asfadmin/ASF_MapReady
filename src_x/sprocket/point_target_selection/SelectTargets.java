import java.util.* ;
import java.io.* ;
import java.awt.* ;
import java.awt.event.* ;

public class SelectTargets extends Frame implements ActionListener,
                                                    ItemListener,
                                                    WindowListener {
   TextField db_comments;
   Button okButton, cancelButton;
   ArrayList targets, target_boxes;
   PrintStream printer;

   public SelectTargets (String title , Map m, String center_gmt, String gnd_targ_file_name)
   {
      super(title);

      // Use the border layout 
      setLayout(new BorderLayout());

      // The main point target display 
      ScrollPane sp = new ScrollPane();
      add(sp, BorderLayout.CENTER);

      // Label for the main target display 
      add(new Label(
         "Data take center GMT is " + center_gmt),
         BorderLayout.NORTH);

      Panel p = new Panel();
      p.setLayout(new GridLayout(m.size()+1,1));
      sp.add(p);

      // Allocate space for arrays -
      //    targets      - a list of all the targets
      //    target_boxes - a list of all the target check boxes
      targets = new ArrayList();
      target_boxes = new ArrayList();

      // Column labels for main target display
      Panel targetLabelsPanel = new Panel(new GridLayout(1, 5 ));
      targetLabelsPanel.add(new Label("Target"));
      targetLabelsPanel.add(new Label("Date of Maintance"));
      targetLabelsPanel.add(new Label());
      targetLabelsPanel.add(new Label());
      targetLabelsPanel.add(new Label());
      targetLabelsPanel.add(new Label());
      p.add(targetLabelsPanel);

      // Create the main display of targets to choose from
      for (Iterator i=m.values().iterator();  i.hasNext();  ) {
         Panel targetsPanel = new Panel(new BorderLayout());
         Panel targetDatesPanel = new Panel(new GridLayout(1, 5));
         CheckboxGroup cbg = new CheckboxGroup();
         java.util.List targetsList = (java.util.List)i.next();
         int a;

         // Add the "None" flag for no targets 
         targetDatesPanel.add(new Checkbox("None", cbg, true));

         // Add up to 5 of the targets to the panel 
         for (a = 0; (a < targetsList.size()) && (a < 5); a++) {
            Checkbox tmp = new Checkbox(((Target)targetsList.get(a)).cs_maint_date,
                                          cbg, false);
            targetDatesPanel.add(tmp);

            // Save Target and associated checkbox representing the target 
            target_boxes.add(tmp);
            targets.add(targetsList.get(a));

	    // Listen for when the checkbox is clicked on
            tmp.addItemListener(this);
         }

         // To even out spacing of targets, backfill with empty dates 
         for ( ; a < 5; a++)
            targetDatesPanel.add(new Label());

         // Create label for target 
         TextField lb = new TextField(((Target)targetsList.get(0)).cs_dev_id);
         lb.setEditable(false);
         lb.setColumns(10);
         targetsPanel.add(lb, BorderLayout.WEST);
         targetsPanel.add(targetDatesPanel, BorderLayout.CENTER);
         
         p.add(targetsPanel);
      }

      // Create lower panel - has okay/cancel buttons and displays db comments
      Panel tt = new Panel(new GridLayout(2,1));
      Panel buttons = new Panel();
      okButton = new Button("Ok");
      cancelButton = new Button("Cancel");
      db_comments = new TextField();
      buttons.add(okButton);
      buttons.add(cancelButton);
      db_comments.setEditable(false);
      tt.add(db_comments);
      tt.add(buttons);
      add(tt, BorderLayout.SOUTH);

      try {
      	 printer = new PrintStream(new FileOutputStream(gnd_targ_file_name));
      }
      catch ( IOException  e) {
         System.err.println("An error occurred while reading from the file \""
                            + gnd_targ_file_name +"\" (" + e +")");
      }

      // Add listeners
      cancelButton.addActionListener(this);
      okButton.addActionListener(this);
      addWindowListener(this);
   }

   /* ********************************************************
    * Writes out the targets... i don't know about quitting */
   private void WriteAndQuit() {
      for (int a=0; a<targets.size(); a++) {
         if (((Checkbox)target_boxes.get(a)).getState()) {
            ((Target)targets.get(a)).print(printer);
         }
      }
   }

   /* ***************************************************
    * What to do if the ActionListener hears something */
   public void actionPerformed (ActionEvent actionEvt) {
      if (actionEvt.getSource() == cancelButton) {
         processEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
      }
      else if (actionEvt.getSource() == okButton) {
         WriteAndQuit();
         processEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
      }
   }
   
   /* *************************************************
    * What to do if the ItemListener hears something */
   public void itemStateChanged (ItemEvent itemEvt) {
       // Check to see if it was one of the check boxes. If possible,
       // fill the textfield the database comments. Couldn't get this
       // to work with ArrayList's 'contains()' method, so it had to
       // be done with a for loop and if statement :(
      Object cb = itemEvt.getItem();
      for (int ii=0; ii<target_boxes.size(); ii++) {
	  if (((Checkbox)target_boxes.get(ii)).getLabel() == cb) {
	      db_comments.setText(((Target)targets.get(ii)).cs_cmnts);
	  }
      }
   }

   /* ***************************************************
    * What to do if the windowListener hears something */
   public void windowClosing(WindowEvent we) {
      this.dispose();
      System.exit(0);
   }
   public void windowActivated(WindowEvent we) { }
   public void windowDeactivated(WindowEvent we) { }
   public void windowDeiconified(WindowEvent we) { }
   public void windowClosed(WindowEvent we) { }
   public void windowIconified(WindowEvent we) { }
   public void windowOpened(WindowEvent we) { }
}
