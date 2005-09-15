/* *****************************************************************************
NAME: helpFrame.java

DESCRIPTION:
   Window that displays a little bit about SProCKET.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
            07/03  P. Denny     Split bloated pvs.java up into
                                 files named after their classes
            07/03  P. Denny     Replaced depricated action and handleEvent
                                 methods with appropriate Listeners

***************************************************************************** */

import java.awt.*;
import java.awt.event.*;

//ABOUT DIALOG

class aboutDialog extends Dialog implements ActionListener, WindowListener {
   Button closeButton;

   aboutDialog (Frame parent) {
      super (parent, "About SProCKET");
      this.setBackground(Color.lightGray);

      Panel aboutpan = new Panel();
      aboutpan.setLayout(new GridLayout(7, 1));
      aboutpan.setBackground(Color.lightGray);
      aboutpan.add(new Label("Alaska Satellite Facility",Label.CENTER));
      aboutpan.add(new Label("Sar PROessing Calibration Kit",Label.CENTER));
      aboutpan.add(new Label("and Evaluation Tool",Label.CENTER));
      aboutpan.add(new Label(" "));
      aboutpan.add(new Label("Interface and noise: Dann Toliver",Label.CENTER));
      aboutpan.add(new Label("Back-end and toys: Jay Cable",Label.CENTER));
      aboutpan.add(new Label("Copyright 2000-2003",Label.CENTER));
      add("Center", aboutpan);

      Panel south = new Panel();
      closeButton = new Button ("Close");
      south.add(closeButton);
      add ("South", south);
      
      // Listen for input
      closeButton.addActionListener(this);
      addWindowListener(this);

      setResizable(false);
      pack();
   }

   public void actionPerformed (ActionEvent actionEvt)
   {
     // will only close this window
     if (actionEvt.getSource() == closeButton)
       processEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
   }

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

