/* *****************************************************************************
NAME: helpFrame.java

DESCRIPTION:
   Window that displays the help.

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
import java.awt.image.*;
import java.util.*;
import java.io.*;


//HELP WINDOW

class helpFrame extends Frame implements ActionListener, WindowListener {
   String directory;
   TextArea textarea;
   Button generalButton, maskButton, smaskButton,
          targetsButton, commandsButton, closeButton;

   public helpFrame(String directory, String filename) {
      super("SProCKET Help");
      
      this.setBackground(Color.lightGray);
      textarea = new TextArea("", 40, 40);
      textarea.setFont(new Font("Serif", Font.PLAIN, 14));
      textarea.setEditable(false);
      this.add("Center", textarea);
      
      Panel p = new Panel();
      p.add(generalButton = new Button("General"));
      p.add(maskButton = new Button("Mask"));
      p.add(smaskButton = new Button("Smask"));
      p.add(targetsButton = new Button("Targets"));
      p.add(commandsButton = new Button("Commands"));
      p.add(closeButton = new Button("Close"));
      p.setBackground(Color.lightGray);
      this.add("South", p);
      
      // Listen for button presses
      generalButton.addActionListener(this);
      maskButton.addActionListener(this);
      smaskButton.addActionListener(this);
      targetsButton.addActionListener(this);
      commandsButton.addActionListener(this);
      closeButton.addActionListener(this);
      addWindowListener(this);

      if(directory == null) {
         File f;
         if((filename != null) && (f = new File(filename)).isAbsolute()) {
            directory = f.getParent();
            filename = f.getName();
         }
         else directory = System.getProperty("user.dir");
      }
      
      this.directory = directory;
      setFile(directory, filename);
      int locatex = getToolkit().getScreenSize().width  - getWidth();
      int locatey = getToolkit().getScreenSize().height - getHeight();
      if(locatex > 0 && locatey > 0)
         setLocation(2*locatex/3, locatey/8);
   }
   
   public void setFile(String directory, String filename) {
      if((filename == null) || (filename.length() == 0)) {return;}
      
      File f;
      FileReader in = null;
      
      try {
         f = new File(directory, filename);
         in = new FileReader(f);
         int size = (int) f.length();
         char[] data = new char[size];
         int chars_read = 0;
         while(chars_read < size)
            chars_read += in.read(data, chars_read, size-chars_read);
         textarea.setText(new String(data));
      }
      catch(Exception e) {
         textarea.setText("error opening file: " + directory + filename);
         logger.log("error opening file: " + directory + filename);
      }
      finally {
         try {
            if(in != null)
               in.close();
         }
         catch(IOException e) {}
      }
   }
   
   public void actionPerformed (ActionEvent actionEvt)
   {
      if (actionEvt.getSource()==generalButton) {
         setFile(directory, "general.help");
      }
      if (actionEvt.getSource()==maskButton) {
         setFile(directory, "mask.help");
      }
      if (actionEvt.getSource()==smaskButton) {
         setFile(directory, "smask.help");
      }
      if (actionEvt.getSource()==targetsButton) {
         setFile(directory, "targets.help");
      }
      if (actionEvt.getSource()==commandsButton) {
         setFile(directory, "commands.help");
      }
      if (actionEvt.getSource()==closeButton) {
         processEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
      }
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

