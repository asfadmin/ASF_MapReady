/* ****************************************************************************
NAME: textFrame.java

DESCRIPTION:
   Display text output of commands executed from SProCKET's command window.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
            07/03  P. Denny     Split bloated pvs.java up into
                                 files named after their classes
            07/03  P. Denny     Replaced depricated action and handleEvent
                                 methods with appropriate Listeners

**************************************************************************** */

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//COMMAND OUTPUT WINDOW

class textFrame extends Frame implements ActionListener, WindowListener {
   String directory;
   TextArea textarea;
   Button closeButton;
   
   public textFrame(String directory, String filename) {
      super(filename);
      
      this.setBackground(Color.lightGray);
      textarea = new TextArea("", 24, 80);
      textarea.setFont(new Font("MonoSpaced", Font.PLAIN, 12));
      textarea.setEditable(false);
      this.add("Center", textarea);
      
      Panel p = new Panel();
      p.add(closeButton = new Button("close"));
      p.setBackground(Color.lightGray);
      this.add("South", p);      

      // Listening Stuff
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
         this.setTitle(filename + " - " + directory);
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
   
   public void actionPerformed (ActionEvent actEvent)
   {
     // will only close this window
     if (actEvent.getSource()==closeButton)
       processEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
   }

   public void windowClosing(WindowEvent e) { 
      this.dispose(); 
   } 
   public void windowActivated(WindowEvent e) { }
   public void windowDeactivated(WindowEvent e) { }
   public void windowDeiconified(WindowEvent e) { }
   public void windowClosed(WindowEvent e) { }
   public void windowIconified(WindowEvent e) { }
   public void windowOpened(WindowEvent e) { }  
}

