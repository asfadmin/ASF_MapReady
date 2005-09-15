/* ****************************************************************************
NAME: logger.java

DESCRIPTION:
   Write a log file specifying any warnings/errors.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
            07/03  P. Denny     Split bloated pvs.java up into
                                 files named after their classes
            07/03  P. Denny     Replaced depricated action and handleEvent
                                 methods with appropriate Listeners

**************************************************************************** */

import java.util.*;
import java.io.*;
import java.text.DateFormat;

class logger {
   static String preffile = "pvs.pref";   //echoed in pvs()...
   static String end = System.getProperty("line.separator");
   static String logfile = null;
   
   public static void log (String statement) {   //maybe i should display the exception also?
      Date dt = new Date();
      
      //get the log filename from the preferences file

      File f;
      boolean foundFLAG = false;
      
      if((f = new File("pvs.pref")).exists()) {
         foundFLAG = true;
      }
      else {
         StringTokenizer s = new StringTokenizer(System.getProperty("java.class.path"), File.pathSeparator);
         while(s.hasMoreTokens()) {
            if((f = new File(s.nextToken(), preffile)).exists()) {
               foundFLAG = true;
               break;
            }
         }
      }

      if(!foundFLAG) {return;}
      
      try {
         FileReader fr = new FileReader(f);
         LineNumberReader in = new LineNumberReader(fr);
      
         boolean lf = false;
         while(!lf) {
            String thisline = in.readLine();
            StringTokenizer s = new StringTokenizer(thisline);
            while(s.hasMoreTokens()) {
               if(s.nextToken().equalsIgnoreCase("logfile")) {
                  logfile = thisline.substring(thisline.indexOf(s.nextToken()));
                  lf = true;
                  break;
               }
            }
         }
         fr.close();
      }
      catch (Exception e){
         System.err.println(e);
         System.out.println("preferences? i thought you said praeter naturam!");
      }
      
      //write to the logfile
      
      if(logfile == null)
         logfile = "pvs.log";
      
      try {
         System.err.println(DateFormat.getDateInstance().format(dt) + " " + statement );
         FileWriter to = new FileWriter(logfile, true);
         to.write(DateFormat.getDateInstance().format(dt) + " " + statement + end);
         to.flush();
         to.close();
      }
      catch(IOException e) {
         System.err.println(e);
         System.out.println("uh, houston. . . i think we have a problem.");
      }
   }
}

