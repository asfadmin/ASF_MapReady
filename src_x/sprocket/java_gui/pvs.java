/* *****************************************************************************
NAME: pvs.java

DESCRIPTION:
	Main SProCKET window. This where the SProCKET program is driven
	Was once known as pvs (process validation system)... is better known as
	the ASF's ultracool imageviewing application.

PROGRAM HISTORY:
    VERS:  DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0   06/16/00  Dan Toliver  i'm thinking of adding a changes log. it will
                                   look something like this
    0.1   05/31/00  D. Toliver   delivered beta1 to operators, to rave reviews
                                   and cries of "take off your shirt!". this
                                   will be considered an auspicious start.
    0.2   06/13/00  D. Toliver   delivered beta2 to operators complete with
                                   process meters, glowing blue box, and
                                   enhanced flythrough mode. also updated
                                   (rewrote) the help files, and added some
                                   cosmetic polish.
    0.3   06/15/00  D. Toliver   made popup things popup in nice places; added
                                   keyboard shortcuts to the menus, an about
                                   box, and this changes log. eating yogurt.
    0.4   06/16/00  D. Toliver   added 'best quality' mode. this is sick, dog.
          06/19/00  D. Toliver   targets don't move around nearly as much
                                   anymore. also, the yellowing accuracy has
                                   increased dramatically.
    0.5   06/25/00  D. Toliver   pvs exports images from the zoom window now. it
                                   can auto export when ever the scene changes,
                                   generating sequences of pgm image data that
                                   can be synthesized into MPEGs.
    0.6   06/29/00  D. Toliver   ingests asf data. hooray.
    0.7   07/20/00  D. Toliver   last day of work on pvs. completed the site
                                   where it will rest for the forseeable future.
                                   fixed numerous problems - wiggling smasks,
                                   menu disabling at inappropriate moments,
                                   various aesthetic faux pas. turning the
                                   project over to the capable hands of jay
                                   cable. it's been fun while it lasted. hope
                                   you enjoy it.
    0.71  07/03     P. Denny     Split bloated pvs.java up into
                                   files named after their classes
    0.8   07/03     P. Denny     Replaced depricated action and handleEvent
                                   methods with appropriate Listeners

***************************************************************************** */

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


class pvs extends Frame implements ActionListener, ComponentListener,
                                   ItemListener, MouseListener,
                                   MouseMotionListener, WindowListener {
   imageCanvas mainCanvas; // Try to keep track of everything at this level
   zoomFrame zf;           // Zoomframe: this has it's own canvas
   importImage imageObject;// The far too complicated image object
   mask imagemask;         // el mask, natch.
   helpFrame help;
   int locatex;
   int locatey;
   int pensize = 7;
   int imagesize = 400;
   int imagequality = 1;
   Panel mtools;
   boolean unmask = false;
   private int zoommaskcount = 0;
   targetFrame targetframe;
   boolean mouseListenerOn = false;   // Turn mouse listeners on & off

// Preferences and whatnot...
   String preffile = "pvs.pref";   //echoed in logger class...
   String imagedir = null;
   String maskdir = null;
   String helpdir = null;
   String logfile = null;    //actually, this gets eaten in the log class itself.
   
   String xy_plotter = null; //to make another command: put a variable here,
                             //add a menu option, add a line to the pref file,
                             //copy the xy_plotter section of the pref file opener
                             //and alter it for the new command, then do the same
                             //with the xy_plotter section of the 'action' function.

   String outputfile = null;
   
// Menuing-ing-ing
   
   Menu fileMenu;
   Menu optionMenu;
   Menu maskMenu;
   Menu commandMenu;
   Menu targetMenu;
   Menu helpMenu;
   
   MenuItem openMenuItem;
   MenuItem printMenuItem;
   MenuItem exitMenuItem;

   MenuItem zoomMenuItem;
   MenuItem contrastMenuItem;
   
   MenuItem enterMaskMenuItem;
   MenuItem startMaskMenuItem;
   MenuItem stopMaskMenuItem;
   MenuItem exitMaskMenuItem;
   CheckboxMenuItem unmaskMenuItem;
   MenuItem importMaskMenuItem;
   MenuItem exportMaskMenuItem;
   
   MenuItem outputterMenuItem;
   MenuItem otherMenuItem;
   
   MenuItem enterTargetsMenuItem;
   MenuItem endTargetsMenuItem;
   CheckboxMenuItem brightMenuItem;
   MenuItem exitTargetsMenuItem;
   MenuItem importTargetsMenuItem;
   MenuItem exportTargetsMenuItem;
   
   MenuItem generalHelpMenuItem;
   MenuItem maskHelpMenuItem;
   MenuItem smaskHelpMenuItem;
   MenuItem targetsHelpMenuItem;
   MenuItem commandsHelpMenuItem;
   MenuItem aboutHelpMenuItem;

   Button penSizeButton1;
   Button penSizeButton3;
   Button penSizeButton7;
   Button penSizeButton10;
   Button penSizeButton20;

   private Properties printprefs = new Properties(); //for printing

   //  *************************************************************************
   pvs() {
      super("SProCKET");
      
      MenuBar mb = new MenuBar();

      addWindowListener(this);

      fileMenu    = new Menu("File");
      optionMenu  = new Menu("Option");
      maskMenu    = new Menu("Mask");
      commandMenu = new Menu("Commands");
      targetMenu  = new Menu("Targets");
      helpMenu    = new Menu("Help");

   // File menu
      openMenuItem  = new MenuItem("Open", new MenuShortcut(KeyEvent.VK_O));
      printMenuItem = new MenuItem("Print");
      exitMenuItem  = new MenuItem("Exit");
      fileMenu.add(openMenuItem);
      fileMenu.add(printMenuItem);
      fileMenu.addSeparator();
      fileMenu.add(exitMenuItem);
      openMenuItem.addActionListener(this);
      printMenuItem.addActionListener(this);
      exitMenuItem.addActionListener(this);

   // Option menu
      zoomMenuItem     = new MenuItem("Zoom window",new MenuShortcut(KeyEvent.VK_Z));
      contrastMenuItem = new MenuItem("Contrast",new MenuShortcut(KeyEvent.VK_C));
      optionMenu.add(zoomMenuItem);
      optionMenu.add(contrastMenuItem);
      zoomMenuItem.addActionListener(this);
      contrastMenuItem.addActionListener(this);

   // Mask menu
      enterMaskMenuItem  = new MenuItem("Enter mask mode",new MenuShortcut(KeyEvent.VK_M));
      startMaskMenuItem  = new MenuItem("Start masking");
      stopMaskMenuItem   = new MenuItem("Stop masking");
      exitMaskMenuItem   = new MenuItem("Exit mask mode");
      unmaskMenuItem     = new CheckboxMenuItem("Unmask mode");
      importMaskMenuItem = new MenuItem("Import mask");
      exportMaskMenuItem = new MenuItem("Export mask");
      maskMenu.add(enterMaskMenuItem);
      maskMenu.add(startMaskMenuItem);
      maskMenu.add(stopMaskMenuItem);
      maskMenu.add(exitMaskMenuItem);
      maskMenu.addSeparator();
      maskMenu.add(unmaskMenuItem);
      maskMenu.addSeparator();
      maskMenu.add(importMaskMenuItem);
      maskMenu.add(exportMaskMenuItem);
      unmaskMenuItem.addItemListener(this);
      enterMaskMenuItem.addActionListener(this);
      startMaskMenuItem.addActionListener(this);
      stopMaskMenuItem.addActionListener(this);
      exitMaskMenuItem.addActionListener(this);
      importMaskMenuItem.addActionListener(this);
      exportMaskMenuItem.addActionListener(this);

   // Command menu
      outputterMenuItem = new MenuItem("xy plotter");
      otherMenuItem     = new MenuItem("Other...");
      commandMenu.add(outputterMenuItem);
      commandMenu.addSeparator();
      commandMenu.add(otherMenuItem);
      outputterMenuItem.addActionListener(this);
      otherMenuItem.addActionListener(this);

   // Targets menu
      enterTargetsMenuItem  = new MenuItem("enter target mode", new MenuShortcut(KeyEvent.VK_T));
      endTargetsMenuItem    = new MenuItem("exit target mode");
      brightMenuItem        = new CheckboxMenuItem("brightest pixel", true);
      importTargetsMenuItem = new MenuItem("import targets");
      exportTargetsMenuItem = new MenuItem("export targets");
      targetMenu.add(enterTargetsMenuItem);
      targetMenu.add(endTargetsMenuItem);
      targetMenu.addSeparator();
      targetMenu.add(brightMenuItem);
      targetMenu.addSeparator();
      targetMenu.add(importTargetsMenuItem);
      targetMenu.add(exportTargetsMenuItem);
      enterTargetsMenuItem.addActionListener(this);
      endTargetsMenuItem.addActionListener(this);
      brightMenuItem.addItemListener(this);
      importTargetsMenuItem.addActionListener(this);
      exportTargetsMenuItem.addActionListener(this);

   // Help menu
      generalHelpMenuItem  = new MenuItem("General");
      maskHelpMenuItem     = new MenuItem("Mask");
      smaskHelpMenuItem    = new MenuItem("Smask");
      targetsHelpMenuItem  = new MenuItem("Targets");
      commandsHelpMenuItem = new MenuItem("Commands");
      aboutHelpMenuItem    = new MenuItem("About");
      helpMenu.add(generalHelpMenuItem);
      helpMenu.add(maskHelpMenuItem);
      helpMenu.add(smaskHelpMenuItem);
      helpMenu.add(targetsHelpMenuItem);
      helpMenu.add(commandsHelpMenuItem);
      helpMenu.addSeparator();
      helpMenu.add(aboutHelpMenuItem);
      generalHelpMenuItem.addActionListener(this);
      maskHelpMenuItem.addActionListener(this);
      smaskHelpMenuItem.addActionListener(this);
      targetsHelpMenuItem.addActionListener(this);
      commandsHelpMenuItem.addActionListener(this);
      aboutHelpMenuItem.addActionListener(this);

   // Menu bar
      mb.setHelpMenu(helpMenu);
      mb.add(fileMenu);
      mb.add(optionMenu);
      mb.add(maskMenu);
      mb.add(commandMenu);
      mb.add(targetMenu);
      mb.add(helpMenu);
      setMenuBar(mb);

      mainCanvas = new imageCanvas(this);

   // Masking buttons
      mtools = new Panel();
      mtools.setLayout(new GridLayout(5, 1));
      penSizeButton1  = new Button ("1");
      penSizeButton3  = new Button ("3");
      penSizeButton7  = new Button ("7");
      penSizeButton10 = new Button ("10");
      penSizeButton20 = new Button ("20");
      mtools.add(penSizeButton1);
      mtools.add(penSizeButton3);
      mtools.add(penSizeButton7);
      mtools.add(penSizeButton10);
      mtools.add(penSizeButton20);
    
      this.add("Center", mainCanvas);
      optionMenuEnabler(false);
      commandMenuEnabler(false);
      maskMenuEnabler(false);
      targetMenuEnabler(false);
      
      preferences();
      
      setResizable(false);
      setSize(imagesize + 8, imagesize + 46);

      locatex = (getToolkit().getScreenSize().width  - this.getWidth())/3;
      locatey = (getToolkit().getScreenSize().height - this.getHeight())/3;
      if(locatex > 0 && locatey > 0)
         setLocation(locatex, locatey);
      else
         setLocation(1,1);

      setVisible(true);
   }

   //  *************************************************************************
   public void optionMenuEnabler(boolean b) {
      printMenuItem.setEnabled(b);
      zoomMenuItem.setEnabled(b);
      contrastMenuItem.setEnabled(b);
   }
   
   //  *************************************************************************
   public void maskMenuEnabler(boolean b) {
      enterMaskMenuItem.setEnabled(b);
      importMaskMenuItem.setEnabled(b);
      if(!b)
         maskFunctionsMenuEnabler(b);
   }

   //  *************************************************************************
   public void maskFunctionsMenuEnabler(boolean b) {
      startMaskMenuItem.setEnabled(b);
      stopMaskMenuItem.setEnabled(b);
      exitMaskMenuItem.setEnabled(b);
      unmaskMenuItem.setEnabled(b);
      exportMaskMenuItem.setEnabled(b);
   }

   //  *************************************************************************
   public void commandMenuEnabler(boolean b) {
      outputterMenuItem.setEnabled(b);
      otherMenuItem.setEnabled(b);
   }
   
   //  *************************************************************************
   public void targetMenuEnabler(boolean b) {
      enterTargetsMenuItem.setEnabled(b);
      if(!b)
         targetFunctionsMenuEnabler(b);
   }
   
   //  *************************************************************************
   public void targetFunctionsMenuEnabler(boolean b) {
      endTargetsMenuItem.setEnabled(b);      
      brightMenuItem.setEnabled(b);      
      importTargetsMenuItem.setEnabled(b);      
      exportTargetsMenuItem.setEnabled(b);      
   }
   
   //  *************************************************************************
   public void preferences() {
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
      
         boolean id = false;
         while(!id) {
            String thisline = in.readLine();
            StringTokenizer s = new StringTokenizer(thisline);
            while(s.hasMoreTokens()) {
               if(s.nextToken().equalsIgnoreCase("imagedir")) {
                  imagedir = thisline.substring(thisline.indexOf(s.nextToken()));
                  id = true;
                  break;
               }
            }
         }

         boolean md = false;
         while(!md) {
            String thisline = in.readLine();
            StringTokenizer s = new StringTokenizer(thisline);
            while(s.hasMoreTokens()) {
               if(s.nextToken().equalsIgnoreCase("maskdir")) {
                  maskdir = thisline.substring(thisline.indexOf(s.nextToken()));
                  md = true;
                  break;
               }
            }
         }

         boolean hd = false;
         while(!hd) {
            String thisline = in.readLine();
            StringTokenizer s = new StringTokenizer(thisline);
            while(s.hasMoreTokens()) {
               if(s.nextToken().equalsIgnoreCase("helpdir")) {
                  helpdir = thisline.substring(thisline.indexOf(s.nextToken()));
                  hd = true;
                  break;
               }
            }
         }

         boolean lf = false;   //the logfile variable doesn't actually get used here. . . (yet)
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

         boolean xy = false;
         while(!xy) {
            String thisline = in.readLine();
            StringTokenizer s = new StringTokenizer(thisline);
            while(s.hasMoreTokens()) {
               if(s.nextToken().equalsIgnoreCase("xy_plotter")) {
                  xy_plotter = thisline.substring(thisline.indexOf(s.nextToken()));
                  xy = true;
                  break;
               }
            }
         }

         boolean of = false;
         while(!of) {
            String thisline = in.readLine();
            StringTokenizer s = new StringTokenizer(thisline);
            while(s.hasMoreTokens()) {
               if(s.nextToken().equalsIgnoreCase("outputfile")) {
                  outputfile = thisline.substring(thisline.indexOf(s.nextToken()));
                  of = true;
                  break;
               }
            }
         }
         
         fr.close();
      }
      catch (Exception e){
         logger.log("error opening preferences file.");
      }   
   }
   
   //  *************************************************************************
   public void newcontrast(int low, int high) {
      imageObject.colourarray(low, high);
      imageObject.newcontrast = true;
      imageObject.recolour();
      mainCanvas.updateImage(imageObject);
      if (zf instanceof zoomFrame) 
         zf.zoomedCanvas.repaint();   
   }

   //  *************************************************************************
   public void exitTargets() {
      targetFunctionsMenuEnabler(false);
      
      targetframe.dispose();
      
      zf.smaskMenuEnabler(true);
      maskMenuEnabler(true);
      
      mainCanvas.updateTargets(null);
      zf.updateTargets(null);   
   }

   //  *************************************************************************
   public void commander(String commanddirectory, String commandfilename) {   //(this is stupid.)
       //cut of the .img part
      String imagefile = imageObject.filename.substring(0, imageObject.filename.length() - 4);
      String maskfile;
      String outputfilename;
      String outputdirectory;
      procDialog pd = null;
      
      if(outputfile != null) {
         outputdirectory = outputfile.substring(0, outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);
         outputfilename = outputfile.substring(outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);      
      }
      else {
         outputdirectory = commanddirectory;
         outputfilename = "output.txt";
      }
      
      if(!maskdir.endsWith(System.getProperty("file.separator")))
          maskdir = maskdir + System.getProperty("file.separator");
      maskfile = maskdir + "default.mask";
      
      if(imagemask != null)
         maskfile = imagemask.filename;
      else if((zf != null) && (zf.zoomedCanvas.simagemask != null))
         maskfile = zf.zoomedCanvas.simagemask.filename;
      
      commDialog comm = new commDialog(this, maskfile, imageObject.filename, outputfile);
      if(getToolkit().getScreenSize().width  - comm.getWidth()  > 0 &&
         getToolkit().getScreenSize().height - comm.getHeight() > 0) {
         comm.setLocation((getToolkit().getScreenSize().width  - comm.getWidth())/2, 
                          (getToolkit().getScreenSize().height - comm.getHeight())/2);
      }
      comm.setVisible(true);
      comm.dispose();
      
      if(comm.cancelled) {return;}
               
      //use the return from comm here
      String args[] = new String[8];  //the number should be from commDialog

      if(commanddirectory != null) {args[0] = commanddirectory + commandfilename;}
      else {args[0] = commandfilename;}

      if(comm.imagefile != null) {args[1] = comm.imagefile.substring(0, comm.imagefile.length() - 6);}
      else {args[1] = imagefile;}
      
      if(comm.xthing != null) {args[2] = comm.xthing;}
      else {args[2] = "look";}
      
      if(comm.ything != null) {args[3] = comm.ything;}
      else {args[3] = "power";}
      
      if(comm.mthing != null) {args[4] = comm.mthing;}
      else {args[4] = "on";}
      
      args[5] = Integer.toString(comm.bin);
      
      if(comm.maskfile != null) {args[6] = comm.maskfile;}
      else {args[6] = maskfile;}
      
      if(comm.outputfile != null) {
         args[7] = comm.outputfile;
         outputdirectory = comm.outputfile.substring(0, comm.outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);
         outputfilename = comm.outputfile.substring(comm.outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);
      }
      else {args[7] = outputfilename;}
            
      setCursor(new Cursor(Cursor.WAIT_CURSOR));
      
      try {
         Runtime run = Runtime.getRuntime();
         Process pr = run.exec(args);

         //process meter stuff
         pd = new procDialog(this, "running " + commandfilename + "...");   
         if(locatex > 0 && locatey > 0)
            pd.setLocation(locatex + 3, locatey + 41);   //arbitrary... yuck.
         pd.setVisible(true);
         int done = 0;

         Reader reader = new BufferedReader(new InputStreamReader(pr.getInputStream()));
         StreamTokenizer st = new StreamTokenizer(reader);
         try {
            while(done < 99) {
               if(st.nextToken() == st.TT_NUMBER) {
                  if((int) st.nval > done) {
                     done = (int) st.nval;
                     pd.process(done);
                  }
               }
            }
         }
         catch(IOException e) {
            System.out.println("stupid." + e);
            pr.waitFor();      //uhm. . . right.            
         }
         pr.waitFor(); 
      }
      catch(Exception e) {
         logger.log("error running command: " + args[0]);
      }
      finally {
         if(pd != null)
            pd.dispose();
         setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
      }
      
      textFrame text = new textFrame(outputdirectory, outputfilename);
      text.pack();
      if(getToolkit().getScreenSize().width  - text.getWidth()  > 0 &&
         getToolkit().getScreenSize().height - text.getHeight() > 0) {
            text.setLocation((getToolkit().getScreenSize().width  - text.getWidth())/2, 
                             (getToolkit().getScreenSize().height - text.getHeight())/2);
      }
      text.setVisible(true);
   }
      
   // ACTION LISTENER ROUTINE **************************************************
   public void actionPerformed (ActionEvent actionEvt) {

   // FILE MENU OPTIONS ****************
      if (actionEvt.getSource() == openMenuItem) {
         prefDialog pref = new prefDialog(this, imagesize, imagequality);
         pref.pack();
         if(locatex > 0 && locatey > 0)
            pref.setLocation(locatex + 3, locatey + 41);  
         pref.setVisible(true);
         pref.dispose();  

         if (pref.cancelled) {return;}

         if(pref.cksize.getSelectedCheckbox().getLabel() == "200 x 200") 
            imagesize = 200;
         else if(pref.cksize.getSelectedCheckbox().getLabel() == "400 x 400")
            imagesize = 400;
         else if(pref.cksize.getSelectedCheckbox().getLabel() == "800 x 800")
            imagesize = 800;

         if(pref.ckqual.getSelectedCheckbox().getLabel() == "good") 
            imagequality = 1;
         else if(pref.ckqual.getSelectedCheckbox().getLabel() == "better")
            imagequality = 9;
         else if(pref.ckqual.getSelectedCheckbox().getLabel() == "best")
            imagequality = 0;

         FileDialog f = new FileDialog(this, "Load image", FileDialog.LOAD);
         if(imagedir != null)
            f.setDirectory(imagedir);
         f.setVisible(true);
         setCursor(new Cursor(Cursor.WAIT_CURSOR));
         f.dispose();
         String filename = f.getFile();
         if(filename != null) {
            imagedir = f.getDirectory();
            filename = imagedir + filename;

            //close it if it's already there.
            if(imageObject instanceof importImage) {
               imageObject.closeImage(); // returns boolean
               imageObject = null;
            }

            imageObject = new importImage(this,filename,imagesize,imagequality);
            if (imageObject.im == null) {
               setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
               return;
            }

            mainCanvas.updateImage(imageObject);
            if (zf instanceof zoomFrame) 
               zf.repaint();    

            if (targetframe != null)   { exitTargets(); }

            if(imagemask != null) {
               imagemask = null;
               maskFunctionsMenuEnabler(false);
               mainCanvas.clearMask();
               if (zf != null)
                  zf.updateMask(imagemask); 
               this.remove(mtools);
            }

            if (zf != null) {
               if (zf.zoomedCanvas.simagemask != null) {
                  zf.zoomedCanvas.unsmasking();
                  mainCanvas.clearSmask();
                  maskMenuEnabler(true);
                  zf.zoomedCanvas.fly = false;
                  zf.setResizable(true);
               }
            }

            mainCanvas.setSize(imagesize, imagesize);
            setSize(imagesize + 8, imagesize + 46); //yuck!!! do something different here.
         // mainCanvas.setSize(mainCanvas.getPreferredSize());
         // setSize(this.getPreferredSize());

            if((getToolkit().getScreenSize().width  < this.getWidth()  + locatex) ||
               (getToolkit().getScreenSize().height < this.getHeight() + locatey)) {
               locatex = (getToolkit().getScreenSize().width  - this.getWidth())/3;
               locatey = (getToolkit().getScreenSize().height - this.getHeight())/3;
               if (locatex > 0 && locatey > 0)
                  setLocation(locatex, locatey);
               else
                  setLocation(1,1);
            }
            optionMenuEnabler(true);
            commandMenuEnabler(true);
            maskMenuEnabler(true);
         }
         setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
         return;
      } // END if (actionEvt.getSource() == openMenuItem)
      if (actionEvt.getSource() == printMenuItem) {
         mainCanvas.printit(this, printprefs);
         return;
      }         
      if (actionEvt.getSource() == exitMenuItem) {
         processEvent(new WindowEvent(this,WindowEvent.WINDOW_CLOSING));
         return;
      }

   // OPTION MENU OPTIONS **************
      if (actionEvt.getSource() == zoomMenuItem) {
         if (zf instanceof zoomFrame) {
            if (zf.isVisible()) {
               if (imagemask == null && mouseListenerOn) {
                  mainCanvas.removeMouseListener(this);
                  mainCanvas.removeMouseMotionListener(this);
                  mouseListenerOn = false;
               }
               zf.dispose();
            }
            else {
               if (imagemask == null && !mouseListenerOn) {
                  mainCanvas.addMouseListener(this);
                  mainCanvas.addMouseMotionListener(this);
                  mouseListenerOn = true;
               }
               zf.setVisible(true);
            }
         }
         else {
            if (mainCanvas.im != null) {
               zf = new zoomFrame(this);
               if (imagemask == null && !mouseListenerOn) {
                  mainCanvas.addMouseListener(this);
                  mainCanvas.addMouseMotionListener(this);
                  mouseListenerOn = true;
               }
               if (mainCanvas.x != -1 && mainCanvas.y != -1) {
                  zf.zoomImage(mainCanvas.x, mainCanvas.y, imageObject);
               }
               else {
                  zf.zoomImage(imagesize/2, imagesize/2, imageObject);
               }
               if (imagemask != null) {
                  zf.updateMask(imagemask);
                  zf.smaskMenuEnabler(false);
               }
            }
         }
         targetMenuEnabler(true);
         return;
      }
      if (actionEvt.getSource() == contrastMenuItem) {  //display contrast controls
         if (imageObject != null) {
            contDialog cont = new contDialog(this, imageObject.contrastLow,
                                             imageObject.contrastHigh, 
                                             imageObject.defaultContrastLow,
                                             imageObject.defaultContrastHigh);
            if (locatex > 0 && locatey > 0)  {
               if (locatey + this.getHeight() + cont.getHeight() < getToolkit().getScreenSize().height)
                  cont.setLocation(locatex, locatey + this.getHeight());
               else
                  cont.setLocation(locatex + 3, locatey + 41);
            }
            cont.setVisible(true);
            newcontrast(cont.low, cont.high);
         }
         return;
      }

   // MASK MENU OPTIONS ****************
       if (actionEvt.getSource() == enterMaskMenuItem) {  // Allow mask drawing
         if(imageObject != null) {
            if(imagemask == null) {
               imagemask = new mask(this, imagesize, imageObject.width, imageObject.height);
               imagemask.drawing = true;
               this.add("East", mtools);
               setSize(this.getPreferredSize());
               setVisible(true);
               maskFunctionsMenuEnabler(true);
               if(zf != null) {
                  zf.smaskMenuEnabler(false);
                  targetMenuEnabler(false);
               }
            }
            else 
               imagemask.drawing = true;
         }
         penSizeButton1.addActionListener(this);
         penSizeButton3.addActionListener(this);
         penSizeButton7.addActionListener(this);
         penSizeButton10.addActionListener(this);
         penSizeButton20.addActionListener(this);
         return;
      }
      if (actionEvt.getSource() == startMaskMenuItem) {  // Start drawing mask
         if (imageObject != null) {
            if (imagemask != null) {
               if ( !(zf instanceof zoomFrame) && !mouseListenerOn ) {
                  mainCanvas.addMouseListener(this);
                  mainCanvas.addMouseMotionListener(this);
                  mouseListenerOn = true;
               }
               imagemask.drawing = true;
            }
         }
         return;
      }
      if (actionEvt.getSource() == stopMaskMenuItem) {  // Stop drawing mask
         if (imagemask != null) {
            if ( !(zf instanceof zoomFrame) && mouseListenerOn  ) {
               mainCanvas.removeMouseListener(this);
               mainCanvas.removeMouseMotionListener(this);
               mouseListenerOn = false;
            }
            imagemask.drawing = false;
         }
         if (zf != null)
            zf.updateMask(imagemask);
         return;
      }
      if (actionEvt.getSource() == exitMaskMenuItem) {  // Exit mode, destroy mask
         imagemask = null;
         mainCanvas.clearMask();
         if(zf != null)
            zf.updateMask(imagemask);
         this.remove(mtools);
         setSize(this.getPreferredSize());
         maskFunctionsMenuEnabler(false);
         if(zf != null) {
            zf.smaskMenuEnabler(true);
            targetMenuEnabler(true);
         }
         penSizeButton1.removeActionListener(this);
         penSizeButton3.removeActionListener(this);
         penSizeButton7.removeActionListener(this);
         penSizeButton10.removeActionListener(this);
         penSizeButton20.removeActionListener(this);
         return;
      }         
      if (actionEvt.getSource() == importMaskMenuItem) {  // Import mask from bitmap
         if (imageObject != null) {
            FileDialog f = new FileDialog(this, "Import mask", FileDialog.LOAD);
            if (maskdir != null)
               f.setDirectory(maskdir);
            f.setVisible(true);
            f.dispose();
            String filename = f.getFile();
            if (filename != null) {
               filename = f.getDirectory() + filename;         
               imagemask = new mask(this, filename, imagesize, imageObject.width, imageObject.height);
               imagemask.drawing = true;
               if (zf != null) {
                  zf.smaskMenuEnabler(false);
                  targetMenuEnabler(false);
               }
               this.add("East", mtools);
               setSize(this.getPreferredSize());
               setVisible(true);
               maskFunctionsMenuEnabler(true);
               imagemask.updateImage();
               mainCanvas.updateMask(imagemask);
            }
         }
         return;
      }
      if (actionEvt.getSource() == exportMaskMenuItem) {  //export mask to bitmap
         if (imageObject != null) {
            if (imagemask != null) {
               FileDialog f = new FileDialog(this, "Export mask", FileDialog.SAVE);
               if (maskdir != null)
                  f.setDirectory(maskdir);
               f.setVisible(true);
               f.dispose();
               setCursor(new Cursor(Cursor.WAIT_CURSOR));
               String filename = f.getFile();
               if (filename != null) {
                  filename = f.getDirectory() + filename;         
                  imagemask.exportMask(filename);
                  setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
               }
               else {
                  setCursor(new Cursor(Cursor.DEFAULT_CURSOR));                  
               }
            }
         }
         return;
      }

   // COMMANDS MENU OPTIONS ************
      if (actionEvt.getSource() == outputterMenuItem) {  // XY Plotter output
         if(imageObject != null) {
            if(xy_plotter != null) {
               String xy_directory = xy_plotter.substring(0, xy_plotter.lastIndexOf(System.getProperty("file.separator")) + 1);
               String xy_filename = xy_plotter.substring(xy_plotter.lastIndexOf(System.getProperty("file.separator")) + 1);
               commander(xy_directory, xy_filename);
            }
            else {
               FileDialog f = new FileDialog(this, "Select xy_plotter", FileDialog.LOAD);
               f.setVisible(true);
               String filename = f.getFile();
            // Should get this from somewhere else...
               if (filename != null)  { commander(f.getDirectory(), filename); }
            }
         }
         return;
      }
      if (actionEvt.getSource() == otherMenuItem) {  // Do outputter command
         if(imageObject != null) {
            FileDialog f = new FileDialog(this, "Select command", FileDialog.LOAD);
            f.setVisible(true);
            String filename = f.getFile();
         // Should get this from somewhere else. . . 
            if (filename != null)  { commander(f.getDirectory(), filename); }
         }
         return;
      }

   // TARGETS MENU OPTIONS ************
      if (actionEvt.getSource() == enterTargetsMenuItem) {
         if (imageObject != null && zf != null) {
            if (targetframe != null && targetframe.isVisible()) {return;}
            pointtargets targets = new pointtargets(imageObject.filename.substring(0, imageObject.filename.length() - 6));
            targetframe = new targetFrame(this, targets, mainCanvas,  zf);   //show the target thing.
            targetframe.setVisible(true);
            zf.smaskMenuEnabler(false);
            maskMenuEnabler(false);
            mainCanvas.updateTargets(targets);
            zf.updateTargets(targets);
            targetFunctionsMenuEnabler(true);
         }
         return;
      }
      if(actionEvt.getSource() == exitTargetsMenuItem) {
         exitTargets();				
         return;
      }
      if (actionEvt.getSource() == importTargetsMenuItem) {
         if (targetframe != null)
            { targetframe.importTargets(); }
         return;
      }
      if (actionEvt.getSource() == exportTargetsMenuItem) {
         if (targetframe != null) 
            { targetframe.mytargets.exportTargets(); }
         return;
      }
         
   // HELP MENU OPTIONS ****************
      if (actionEvt.getSource() == generalHelpMenuItem) {
         if ((help==null) || !help.isVisible()) {
            help = new helpFrame(helpdir, "general.help");
            help.pack();
            help.setVisible(true);
         }
         else  { help.setFile(helpdir, "general.help"); }
         return;
      }
      if (actionEvt.getSource() == maskHelpMenuItem) {
         if ((help==null) || !help.isVisible()) {
            help = new helpFrame(helpdir, "mask.help");            
            help.pack();
            help.setVisible(true);
         }
         else  { help.setFile(helpdir, "mask.help"); }
         return;
      }
      if (actionEvt.getSource() == smaskHelpMenuItem) {
         if ((help==null) || !help.isVisible()) {
            help = new helpFrame(helpdir, "smask.help");            
            help.pack();
            help.setVisible(true);
         }
         else  { help.setFile(helpdir, "smask.help"); }
         return;
      }
      if (actionEvt.getSource() == targetsHelpMenuItem) {
         if ((help==null) || !help.isVisible()) {
            help = new helpFrame(helpdir, "targets.help");            
            help.pack();
            help.setVisible(true);
         }
         else  { help.setFile(helpdir, "targets.help"); }
         return;
      }
      if (actionEvt.getSource() == commandsHelpMenuItem) {
         if ((help==null) || !help.isVisible()) {
            help = new helpFrame(helpdir, "commands.help");            
            help.pack();
            help.setVisible(true);
         }
         else  { help.setFile(helpdir, "commands.help"); }
         return;
      }
      if (actionEvt.getSource() == aboutHelpMenuItem) {
         aboutDialog about = new aboutDialog(this);
         about.pack();
         about.setLocation((getToolkit().getScreenSize().width  - about.getWidth())/2,
                           (getToolkit().getScreenSize().height - about.getHeight())/2);
         about.setVisible(true);
         return;
      }

   // MASKING PENSIZE BUTTONS **********
      if (actionEvt.getSource() == penSizeButton1) {
         pensize = 1;
         if (imagemask != null)  { imagemask.drawing = true; }
      }
      if (actionEvt.getSource() == penSizeButton3) {
         pensize = 3;
         if (imagemask != null)  { imagemask.drawing = true; }
      }
      if (actionEvt.getSource() == penSizeButton7) {
         pensize = 7;
         if (imagemask != null)  { imagemask.drawing = true; }
      }
      if (actionEvt.getSource() == penSizeButton10) {
         pensize = 10;
         if (imagemask != null)  { imagemask.drawing = true; }
      }
      if (actionEvt.getSource() == penSizeButton20) {
         pensize = 20;
         if (imagemask != null)  { imagemask.drawing = true; }
      }
   }

   // ITEM LISTENER ROUTINE ****************************************************
   public void itemStateChanged(ItemEvent itemEvt) {
      if (itemEvt.getSource() == brightMenuItem) {
         zf.zoomedCanvas.bright = !zf.zoomedCanvas.bright; 
         zf.zoomedCanvas.repaint();
      }
      if (itemEvt.getSource() == unmaskMenuItem)
         { unmask = !unmask; }
   }

   // What to do if the mouse is pressed or dragged ****************************
   public void mousePressedOrDragged(MouseEvent mouseEvt) {
   // If we're masking
      if (imagemask != null && imagemask.drawing) {
         imagemask.updateImage(mainCanvas.x-pensize, mainCanvas.y-pensize,
                               mainCanvas.x+pensize, mainCanvas.y+pensize,
                               unmask);
         mainCanvas.updateMask(imagemask, pensize, unmask);
      // Decide if the mask pen was inside the zoom window
      //   - makes redrawing scads quicker.
         if (zf != null) {
            int tx = mainCanvas.x;
            int ty = mainCanvas.y;
            int offx; int offy;
            offx = (int)(zf.getWidth()/zf.zoomedCanvas.ratio * imagesize)
                       / imageObject.width;
            offy = (int)(zf.getHeight()/zf.zoomedCanvas.ratio * imagesize)
                       / imageObject.height;
            int zx1 = zf.getWidth()  - offx/2;      
            int zy1 = zf.getHeight() - offy/2;   
            int zx2 = zf.getWidth()  + offx/2;                                   
            int zy2 = zf.getHeight() + offy/2;

            // Heck, this will work for now. I mean have you ever seen an uglier if statement?!
            if ((((((zx1 < tx-pensize) && (tx-pensize < zx2)) || ((zx1 < tx+pensize) && (tx+pensize < zx2))) &&
                  (((zy1 < ty-pensize) && (ty-pensize < zy2)) || ((zy1 < ty+pensize) && (ty+pensize < zy2)))) ||
                 ((((tx-pensize < zx1) && (zx1 < tx+pensize)) || ((tx-pensize < zx2) && (zx2 < tx+pensize))) &&
                  (((ty-pensize < zy1) && (zy1 < ty+pensize)) || ((ty-pensize < zy2) && (zy2 < ty+pensize))))) &&
                    (zoommaskcount%6 == 0)) {
               zf.updateMask(imagemask);
               zoommaskcount = 0;
            }
            zoommaskcount++;
         }
         return;
      }
   // If nothing else is happening...
      if (zf instanceof zoomFrame) {
         zf.zoomImage(mainCanvas.x, mainCanvas.y, imageObject);
         return;
      }
   }

   // MOUSE LISTENER ROUTINES **************************************************
   public void mouseClicked(MouseEvent mouseEvt) { }
   public void mouseEntered(MouseEvent mouseEvt) { }
   public void mouseExited(MouseEvent mouseEvt) { }
   public void mousePressed(MouseEvent mouseEvt) { 
      mousePressedOrDragged(mouseEvt);
   }
   public void mouseReleased(MouseEvent mouseEvt) {
      if(zf instanceof zoomFrame) {
         zf.updateMask(imagemask);
      }
   }

   // MOUSE MOTION LISTENER ROUTINES *******************************************
   public void mouseMoved(MouseEvent mouseEvt) { }
   public void mouseDragged(MouseEvent mouseEvt) {
      mousePressedOrDragged(mouseEvt);
   }


   // COMPONENT LISTENER ROUTINES **********************************************
   public void componentHidden(ComponentEvent compEvt) { }
   public void componentMoved(ComponentEvent compEvt) {
      if (compEvt.getComponent() == this) {
         locatex = this.getX();
         locatey = this.getY();
      }
   }
   public void componentResized(ComponentEvent compEvt) { }
   public void componentShown(ComponentEvent compEvt) { }


   // WINDOW LISTENER ROUTINES *************************************************
   public void windowClosing(WindowEvent windowEvt) { 
      this.dispose();
      System.exit(0);
   } 
   public void windowActivated(WindowEvent windowEvt) { }
   public void windowDeactivated(WindowEvent windowEvt) { }
   public void windowDeiconified(WindowEvent windowEvt) { }
   public void windowClosed(WindowEvent windowEvt) { }
   public void windowIconified(WindowEvent windowEvt) { }
   public void windowOpened(WindowEvent windowEvt) { }


   // MAIN *********************************************************************
   public static void main(String args[]) {
      pvs tf = new pvs();
   }
}

