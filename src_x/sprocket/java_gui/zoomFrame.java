/* ****************************************************************************
NAME: zoomFrame.java

DESCRIPTION:
	SProCKET Zoom window.

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


//ZOOM FRAME

class zoomFrame extends Frame implements ActionListener, ItemListener,
                                                          WindowListener {
  mask imagemask;
  pvs mainFrame;
  imageCanvas mainCanvas;
  zoomCanvas zoomedCanvas; 
  pointtargets targets;
  Label coordinates;
  
  Menu optionMenu;
  Menu ratioMenu;
  Menu smaskMenu;
  
  MenuItem r12MenuItem;
  MenuItem r25MenuItem;
  MenuItem r50MenuItem;
  MenuItem r100MenuItem;
  MenuItem r200MenuItem;
  MenuItem r400MenuItem;
  MenuItem r800MenuItem;
  MenuItem r1600MenuItem;
  
  MenuItem enterMenuItem;
  MenuItem beginMenuItem;
  MenuItem exitMenuItem;
  MenuItem importMenuItem;
  
  MenuItem exportImageMenuItem;
  CheckboxMenuItem autoMenuItem;
  CheckboxMenuItem flyMenuItem;
  CheckboxMenuItem bestMenuItem;
  MenuItem closeMenuItem;

  zoomFrame(pvs mainFrame) {
    super("Zoomer");
    this.mainCanvas = mainFrame.mainCanvas;
    this.mainFrame = mainFrame;
    int locatex = mainFrame.locatex;
    int locatey = mainFrame.locatey;
    MenuBar mb = new MenuBar();

    addWindowListener(this);

    //OPTION MENU
    optionMenu = new Menu("Option");
    optionMenu.add(exportImageMenuItem = new MenuItem("Export image..."));
    optionMenu.add(autoMenuItem = new CheckboxMenuItem("Auto export"));
    optionMenu.add(flyMenuItem = new CheckboxMenuItem("Fly through"));
    optionMenu.add(bestMenuItem = new CheckboxMenuItem("Best quality"));
    optionMenu.add(closeMenuItem = new MenuItem("Close window"));
    exportImageMenuItem.addActionListener(this);
    autoMenuItem.addItemListener(this);
    flyMenuItem.addItemListener(this);
    bestMenuItem.addItemListener(this);
    closeMenuItem.addActionListener(this);

    // ZOOM RATIO MENU
    ratioMenu = new Menu("zoom ratio");
    ratioMenu.add(r12MenuItem   = new MenuItem("12.5%"));
    ratioMenu.add(r25MenuItem   = new MenuItem("25%"));
    ratioMenu.add(r50MenuItem   = new MenuItem("50%"));
    ratioMenu.add(r100MenuItem  = new MenuItem("100%"));
    ratioMenu.add(r200MenuItem  = new MenuItem("200%"));
    ratioMenu.add(r400MenuItem  = new MenuItem("400%"));
    ratioMenu.add(r800MenuItem  = new MenuItem("800%"));
    ratioMenu.add(r1600MenuItem = new MenuItem("1600%"));
    r12MenuItem.addActionListener(this);
    r25MenuItem.addActionListener(this);
    r50MenuItem.addActionListener(this);
    r100MenuItem.addActionListener(this);
    r200MenuItem.addActionListener(this);
    r400MenuItem.addActionListener(this);
    r800MenuItem.addActionListener(this);
    r1600MenuItem.addActionListener(this);

    // SMASK MENU
    smaskMenu = new Menu("smask");
    enterMenuItem = smaskMenu.add(new MenuItem("enter smask mode", new MenuShortcut(KeyEvent.VK_S)));
    beginMenuItem = smaskMenu.add(new MenuItem("begin smasking"));
    exitMenuItem = smaskMenu.add(new MenuItem("exit smask mode"));
    smaskMenu.addSeparator();
    importMenuItem = smaskMenu.add(new MenuItem("import smask"));
    enterMenuItem.addActionListener(this);
    beginMenuItem.addActionListener(this);
    exitMenuItem.addActionListener(this);
    importMenuItem.addActionListener(this);
    
    // Construct zoom window's top bar of menus
    mb.add(optionMenu);
    mb.add(ratioMenu);
    mb.add(smaskMenu);
    setMenuBar(mb);

    // Draw zoomed terrain
    zoomedCanvas = new zoomCanvas(this);
    add("Center", zoomedCanvas);
    
    // Tell user about selected coordinates below the canvas
    coordinates = new Label("Zoom window information bar");
    coordinates.setBackground(Color.lightGray);
    add("South", coordinates);

    smaskMenuEnabler(true);
    smaskFunctionsMenuEnabler(false);

    setSize(260, 260);
    if(locatex > 0 && locatey > 0)  {
      if(locatex + mainFrame.getWidth() + getWidth() < getToolkit().getScreenSize().width)
        setLocation(locatex + mainFrame.getWidth(), locatey + 60);
      else
        setLocation(locatex + 3, locatey + 41);
    }
    
    setVisible(true);
  }

  public void ratioMenuEnabler(boolean b) {
    r12MenuItem.setEnabled(b);
    r25MenuItem.setEnabled(b);
    r50MenuItem.setEnabled(b);
    r100MenuItem.setEnabled(b);
    r200MenuItem.setEnabled(b);
    r400MenuItem.setEnabled(b);
    r800MenuItem.setEnabled(b);
    r1600MenuItem.setEnabled(b);
  }

  public void smaskMenuEnabler(boolean b) {
    enterMenuItem.setEnabled(b);
    importMenuItem.setEnabled(b);

    if(!b) {
      smaskFunctionsMenuEnabler(b);
    }
    if(b && (zoomedCanvas.simagemask != null)) {
      smaskFunctionsMenuEnabler(b);
    }
  }

  public void smaskFunctionsMenuEnabler(boolean b) {
    beginMenuItem.setEnabled(b);
    exitMenuItem.setEnabled(b);
  }

  // have mainCanvas call this to get the coords back.
  public Dimension getPosition() {
    int x = zoomedCanvas.realx * zoomedCanvas.im.imagesize;
    int y = zoomedCanvas.realy * zoomedCanvas.im.imagesize;
    x /= zoomedCanvas.im.width;
    y /= zoomedCanvas.im.height;
    return new Dimension(x, y);
  }

  //have mainCanvas call this to get the size for the box.
  public Dimension getZoomSize() {
    int width  = zoomedCanvas.getSize().width * zoomedCanvas.im.imagesize;
    int height = zoomedCanvas.getSize().height * zoomedCanvas.im.imagesize;
    width  /= zoomedCanvas.im.width;
    height /= zoomedCanvas.im.height;
    width  /= zoomedCanvas.ratio;
    height /= zoomedCanvas.ratio;
    return new Dimension(width, height);
  }

  public void updateMask (mask imagemask) {
    this.imagemask = imagemask;
    zoomedCanvas.updateMask(imagemask);
  }
  
  public void updateMask (mask imagemask, int pensize) {
    this.imagemask = imagemask;
//  zoomedCanvas.updateMask(imagemask, pensize);  //for speed, but using the 'wait to refresh' right now.
  }
  
  public void updateTargets (pointtargets targets) {
    this.targets = targets;
    zoomedCanvas.updateTargets(targets);
  }
  
  public void zoomImage(int x, int y, importImage im) {
    if (this.isVisible()) {
      int realx = x * im.width; //use realx and realy from here on out.
      int realy = y * im.height;    
      realx /= im.imagesize;
      realy /= im.imagesize;
      zoomedCanvas.zoomImage(realx, realy, im, imagemask);
    }
  }

  // What to do if the ActionListener hears something
  public void actionPerformed (ActionEvent actEvent) {
  // OPTION MENU ITEMS *****************
    if (actEvent.getSource() == exportImageMenuItem) {
      FileDialog f = new FileDialog(this, "export image as...", FileDialog.SAVE);
      if (mainFrame.imagedir != null)
        f.setDirectory(mainFrame.imagedir);
      f.setVisible(true);
      f.dispose();
      String filename = f.getFile();
      if(filename != null) {
        if(filename.lastIndexOf(".pgm") == -1)
          { filename += ".pgm"; }
        filename = f.getDirectory() + filename;
        zoomedCanvas.im.exportImage(filename, zoomedCanvas.km);
      }
    }
    if (actEvent.getSource() == closeMenuItem) {
      zoomedCanvas.unsmasking();
      mainCanvas.clearSmask();
      this.setResizable(true);
      mainFrame.maskMenuEnabler(true);
      mainFrame.targetMenuEnabler(true);
      flyMenuItem.setState(false);
      smaskFunctionsMenuEnabler(false);
      zoomedCanvas.fly = false;
      processEvent(new WindowEvent(this,WindowEvent.WINDOW_CLOSING));
    }
  // RATIO MENU ITEMS ******************
    if (actEvent.getSource() == r12MenuItem)   {zoomedCanvas.newRatio(.125);}
    if (actEvent.getSource() == r25MenuItem)   {zoomedCanvas.newRatio(.25);}
    if (actEvent.getSource() == r50MenuItem)   {zoomedCanvas.newRatio(.5);}
    if (actEvent.getSource() == r100MenuItem)  {zoomedCanvas.newRatio(1);}
    if (actEvent.getSource() == r200MenuItem)  {zoomedCanvas.newRatio(2);}
    if (actEvent.getSource() == r400MenuItem)  {zoomedCanvas.newRatio(4);}
    if (actEvent.getSource() == r800MenuItem)  {zoomedCanvas.newRatio(8);}
    if (actEvent.getSource() == r1600MenuItem) {zoomedCanvas.newRatio(16);}
  // SMASK MENU ITEMS ******************
    if(zoomedCanvas.im != null) {
      // Enter smask mode & start drawing
      if (actEvent.getSource() == enterMenuItem ) {
        if(zoomedCanvas.simagemask == null) {
          FileDialog f = new FileDialog(this, "new smask filename", FileDialog.SAVE);
          if(mainFrame.maskdir != null) 
            f.setDirectory(mainFrame.maskdir);
          f.setVisible(true);
          f.dispose();
          String filename = f.getFile();
          if(filename != null) {
            this.setResizable(false);
            flyMenuItem.setEnabled(false);
            smaskFunctionsMenuEnabler(true);
            mainFrame.maskMenuEnabler(false);
            mainFrame.targetMenuEnabler(false);
            filename = f.getDirectory() + filename;
            zoomedCanvas.simagemask = new smask(filename, mainCanvas.im.imagesize, zoomedCanvas.im.width, zoomedCanvas.im.height);
            zoomedCanvas.smasking();
          }
        }
        else { zoomedCanvas.smasking(); }
      }
      // Start drawing
      if (actEvent.getSource() == beginMenuItem) {
        if(zoomedCanvas.im != null) {
          if(zoomedCanvas.simagemask != null)   { zoomedCanvas.smasking(); }
        }
      }
      // Exit mask mode & destroy mask
      if (actEvent.getSource() == exitMenuItem) {
        mainCanvas.clearSmask();
        zoomedCanvas.unsmasking();
        smaskFunctionsMenuEnabler(false);
        mainFrame.maskMenuEnabler(true);
        mainFrame.targetMenuEnabler(true);
      }
      // Import smask from bitmap
      if (actEvent.getSource() == importMenuItem) {
        FileDialog f = new FileDialog(this, "import mask", FileDialog.LOAD);
        if(mainFrame.maskdir != null) 
          f.setDirectory(mainFrame.maskdir);
        f.setVisible(true);
        f.dispose();
        String filename = f.getFile();
        if(filename != null) {
          smaskFunctionsMenuEnabler(true);
          mainFrame.maskMenuEnabler(false);
          mainFrame.targetMenuEnabler(false);
          filename = f.getDirectory() + filename;
          zoomedCanvas.simagemask = new smask(filename, mainCanvas.im.imagesize, zoomedCanvas.im.width, zoomedCanvas.im.height, true);
          mainCanvas.updateSmask(zoomedCanvas.simagemask);
          zoomedCanvas.repaint();
        }
      }
    } // END SMASK MENU ITEMS
  }      

  // What to do if the ItemListener hears something
  public void itemStateChanged (ItemEvent itemEvt) {
    if(itemEvt.getSource() == autoMenuItem) {
      if(!zoomedCanvas.auto) {//Do this first because when the file dialog goes
                          //away,SProCKET will repaint the zoom canvas. If
                          //auto is on, your first filename will be null.
        FileDialog f = new FileDialog(zoomFrame.this, "base export name", FileDialog.SAVE);
        if(zoomFrame.this.mainFrame.imagedir != null)
          f.setDirectory(zoomFrame.this.mainFrame.imagedir);
        f.setVisible(true);
        f.dispose();
        String filename = f.getFile();
        if(filename != null) {
          if(filename.lastIndexOf(".pgm") != -1)
            filename = (filename.substring(0, filename.length() - 4));
          filename = f.getDirectory() + filename;
          zoomedCanvas.autoname = filename;
          zoomedCanvas.autonumber = 0;
        }
      }
      zoomedCanvas.auto = !zoomedCanvas.auto;
    }
    if(itemEvt.getSource() == flyMenuItem) {
      zoomedCanvas.fly = !zoomedCanvas.fly; 
      smaskMenuEnabler(!zoomedCanvas.fly);
    }
    if(itemEvt.getSource() == bestMenuItem) {
      zoomedCanvas.best = !zoomedCanvas.best; 
      zoomedCanvas.repaint();
    }
  }

  // What to do if the windowListener hears something
  public void windowClosing(WindowEvent we) { 
    zoomedCanvas.unsmasking();
    mainCanvas.clearSmask();
    mainFrame.maskMenuEnabler(true);
    mainFrame.targetMenuEnabler(true);
    flyMenuItem.setState(false);
    smaskFunctionsMenuEnabler(false);
    zoomedCanvas.fly = false;
    this.dispose(); 
  } 
  public void windowActivated(WindowEvent we) { }
  public void windowDeactivated(WindowEvent we) { }
  public void windowDeiconified(WindowEvent we) { }
  public void windowClosed(WindowEvent we) { }
  public void windowIconified(WindowEvent we) { }
  public void windowOpened(WindowEvent we) { }
}

