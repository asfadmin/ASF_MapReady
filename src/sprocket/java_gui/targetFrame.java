/* ****************************************************************************
NAME: targetFrame.java

DESCRIPTION:
	SProCKET sub-window with a list of point targets linked to point target
	marks on the main canvas.

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


//POINT TARGET WINDOW

class targetFrame extends Frame implements ActionListener, ItemListener,
                                                          WindowListener {
  pointtargets mytargets;
  imageCanvas mainCanvas;
  pvs parent;
  zoomFrame zf;
  Vector targets;
  Label targetinfo;
  java.awt.List targetlist;
  Button linkButton;
  Button deleteButton;
  Button restoreAllButton;

  public targetFrame(pvs parent, pointtargets mytargets, imageCanvas mainCanvas, zoomFrame zf) {
    super("Point target list");
    this.setBackground(Color.lightGray);
    this.parent = parent;
    this.mytargets = mytargets;
    this.targets = mytargets.visibletargets;
    this.mainCanvas = mainCanvas;
    this.zf = zf;

    //put all the targets in the selection list
    targetlist = new java.awt.List(8);
    for(int i=0; i < targets.size(); i++) {
      pt now = (pt) targets.elementAt(i);
      targetlist.add(now.devname);
    }
    this.add("Center", targetlist);
    targetlist.select(0);

    //diddle with the buttons and stuff.

    Panel bottom = new Panel();
    bottom.setLayout(new BorderLayout());

    pt now = (pt) targets.elementAt(0);
    targetinfo = new Label ("potential x:" + Integer.toString(now.potentialx) + " " +
                            "potential y:" + Integer.toString(now.potentialy) +
                            "                                                    ");
    bottom.add("Center", targetinfo);

    Panel south = new Panel();
    south.setLayout(new GridLayout(1, 3));
    south.add(linkButton = new Button("Link"));
    south.add(deleteButton = new Button("Delete"));
    south.add(restoreAllButton = new Button("Restore all"));
    bottom.add("South", south);

    this.add("South", bottom);                

    int locatex = getToolkit().getScreenSize().width  - getSize().width;
    int locatey = getToolkit().getScreenSize().height - getSize().height;
    if (locatex > 0 && locatey > 0)
       setLocation(locatex/2, locatey/2);

    // Listen for input
    linkButton.addActionListener(this);
    deleteButton.addActionListener(this);
    restoreAllButton.addActionListener(this);
    targetlist.addItemListener(this);
    addWindowListener(this);

    pack();
  }        

  public void importTargets() {
    mytargets.importTargets();

    targetlist.removeAll();
    targets = mytargets.visibletargets;
    for(int i=0; i < targets.size(); i++) {
      pt now = (pt) targets.elementAt(i);
      targetlist.add(now.devname);
    }

    zf.zoomedCanvas.repaint();
    mainCanvas.repaint();
  }

  public void actionPerformed (ActionEvent actEvent)
  {
    if ((actEvent.getSource()==linkButton) &&
        (targetlist.getSelectedItem()!=null)) {
      mytargets.actualize(targetlist.getSelectedItem(),
                          zf.zoomedCanvas.targetx, zf.zoomedCanvas.targety);
      zf.zoomedCanvas.repaint();
      mainCanvas.repaint();
      for(int i=0; i < targets.size(); i++) {
        pt now = (pt) targets.elementAt(i);
        if(now.devname == targetlist.getSelectedItem()) {
          targetinfo.setText("potential x:" + Integer.toString(now.potentialx) + " " +
                             "potential y:" + Integer.toString(now.potentialy) + " " +
                             "actual x:" + Integer.toString(now.actualx) + " " +
                             "actual y:" + Integer.toString(now.actualy) );
        }
      }
    }
    if ((actEvent.getSource()==deleteButton) &&
        (targetlist.getSelectedItem() != null)) {
      mytargets.remove(targetlist.getSelectedItem());
      targetlist.remove(targetlist.getSelectedIndex());
      zf.zoomedCanvas.repaint();
      mainCanvas.repaint();
    }
    if (actEvent.getSource()==restoreAllButton) {
      mytargets.resetvisible();
      targetlist.removeAll();
      targets = mytargets.visibletargets;
      for(int i=0; i < targets.size(); i++) {
        pt now = (pt) targets.elementAt(i);
        targetlist.add(now.devname);
      }
      zf.zoomedCanvas.repaint();
      mainCanvas.repaint();
    }
  }

  public void itemStateChanged (ItemEvent itemEvt) {
    if (itemEvt.getStateChange() == ItemEvent.SELECTED) {
      for(int i=0; i < targets.size(); i++) {
        pt now = (pt) targets.elementAt(i);
        if(now.devname == targetlist.getSelectedItem()) {
          if(now.actualized) {
            targetinfo.setText("potential x:" + Integer.toString(now.potentialx) + " " +
                               "potential y:" + Integer.toString(now.potentialy) + " " +
                               "actual x:" + Integer.toString(now.actualx) + " " +
                               "actual y:" + Integer.toString(now.actualy) );
          }
          else {
            targetinfo.setText("potential x:" + Integer.toString(now.potentialx) + " " +
                               "potential y:" + Integer.toString(now.potentialy) );
          }
        }
      }                
    }
  }

  public void windowClosing(WindowEvent we) { 
    parent.exitTargets();
    this.dispose(); 
  } 
  public void windowActivated(WindowEvent we) { }
  public void windowDeactivated(WindowEvent we) { }
  public void windowDeiconified(WindowEvent we) { }
  public void windowClosed(WindowEvent we) { }
  public void windowIconified(WindowEvent we) { }
  public void windowOpened(WindowEvent we) { }
}

