/* ****************************************************************************
NAME: commDialog.java

DESCRIPTION:
	Sub-gui for SProCKET to call commands.

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


//COMMAND OPTIONS

class commDialog extends Dialog implements ActionListener, KeyListener,
                                                         WindowListener {
  public String outputfile;
  public String imagefile;
  public String maskfile;
  public String xthing;
  public String ything;
  public String mthing;
  public int bin;
  boolean cancelled;
  pvs mainFrame;
  TextField outputTextField;
  TextField imageTextField;
  TextField maskTextField;
  TextField bintextfield;
  Choice xchoice;
  Choice ychoice;
  Choice mchoice;
  Button browseOutputButton;
  Button browseImageButton;
  Button browseMaskButton;
  Button okayButton;
  Button cancelButton;

  commDialog (pvs parent, String maskfile, String imagefile, String outputfile) {
    super (parent, "Command console", true);
    this.setBackground(Color.lightGray);

    mainFrame = parent;
    this.imagefile = imagefile;
    this.maskfile = maskfile;
    this.outputfile = outputfile;
    
    outputTextField = new TextField(outputfile, 40);
    browseOutputButton = new Button("Browse output");
    imageTextField = new TextField(imagefile, 40);
    browseImageButton = new Button("Browse image");
    maskTextField = new TextField(maskfile, 40);
    browseMaskButton = new Button("Browse mask");
    bintextfield = new TextField("500", 5);

    xchoice = new Choice();
    xchoice.add("Incidence");
    xchoice.add("Look");
    xchoice.add("Slant");
    xchoice.add("Ground");
    xchoice.add("Pixel");
    ychoice = new Choice();
    ychoice.add("Digital Number");
    ychoice.add("Power");
    ychoice.add("Sigma0");
    ychoice.add("Gamma0");
    mchoice = new Choice();
    mchoice.add("On");
    mchoice.add("Off");
    
    Panel firstpan = new Panel();
    firstpan.setLayout(new GridLayout(3,1));
    firstpan.setBackground(Color.lightGray);
    firstpan.add(new Label("Image:"));
    firstpan.add(new Label("Mask:"));
    firstpan.add(new Label("Output:"));
    
    Panel secondpan = new Panel();
    secondpan.setLayout(new GridLayout(3,1));
    secondpan.setBackground(Color.lightGray);
    secondpan.add(imageTextField);
    secondpan.add(maskTextField);
    secondpan.add(outputTextField);
    
    Panel thirdpan = new Panel();
    thirdpan.setLayout(new GridLayout(3,1));
    thirdpan.setBackground(Color.lightGray);
    thirdpan.add(browseImageButton);
    thirdpan.add(browseMaskButton);
    thirdpan.add(browseOutputButton);

    Panel collect = new Panel();
    collect.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
    collect.setBackground(Color.lightGray);
    collect.add(firstpan);
    collect.add(secondpan);
    collect.add(thirdpan);

    Panel fourthpan = new Panel();
    fourthpan.setLayout(new GridLayout(2, 4));
    fourthpan.setBackground(Color.lightGray);
    fourthpan.add(new Label("X"));
    fourthpan.add(new Label("Y"));
    fourthpan.add(new Label("Bin"));
    fourthpan.add(new Label("Masked area"));
    fourthpan.add(xchoice);
    fourthpan.add(ychoice);
    fourthpan.add(bintextfield);
    fourthpan.add(mchoice);

    Panel north = new Panel();
    north.setLayout(new BorderLayout());
    north.setBackground(Color.lightGray);
    north.add("North", collect);
    north.add("South", fourthpan);
    add ("North", north);

    Panel south = new Panel();
    collect.setLayout(new FlowLayout(FlowLayout.CENTER));
    south.setBackground(Color.lightGray);
    south.add(okayButton = new Button ("Okay"));
    south.add(cancelButton = new Button ("Cancel"));
    add ("South", south);

    //Add listeners
    imageTextField.addKeyListener(this);
    maskTextField.addKeyListener(this);
    outputTextField.addKeyListener(this);
    browseImageButton.addActionListener(this);
    browseMaskButton.addActionListener(this);
    browseOutputButton.addActionListener(this);
    okayButton.addActionListener(this);
    cancelButton.addActionListener(this);
    addWindowListener(this);

    pack();
  }
    
  public void keyPressed (KeyEvent keyEvt) {
    if (keyEvt.getKeyCode() == KeyEvent.VK_ENTER) {
      try {
        xthing = xchoice.getSelectedItem();
        ything = ychoice.getSelectedItem();
        mthing = mchoice.getSelectedItem();
        bin = Integer.parseInt(bintextfield.getText());
        outputfile = outputTextField.getText();
        imagefile = imageTextField.getText();
        maskfile = maskTextField.getText();
      }
      catch (Exception ex) {
        System.out.print("Egad, man!");
      }
      finally {
        processEvent(new WindowEvent(this,WindowEvent.WINDOW_CLOSING));
      }
    }
  }
  public void keyReleased (KeyEvent ke) { }
  public void keyTyped (KeyEvent ke) { }


  public void actionPerformed (ActionEvent actEvent) {
    if (actEvent.getSource() == okayButton) {
      try {
        xthing = xchoice.getSelectedItem();
        ything = ychoice.getSelectedItem();
        mthing = mchoice.getSelectedItem();
        bin = Integer.parseInt(bintextfield.getText());
        outputfile = outputTextField.getText();
        imagefile = imageTextField.getText();
        maskfile = maskTextField.getText();
      }
      catch (Exception ex) {
        System.out.print("Egad, man!");
      }
      finally {
        processEvent(new WindowEvent(this,WindowEvent.WINDOW_CLOSING));
      }
    }
    if (actEvent.getSource() == cancelButton) {
      cancelled = true;
      processEvent(new WindowEvent(this,WindowEvent.WINDOW_CLOSING));
    }
    if (actEvent.getSource() == browseImageButton) {
      String imagedir = null;
      FileDialog f = new FileDialog(mainFrame, "select image file", FileDialog.LOAD);
      if(imagefile != null)
        imagedir = imagefile.substring(0, imagefile.lastIndexOf(System.getProperty("file.separator")) + 1);
      if(imagedir != null)
        f.setDirectory(imagedir);
      f.show();
      String filename = f.getFile();
      if(filename != null) {
        String directory = f.getDirectory();
        imagefile = directory + filename;
        imageTextField.setText(imagefile);
      }
    }
    if (actEvent.getSource() == browseMaskButton) {
      String maskdir = null;
      FileDialog f = new FileDialog(mainFrame, "select image file", FileDialog.LOAD);
      if(maskfile != null)
        maskdir = maskfile.substring(0, maskfile.lastIndexOf(System.getProperty("file.separator")) + 1);
      if(maskdir != null)
        f.setDirectory(maskdir);
      f.show();
      String filename = f.getFile();
      if(filename != null) {
        String directory = f.getDirectory();
        maskfile = directory + filename;
        maskTextField.setText(maskfile);
      }
    }
    if (actEvent.getSource() == browseOutputButton) {
      String outputdir = null;
      FileDialog f = new FileDialog(mainFrame, "select image file", FileDialog.SAVE);
      if(outputfile != null)
        outputdir = outputfile.substring(0, outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);
      if(outputdir != null)
        f.setDirectory(outputdir);
      f.show();
      String filename = f.getFile();
      if(filename != null) {
        String directory = f.getDirectory();
        outputfile = directory + filename;
        outputTextField.setText(outputfile);
      }
    }
  } 

  public void windowClosing(WindowEvent we) { 
    cancelled = true;
    this.dispose(); 
  } 
  public void windowActivated(WindowEvent we) { }
  public void windowDeactivated(WindowEvent we) { }
  public void windowDeiconified(WindowEvent we) { }
  public void windowClosed(WindowEvent we) { }
  public void windowIconified(WindowEvent we) { }
  public void windowOpened(WindowEvent we) { }
  
}

