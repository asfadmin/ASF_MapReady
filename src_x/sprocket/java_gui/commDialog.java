/* ****************************************************************************
NAME: commDialog.java

DESCRIPTION:
	Sub-gui for SProCKET to call one of its accompanying programs,
        xy_plotter.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
            07/03  P. Denny     Split bloated pvs.java up into
                                 files named after their classes
            07/03  P. Denny     Replaced depricated action and handleEvent
                                 methods with appropriate Listeners
            09/03  P. Denny     Update the gui to use the new improved java
                                 J-Components. And use gridBagLayout for a nicer
                                 looking gui. As soon as we upgrade to a newer
                                 java, we should change over to GridLayout2 for
                                 better readability

**************************************************************************** */

import java.io.File;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

class commDialog extends JDialog implements ActionListener, KeyListener,
                                            WindowListener {
   public boolean cancelled=true;
   public String outputfile;
   public String imagefile;
   public String maskfile;
   public String maskThing;
   pvs mainFrame;
   JTextField imageTextField;
   JTextField maskTextField;
   JTextField outputTextField;
   Choice maskChoice;
   JButton browseOutputButton;
   JButton browseImageButton;
   JButton browseMaskButton;
   JButton okayButton;
   JButton cancelButton;

   commDialog (pvs parent, String maskfile, String imagefile, String outputfile) {
      super (parent, "Command console", true);
      this.setBackground(Color.lightGray);

      mainFrame = parent;
      this.imagefile = imagefile;
      this.maskfile = maskfile;
      this.outputfile = outputfile;

   // Build textFields, buttons, and choices for the 1st panel
      imageTextField = new JTextField(imagefile, 40);
      maskTextField = new JTextField(maskfile, 40);
      outputTextField = new JTextField(outputfile, 40);
      browseImageButton = new JButton("Browse image");
      browseMaskButton = new JButton("Browse mask");
      browseOutputButton = new JButton("Browse output");
      maskChoice = new Choice();
      maskChoice.add("on");
      maskChoice.add("off");

   // Since we're using java 1.3 instead of 1.4, we can't use GridLayout2
   // This panel collects all the info needed to run xy_plotter
      GridBagLayout gbL = new GridBagLayout();
      GridBagConstraints gbC = new GridBagConstraints();
      gbC.fill = GridBagConstraints.BOTH;
      gbC.weightx = 1.0;
      gbC.weighty = 1.0;
      JPanel topPanel = new JPanel(gbL);
      topPanel.setBackground(Color.lightGray);
      insertRow(topPanel,"Image:", imageTextField, browseImageButton, gbL,gbC);
      insertRow(topPanel,"Mask:",  maskTextField,  browseMaskButton,  gbL,gbC);
      insertRow(topPanel,"Output:",outputTextField,browseOutputButton,gbL,gbC);
      insertRow(topPanel,"Masked area:", maskChoice, new JLabel(""),  gbL,gbC);
      getContentPane().add ("Center", topPanel);

   // Make the panel with okay & cancel buttons
      JPanel bottomPanel = new JPanel();
      bottomPanel.setLayout(new FlowLayout(FlowLayout.CENTER));
      bottomPanel.setBackground(Color.lightGray);
      bottomPanel.add(okayButton = new JButton ("Okay"));
      bottomPanel.add(cancelButton = new JButton ("Cancel"));
      getContentPane().add ("South", bottomPanel);

   // Add listeners
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

// *****************************************************************************
// Insert a row with 3 columns to a panel using the gridBagLayout format
//   the first column is a label & the other 2 are misc components
   private void insertRow(JPanel panel,
                          String col_1, Component col_2, Component col_3,
                          GridBagLayout gbl, GridBagConstraints gbc) {
      JLabel label = new JLabel(col_1);
      gbc.gridwidth = GridBagConstraints.WEST; //Start row on left
      gbl.setConstraints(label, gbc);
      panel.add(label);
      gbl.setConstraints(col_2, gbc);
      panel.add(col_2);
      gbc.gridwidth = GridBagConstraints.REMAINDER; //end row
      gbl.setConstraints(col_3, gbc);
      panel.add(col_3);
      gbc.gridwidth = GridBagConstraints.WEST; //Start next row on left
   }

// Warn user that a file does not exist ****************************************
   public boolean fileTest(String fileName) {
      boolean fileReadable = (new File(fileName)).canRead();

      if (!fileReadable) {
         String message = 
           "File \""+fileName+"\" cannot be read\n"+
           "Please check to make sure that the path and file name are correct\n"+
           "and that you have the correct permissions to read the file.\n";
         JOptionPane.showMessageDialog(this, message, "File unreadable",
                                       JOptionPane.WARNING_MESSAGE);
      }
      return fileReadable;
   }

// What to do if enter or the 'okay' button is pressed *************************
   public void goAhead() {
      try {
         imagefile = imageTextField.getText();
         if (!fileTest(imagefile)) return;
         maskfile = maskTextField.getText();
         if (!fileTest(maskfile)) return;
         maskThing = maskChoice.getSelectedItem();
         outputfile = outputTextField.getText();
         cancelled = false;
         processEvent(new WindowEvent(this,WindowEvent.WINDOW_CLOSING));
      }
      catch (Exception ex) {
         System.out.print("Egad, man!");
      }
   }

// Key Listener stuff **********************************************************
   public void keyPressed (KeyEvent keyEvt) {
      if (keyEvt.getKeyCode() == KeyEvent.VK_ENTER) {
         this.goAhead();
      }
   }
   public void keyReleased (KeyEvent ke) { }
   public void keyTyped (KeyEvent ke) { }


// Action Listener stuff *******************************************************
   public void actionPerformed (ActionEvent actEvent) {
      if (actEvent.getSource() == okayButton) {
         this.goAhead();
      }
      if (actEvent.getSource() == cancelButton) {
         cancelled = true;
         processEvent(new WindowEvent(this,WindowEvent.WINDOW_CLOSING));
      }
      if (actEvent.getSource() == browseImageButton) {
         String imagedir = null;
         FileDialog f = new FileDialog(mainFrame, "select image file", FileDialog.LOAD);
         if (imagefile != null)
            imagedir = imagefile.substring(0, imagefile.lastIndexOf(System.getProperty("file.separator")) + 1);
         if (imagedir != null)
            f.setDirectory(imagedir);
         f.show();
         String filename = f.getFile();
         if (filename != null) {
            String directory = f.getDirectory();
            imagefile = directory + filename;
            imageTextField.setText(imagefile);
         }
      }
      if (actEvent.getSource() == browseMaskButton) {
         String maskdir = null;
         FileDialog f = new FileDialog(mainFrame, "select image file", FileDialog.LOAD);
         if (maskfile != null)
            maskdir = maskfile.substring(0, maskfile.lastIndexOf(System.getProperty("file.separator")) + 1);
         if (maskdir != null)
            f.setDirectory(maskdir);
         f.show();
         String filename = f.getFile();
         if (filename != null) {
            String directory = f.getDirectory();
            maskfile = directory + filename;
            maskTextField.setText(maskfile);
         }
      }
      if (actEvent.getSource() == browseOutputButton) {
         String outputdir = null;
         FileDialog f = new FileDialog(mainFrame, "select image file", FileDialog.SAVE);
         if (outputfile != null)
            outputdir = outputfile.substring(0, outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);
         if (outputdir != null)
            f.setDirectory(outputdir);
         f.show();
         String filename = f.getFile();
         if (filename != null) {
            String directory = f.getDirectory();
            outputfile = directory + filename;
            outputTextField.setText(outputfile);
         }
      }
   } 

// Window Listener stuff *******************************************************
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

