/* *****************************************************************************
NAME: prefDialog.java

DESCRIPTION:
	Before opening an image, use this dialog to decipher a users preferences
	on window size and image quality.

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


//IMAGE PREFERENCES

class prefDialog extends Dialog implements ActionListener, WindowListener {
	CheckboxGroup cksize = new CheckboxGroup();
	CheckboxGroup ckqual = new CheckboxGroup();
	boolean cancelled = false;
	Button okayButton;

	prefDialog (Frame parent, int imagesize, int imagequality) {
		super (parent, "image import preferences", true);
		this.setBackground(Color.lightGray);

		boolean b200 = false;
		boolean b400 = false;
		boolean b800 = false;
		
		if(imagesize == 200)  {b200 = true;}
		if(imagesize == 400)  {b400 = true;}
		if(imagesize == 800)  {b800 = true;}
		
		boolean b1 = false;
		boolean b9 = false;
		boolean b0 = false; 
		
		if(imagequality == 1)  {b1 = true;}
		if(imagequality == 9)  {b9 = true;}
		if(imagequality == 0)  {b0 = true;}

		Checkbox two = new Checkbox("200 x 200", b200, cksize);
		Checkbox four = new Checkbox("400 x 400", b400, cksize);
		Checkbox eight = new Checkbox("800 x 800", b800, cksize);

		Checkbox good = new Checkbox("good", b1, ckqual);
		Checkbox better = new Checkbox("better", b9, ckqual);
		Checkbox best = new Checkbox("best", b0, ckqual);
	
		Panel size = new Panel();
		size.setLayout(new BorderLayout());
		size.setBackground(Color.lightGray);
		Panel sizecheck = new Panel();
		sizecheck.setLayout(new GridLayout(1, 3));
		sizecheck.add(two);
		sizecheck.add(four);
		sizecheck.add(eight);
		size.add("North", new Label("inital window size:"));
		size.add("South", sizecheck);
		
		Panel quality = new Panel();
		quality.setLayout(new BorderLayout());
		quality.setBackground(Color.lightGray);
		Panel qualcheck = new Panel();
		qualcheck.setLayout(new GridLayout(1, 3));
		qualcheck.add(good);
		qualcheck.add(better);
		qualcheck.add(best);
		quality.add("North", new Label("import image quality:"));
		quality.add("South", qualcheck);

		Panel north = new Panel();
		north.setLayout(new BorderLayout());
		north.setBackground(Color.lightGray);
		north.add("North", size); 
		north.add("South", quality);
		add ("North", north);
		Panel south = new Panel();
		south.add(okayButton = new Button ("Okay"));
		add ("South", south);
		
		// Listen for input
		okayButton.addActionListener(this);
		addWindowListener(this);

		setResizable(true);
		pack();
	}
		
	public void actionPerformed (ActionEvent actEvent)
	{
		if (actEvent.getSource()==okayButton)
			this.hide();
	}

	public void windowClosing(WindowEvent e) { 
		cancelled = true;
		this.dispose(); 
	} 
	public void windowActivated(WindowEvent e) { }
	public void windowDeactivated(WindowEvent e) { }
	public void windowDeiconified(WindowEvent e) { }
	public void windowClosed(WindowEvent e) { }
	public void windowIconified(WindowEvent e) { }
	public void windowOpened(WindowEvent e) { }  
}

