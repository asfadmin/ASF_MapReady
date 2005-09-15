/* *****************************************************************************
NAME: conDialog.java

DESCRIPTION:
   Window in which to set the contrast levels of the image.

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


//CONTRAST

class contDialog extends Dialog implements ActionListener, AdjustmentListener,
                                           KeyListener, WindowListener {
	pvs mainFrame;
	public int low;
	public int high;
	private TextField lowtext;
	private TextField hightext;
	private int defaultlow;
	private int defaulthigh;
	Scrollbar lowscroll;
	Scrollbar highscroll;
	Button closeButton;
	Button defaultButton;
	
	contDialog (pvs parent, int low, int high, int defaultlow, int defaulthigh) {
		super (parent, "Image contrast");
		this.setBackground(Color.lightGray);
		mainFrame = parent;

		this.low = low;
		this.high = high;
		this.defaultlow = defaultlow;
		this.defaulthigh = defaulthigh;
		
		Panel lowpan = new Panel();
		lowpan.setLayout(new BorderLayout());
		lowpan.setBackground(Color.lightGray);
		lowtext = new TextField(Integer.toString(low), 4);		
		lowpan.add("North", new Label("low:"));
		lowpan.add("East", lowtext);
		lowscroll = (Scrollbar) lowpan.add("Center", new Scrollbar(Scrollbar.HORIZONTAL, low, 0, 0, 255));
		
		Panel highpan = new Panel();
		highpan.setLayout(new BorderLayout());
		highpan.setBackground(Color.lightGray);
		hightext = new TextField(Integer.toString(high), 4);		
		highpan.add("North", new Label("high:"));
		highpan.add("East", hightext);
		highscroll = (Scrollbar) highpan.add("Center", new Scrollbar(Scrollbar.HORIZONTAL, high, 0, 0, 255));

		Panel north = new Panel();
		north.setLayout(new BorderLayout());
		north.setBackground(Color.lightGray);
		north.add("North", lowpan);
		north.add("South", highpan);
		add ("North", north);
		Panel south = new Panel();
		south.add(closeButton = new Button ("close"));
		south.add(defaultButton = new Button ("default"));
		add ("South", south);
		setSize(400, 180);
		
		// Listen for input
		lowtext.addKeyListener(this);
		hightext.addKeyListener(this);
		lowscroll.addAdjustmentListener(this);
		highscroll.addAdjustmentListener(this);
		closeButton.addActionListener(this);
		defaultButton.addActionListener(this);
		addWindowListener(this);
	}
		
	public void actionPerformed (ActionEvent actEvent)
	{
	  if (actEvent.getSource()==closeButton) {
	    try {
	      low = Integer.parseInt(lowtext.getText());
	      high = Integer.parseInt(hightext.getText());
	    }
	    catch (Exception ex) {
	      System.out.print("naughty child");
	    }
	    finally {
	      processEvent(new WindowEvent(this,WindowEvent.WINDOW_CLOSING));
	    }
	  }
	  if (actEvent.getSource()==defaultButton) {
	    low = defaultlow;
	    high = defaulthigh;
	    lowscroll.setValue(low);
	    highscroll.setValue(high);
	    mainFrame.newcontrast(low, high);
	    lowtext.setText(Integer.toString(low));
	    hightext.setText(Integer.toString(high));
	  }
	}
	
	public void keyPressed (KeyEvent keyEvt) {
	  if (keyEvt.getKeyCode() == KeyEvent.VK_ENTER) {
	    try {
	      low = Integer.parseInt(lowtext.getText());
	      high = Integer.parseInt(hightext.getText());
	    }
	    catch (Exception ex) {
	      System.out.print("naughty child");
	    }
	    lowscroll.setValue(low);
	    highscroll.setValue(high);
	    mainFrame.newcontrast(low, high);
	  }
	}
	public void keyReleased (KeyEvent ke) { }
	public void keyTyped (KeyEvent ke) { }
	
	public void adjustmentValueChanged(AdjustmentEvent adjustEvt) {
	  if(adjustEvt.getSource() == lowscroll) {
	    low = lowscroll.getValue();
	    lowtext.setText(Integer.toString(low));
	    mainFrame.newcontrast(low, high);
	  }
	  if(adjustEvt.getSource() == highscroll) {
	    high = highscroll.getValue();
	    hightext.setText(Integer.toString(high));
	    mainFrame.newcontrast(low, high);
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

