
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//PROCESS METER

class procDialog extends Dialog {
	Frame myFrame;
	Label percent;
	procCanvas zinger;	
	
	procDialog (Frame parent, String title) {
		super (parent, title);
		this.setBackground(Color.lightGray);
		myFrame = parent;

		zinger = new procCanvas();
		zinger.setSize(240, 70);
		percent = new Label("      percent completed: 0");
		
		this.add("Center", zinger);
		this.add("South", percent);
		pack();
		setResizable(false);
	}
	
	public void process(int done) {		//ranges from 0 to 100
		if(done < 0) {done = 0;}
		if(done > 100) {done = 100;}
		zinger.process(done);
		percent.setText("      percent completed: " + done);
	}
}

//PROCESS CANVAS

class procCanvas extends Canvas {
	int xoff = 20;
	int yoff = 15;
	int done = 0;
	
	public void process(int done) {
		this.done = done;
		Graphics g = getGraphics();		
		paint(g);
}

	public void paint(Graphics g) {
		g.setColor(Color.black);
		g.clearRect(0, 0, this.getSize().width, this.getSize().height);
		g.drawRect(xoff, yoff, 200, 50);
		g.fillRect(xoff, yoff, done*2, 50);
		Toolkit.getDefaultToolkit().sync();
	}
}

