
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//ABOUT DIALOG

class aboutDialog extends Dialog {
	
	aboutDialog (Frame parent) {
		super (parent, "pvs zero.ninetwo");
		this.setBackground(Color.lightGray);

		Panel aboutpan = new Panel();
		aboutpan.setLayout(new GridLayout(6, 1));
		aboutpan.setBackground(Color.lightGray);
		aboutpan.add(new Label("alaska sar facility"));
		aboutpan.add(new Label("process validation system"));
		aboutpan.add(new Label("copyright 2000"));
		aboutpan.add(new Label(" "));
		aboutpan.add(new Label("interface and noise: dann toliver"));
		aboutpan.add(new Label("backend and toys: jay cable"));
		add("Center", aboutpan);

		Panel south = new Panel();
		south.add(new Button ("okay"));
		add ("South", south);
		
		setResizable(false);
		pack();
	}
	
	public boolean action (Event e, Object o) {
		if ("okay".equals(o)) {
			dispose();
			return true;
		}
		return super.handleEvent(e);
	}

	public boolean handleEvent (Event evt) {
		if (evt.id == Event.WINDOW_DESTROY) {
			dispose();
			return true;
		}
		return super.handleEvent(evt);
	}
}

