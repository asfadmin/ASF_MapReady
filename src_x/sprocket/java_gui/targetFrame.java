
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//POINT TARGET WINDOW

class targetFrame extends Frame {
	pointtargets mytargets;
	imageCanvas mainCanvas;
	pvs parent;
	zoomFrame zf;
	Vector targets;
	Label targetinfo;
	java.awt.List targetlist;
	
	public targetFrame(pvs parent, pointtargets mytargets, imageCanvas mainCanvas, zoomFrame zf) {
		super("::point targets::");
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
														"potential y:" + Integer.toString(now.potentialy) + "                                                    ");
		bottom.add("Center", targetinfo);
		
		Panel south = new Panel();
		south.setLayout(new GridLayout(1, 3));
		south.add(new Button("link"));
		south.add(new Button("delete"));
		south.add(new Button("restore all"));
		bottom.add("South", south);

		this.add("South", bottom);		

		int locatex = getToolkit().getScreenSize().width  - getSize().width;
		int locatey = getToolkit().getScreenSize().height - getSize().height;
		if(locatex > 0 && locatey > 0)
			setLocation(locatex/2, locatey/2);
		
		pack();
	}	
	
	public void importtargets() {
		mytargets.importtargets();
		
		targetlist.removeAll();
		targets = mytargets.visibletargets;
		for(int i=0; i < targets.size(); i++) {
			pt now = (pt) targets.elementAt(i);
			targetlist.add(now.devname);
		}

		zf.myzoomer.repaint();
		mainCanvas.repaint();
	}
	
	public boolean action (Event e, Object o) {
		if ("link".equals(o) && targetlist.getSelectedItem() != null) {
			mytargets.actualize(targetlist.getSelectedItem(), zf.myzoomer.targetx, zf.myzoomer.targety);
			zf.myzoomer.repaint();
			mainCanvas.repaint();
			for(int i=0; i < targets.size(); i++) {
				pt now = (pt) targets.elementAt(i);
				if(now.devname == targetlist.getSelectedItem()) {
					targetinfo.setText("potential x:" + Integer.toString(now.potentialx) + " " +
																	"potential y:" + Integer.toString(now.potentialy) + " " +
																	"actual x:" + Integer.toString(now.actualx) + " " +
																	"actual y:" + Integer.toString(now.actualy) );
					return true;
				}
			}
		}
		if ("delete".equals(o) && targetlist.getSelectedItem() != null) {
			mytargets.remove(targetlist.getSelectedItem());
			targetlist.remove(targetlist.getSelectedIndex());
			zf.myzoomer.repaint();
			mainCanvas.repaint();
			return true;
		}
		if ("restore all".equals(o)) {
			mytargets.resetvisible();
			targetlist.removeAll();
			targets = mytargets.visibletargets;
			for(int i=0; i < targets.size(); i++) {
				pt now = (pt) targets.elementAt(i);
				targetlist.add(now.devname);
			}
			zf.myzoomer.repaint();
			mainCanvas.repaint();
			return true;
		}
		return false;
	}	

	public boolean handleEvent (Event e) {
		if (e.id == Event.LIST_SELECT) {
			for(int i=0; i < targets.size(); i++) {
				pt now = (pt) targets.elementAt(i);
				if(now.devname == targetlist.getSelectedItem()) {
					if(now.actualized)
						targetinfo.setText("potential x:" + Integer.toString(now.potentialx) + " " +
																		"potential y:" + Integer.toString(now.potentialy) + " " +
																		"actual x:" + Integer.toString(now.actualx) + " " +
																		"actual y:" + Integer.toString(now.actualy) );
					else
						targetinfo.setText("potential x:" + Integer.toString(now.potentialx) + " " +
																		"potential y:" + Integer.toString(now.potentialy) );
					
					return true;
				}
			}		
			return false;
		}
		if (e.id == Event.WINDOW_DESTROY) {
			parent.exittargets();
			dispose();
			return true;
		}
		return super.handleEvent(e);
	}
}

