
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//CONTRAST

class contDialog extends Dialog {
	pvs mainFrame;
	public int low;
	public int high;
	private TextField lowtext;
	private TextField hightext;
	private int defaultlow;
	private int defaulthigh;
	Scrollbar lowscroll;
	Scrollbar highscroll;
	
	
	contDialog (pvs parent, int low, int high, int defaultlow, int defaulthigh) {
		super (parent, "image contrast");
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
		south.add(new Button ("okay"));
		south.add(new Button ("default"));
		add ("South", south);
		setSize(400, 180);
	}
		
	public boolean handleEvent (Event evt) {
		if (evt.id == Event.KEY_PRESS) {
			if (evt.key == 10) {
				try {
					low = Integer.parseInt(lowtext.getText());
					high = Integer.parseInt(hightext.getText());
				}
				catch (Exception ex) {
					System.out.print("naughty boy");
				}
				
				lowscroll.setValue(low);
				highscroll.setValue(high);
				mainFrame.newcontrast(low, high);

				return true;
			}
		}
		if ((evt.id == Event.SCROLL_PAGE_DOWN) ||
				(evt.id == Event.SCROLL_LINE_DOWN) ||
				(evt.id == Event.SCROLL_ABSOLUTE) ||
				(evt.id == Event.SCROLL_PAGE_UP) ||
				(evt.id == Event.SCROLL_LINE_UP)) {
			
			if(evt.target == lowscroll) {
				low = lowscroll.getValue();
				lowtext.setText(Integer.toString(low));
				mainFrame.newcontrast(low, high);
			}
			if(evt.target == highscroll) {
				high = highscroll.getValue();
				hightext.setText(Integer.toString(high));
				mainFrame.newcontrast(low, high);
			}
		}
		if (evt.id == Event.WINDOW_DESTROY) {
			dispose();
			return true;
		}
		return super.handleEvent(evt);
	}

	public boolean action (Event e, Object o) {
		if ("okay".equals(o)) {
			try {
				low = Integer.parseInt(lowtext.getText());
				high = Integer.parseInt(hightext.getText());
			}
			catch (Exception ex) {
				System.out.print("naughty boy");
			}
			finally {
				dispose();
				return true;
			}
		}
		if ("default".equals(o)) {
			low = defaultlow;
			high = defaulthigh;
			lowscroll.setValue(low);
			highscroll.setValue(high);
			mainFrame.newcontrast(low, high);
			lowtext.setText(Integer.toString(low));
			hightext.setText(Integer.toString(high));
			return true;
		}
		return super.action(e, o);
	}
}

