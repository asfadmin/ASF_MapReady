
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//HELP WINDOW

class helpFrame extends Frame {
	String directory;
	TextArea textarea;
	
	public helpFrame(String directory, String filename) {
		super("pvs help");
		
		this.setBackground(Color.lightGray);
		textarea = new TextArea("", 40, 40);
		textarea.setFont(new Font("Serif", Font.PLAIN, 14));
		textarea.setEditable(false);
		this.add("Center", textarea);
		
		Panel p = new Panel();
		p.add(new Button("general"));
		p.add(new Button("mask"));
		p.add(new Button("smask"));
		p.add(new Button("targets"));
		p.add(new Button("commands"));
		p.add(new Button("close"));
		p.setBackground(Color.lightGray);
		this.add("South", p);		
		
		if(directory == null) {
			File f;
			if((filename != null) && (f = new File(filename)).isAbsolute()) {
				directory = f.getParent();
				filename = f.getName();
			}
			else directory = System.getProperty("user.dir");
		}
		
		this.directory = directory;
		setFile(directory, filename);
		int locatex = getToolkit().getScreenSize().width  - getSize().width;
		int locatey = getToolkit().getScreenSize().height - getSize().height;
		if(locatex > 0 && locatey > 0)
			setLocation(2*locatex/3, locatey/8);
	}
	
	public void setFile(String directory, String filename) {
		if((filename == null) || (filename.length() == 0)) {return;}
		
		File f;
		FileReader in = null;
		
		try {
			f = new File(directory, filename);
			in = new FileReader(f);
			int size = (int) f.length();
			char[] data = new char[size];
			int chars_read = 0;
			while(chars_read < size)
				chars_read += in.read(data, chars_read, size-chars_read);
			textarea.setText(new String(data));
		}
		catch(Exception e) {
			textarea.setText("error opening file: " + directory + filename);
			logger.log("error opening file: " + directory + filename);
		}
		finally {
			try {
				if(in != null)
					in.close();
			}
			catch(IOException e) {}
		}
	}
	
	public boolean action (Event e, Object o) {
		if ("general".equals(o)) {
			setFile(directory, "general.help");
			return true;
		}
		if ("mask".equals(o)) {
			setFile(directory, "mask.help");
			return true;
		}
		if ("smask".equals(o)) {
			setFile(directory, "smask.help");
			return true;
		}
		if ("targets".equals(o)) {
			setFile(directory, "targets.help");
			return true;
		}
		if ("commands".equals(o)) {
			setFile(directory, "commands.help");
			return true;
		}
		if ("close".equals(o)) {
			this.dispose();
			return true;
		}
		return false;
	}	

	public boolean handleEvent (Event e) {
		if (e.id == Event.WINDOW_DESTROY) {
			dispose();
			return true;
		}
		return super.handleEvent(e);
	}
}

