
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//COMMAND OUTPUT WINDOW

class textFrame extends Frame {
	String directory;
	TextArea textarea;
	
	public textFrame(String directory, String filename) {
		super(filename);
		
		this.setBackground(Color.lightGray);
		textarea = new TextArea("", 24, 80);
		textarea.setFont(new Font("MonoSpaced", Font.PLAIN, 12));
		textarea.setEditable(false);
		this.add("Center", textarea);
		
		Panel p = new Panel();
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
			this.setTitle("command thing");
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

