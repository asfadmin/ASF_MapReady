
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//COMMAND OPTIONS

class commDialog extends Dialog {
	public String outputfile;
	public String imagefile;
	public String maskfile;
	public String xthing;
	public String ything;
	public String mthing;
	public int bin;
	boolean cancelled;
	pvs mainFrame;
	TextField outputtextfield;
	TextField imagetextfield;
	TextField masktextfield;
	TextField bintextfield;
	Choice xchoice;
	Choice ychoice;
	Choice mchoice;
	
	commDialog (pvs parent, String maskfile, String imagefile, String outputfile) {
		super (parent, "command console", true);
		this.setBackground(Color.lightGray);

		mainFrame = parent;
		this.imagefile = imagefile;
		this.maskfile = maskfile;
		this.outputfile = outputfile;
		
		outputtextfield = new TextField(outputfile, 40);
		Button outputbutton = new Button("browse output");
		imagetextfield = new TextField(imagefile, 40);
		Button imagebutton = new Button("browse image");
		masktextfield = new TextField(maskfile, 40);
		Button maskbutton = new Button("browse mask");
		bintextfield = new TextField("500", 5);

		xchoice = new Choice();
		xchoice.add("incidence");
  		xchoice.add("look");
  		xchoice.add("slant");
 	 	xchoice.add("ground");
 	 	xchoice.add("pixel");
		ychoice = new Choice();
		ychoice.add("dn");
	  	ychoice.add("power");
 	 	ychoice.add("sigma0");
 	 	ychoice.add("gama0");
		mchoice = new Choice();
		mchoice.add("on");
		mchoice.add("off");
		
		Panel firstpan = new Panel();
		firstpan.setLayout(new GridLayout(3,1));
		firstpan.setBackground(Color.lightGray);
		firstpan.add(new Label("image:"));
		firstpan.add(new Label("mask:"));
		firstpan.add(new Label("output:"));
		
		Panel secondpan = new Panel();
		secondpan.setLayout(new GridLayout(3,1));
		secondpan.setBackground(Color.lightGray);
		secondpan.add(imagetextfield);
		secondpan.add(masktextfield);
		secondpan.add(outputtextfield);
		
		Panel thirdpan = new Panel();
		thirdpan.setLayout(new GridLayout(3,1));
		thirdpan.setBackground(Color.lightGray);
		thirdpan.add(imagebutton);
		thirdpan.add(maskbutton);
		thirdpan.add(outputbutton);

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
		fourthpan.add(new Label("bin"));
		fourthpan.add(new Label("masked area"));
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
		south.add(new Button ("okay"));
		south.add(new Button ("cancel"));
		add ("South", south);

		pack();
	}
		
	public boolean handleEvent (Event evt) {
		if (evt.id == Event.KEY_PRESS) {
			if (evt.key == 10) {
				try {
					xthing = xchoice.getSelectedItem();
					ything = ychoice.getSelectedItem();
					mthing = mchoice.getSelectedItem();
					bin = Integer.parseInt(bintextfield.getText());
					outputfile = outputtextfield.getText();
					imagefile = imagetextfield.getText();
					maskfile = masktextfield.getText();
				}
				catch (Exception ex) {
					System.out.print("egad, man!");
				}
				finally {
					hide();
				}
				return true;
			}
		}
		if (evt.id == Event.WINDOW_DESTROY) {
			cancelled = true;
			dispose();
			return true;
		}
		return super.handleEvent(evt);
	}

	public boolean action (Event e, Object o) {
		if ("okay".equals(o)) {
			try {
				xthing = xchoice.getSelectedItem();
				ything = ychoice.getSelectedItem();
				mthing = mchoice.getSelectedItem();
				bin = Integer.parseInt(bintextfield.getText());
				outputfile = outputtextfield.getText();
				imagefile = imagetextfield.getText();
				maskfile = masktextfield.getText();
			}
			catch (Exception ex) {
				System.out.print("egad, man!");
			}
			finally {
				dispose();
				return true;
			}
		}
		if ("cancel".equals(o)) {
			cancelled = true;
			dispose();
			return true;
		}
		if ("browse image".equals(o)) {
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
				imagetextfield.setText(imagefile);
			}
			return true;
		}
		if ("browse mask".equals(o)) {
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
				masktextfield.setText(maskfile);
			}
			return true;
		}
		if ("browse output".equals(o)) {
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
				outputtextfield.setText(outputfile);
			}
			return true;
		}
		return super.action(e, o);
	}
}

