////////////////////////////////////////////////////////////////////////////////
//
//	alaska sar facility
//	process validation system
//	(and ultracool imageviewing application)
//
//	creator: dann toliver
//
//	today is june16, 2000. i'm thinking of adding a changes log. it will
//		look something like this:
//
//	july20: last day of work on pvs. completed the site where it will rest
//		for the forseeable future. fixed numerous problems - wiggling
//		smasks, menu disabling at inappropriate moments, various
//		aesthetic faux pas. turning the project over to the capable
//		hands of jay cable. it's been fun while it lasted. hope you
//		enjoy it.
//
//	june29:	ingests asf data. hooray.  
//
//	june25:	pvs exports images from the zoom window now. it can auto export
//		when ever the scene changes, generating sequences of pgm image
//		data that can be synthesized into MPEGs.
//
//	june19: targets don't move around nearly as much anymore. also, the
//		yellowing accuracy has increased dramatically. 
//
//	june16: added 'best quality' mode. this is sick, dog.
//
//	june15:	made popup things popup in nice places; added keyboard shortcuts
//		to the menus, an about box, and this changes log. eating yogurt.
//
//	june13: delivered beta2 to operators complete with process meters,
//		glowing blue box, and enhanced flythrough mode. also updated
//		(rewrote) the help files, and added some cosmetic polish.
//
//	may31:	delivered beta1 to operators, to rave reviews and cries of "take
//		off your shirt!". this will be considered an auspicious start.
//
////////////////////////////////////////////////////////////////////////////////


import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


class pvs extends Frame{
	imageCanvas mainCanvas; //try to keep track of everything at this level
	zoomFrame zf;    	//this has it's own canvas
	importImage im;  	//the far too complicated image object
	mask imagemask;  	//el mask, natch.
	helpFrame help;
	int locatex;
	int locatey;
	int pensize = 1;
	int imagesize = 400;
	int imagequality = 1;
	Panel mtools;
	boolean drag = false;
	boolean unmask = false;
	private int zoommaskcount = 0;
	targetFrame targetframe;
	
	//preferences and whatnot...
	
	String preffile = "pvs.pref";	//echoed in logger class...
	String imagedir = null;
	String maskdir = null;
	String helpdir = null;
	String logfile = null;	//actually, this gets eaten in the log class itself.
	
	String xy_plotter = null; //to make another command: put a variable here,
				  //add a menu option, add a line to the pref file,
				  //copy the xy_plotter section of the pref file opener
				  //and alter it for the new command, then do the same
				  //with the xy_plotter section of the 'action' function.

	String outputfile = null;
	
	//menuing-ing-ing
	
	Menu fileMenu;
	Menu optionMenu;
	Menu maskMenu;
	Menu commandMenu;
	Menu targetMenu;
	Menu helpMenu;
	
	MenuItem printMenuItem;
	MenuItem zoomMenuItem;
	MenuItem contrastMenuItem;
	
	MenuItem enterMenuItem;
	MenuItem startMenuItem;
	MenuItem stopMenuItem;
	MenuItem exitMenuItem;
	CheckboxMenuItem unmaskMenuItem;		//check boxing.
	MenuItem importMenuItem;
	MenuItem exportMenuItem;
	
	MenuItem outputterMenuItem;
	MenuItem otherMenuItem;
	
	MenuItem entertargetsMenuItem;
	MenuItem endtargetsMenuItem;
	CheckboxMenuItem brightMenuItem;
	MenuItem importtargetsMenuItem;
	MenuItem exporttargetsMenuItem;
	
	private Properties printprefs = new Properties(); //for printing

	pvs() {
		super("Sprocket");
		
		MenuBar mb = new MenuBar();

		fileMenu = new Menu("File");
		optionMenu = new Menu("Option");
		maskMenu = new Menu("Mask");
		commandMenu = new Menu("Commands");
		targetMenu = new Menu("Targets");
		helpMenu = new Menu("Help");

		fileMenu.add(new MenuItem("open", new MenuShortcut(KeyEvent.VK_O)));
		printMenuItem = fileMenu.add(new MenuItem("print"));
		fileMenu.addSeparator();
		fileMenu.add(new MenuItem("exit"));

		zoomMenuItem = optionMenu.add(new MenuItem("zoom window", new MenuShortcut(KeyEvent.VK_Z)));
		contrastMenuItem = optionMenu.add(new MenuItem("contrast", new MenuShortcut(KeyEvent.VK_C)));

		unmaskMenuItem = new CheckboxMenuItem("unmask mode");
		enterMenuItem = maskMenu.add(new MenuItem("enter mask mode", new MenuShortcut(KeyEvent.VK_M)));
		startMenuItem = maskMenu.add(new MenuItem("start masking"));
		stopMenuItem = maskMenu.add(new MenuItem("stop masking"));
		exitMenuItem = maskMenu.add(new MenuItem("exit mask mode"));
		maskMenu.addSeparator();
		maskMenu.add(unmaskMenuItem);
		maskMenu.addSeparator();
		importMenuItem = maskMenu.add(new MenuItem("import mask"));
		exportMenuItem = maskMenu.add(new MenuItem("export mask"));
		
		unmaskMenuItem.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {unmask = !unmask;}
		});

		outputterMenuItem = commandMenu.add(new MenuItem("xy plotter"));
		commandMenu.addSeparator();
		otherMenuItem = commandMenu.add(new MenuItem("other..."));
		
		brightMenuItem = new CheckboxMenuItem("brightest pixel", true);
		entertargetsMenuItem = targetMenu.add(new MenuItem("enter target mode", new MenuShortcut(KeyEvent.VK_T)));
		endtargetsMenuItem = targetMenu.add(new MenuItem("exit target mode"));
		targetMenu.addSeparator();
		targetMenu.add(brightMenuItem);
		targetMenu.addSeparator();
		importtargetsMenuItem = targetMenu.add(new MenuItem("import targets"));
		exporttargetsMenuItem = targetMenu.add(new MenuItem("export targets"));
		
		brightMenuItem.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				zf.myzoomer.bright = !zf.myzoomer.bright; 
				zf.myzoomer.repaint();
			}
		});

		helpMenu.add(new MenuItem("general"));
		helpMenu.add(new MenuItem("mask"));
		helpMenu.add(new MenuItem("smask"));
		helpMenu.add(new MenuItem("targets"));
		helpMenu.add(new MenuItem("commands"));
		helpMenu.addSeparator();
		helpMenu.add(new MenuItem("about pvs..."));

		mb.setHelpMenu(helpMenu);
		mb.add(fileMenu);
		mb.add(optionMenu);
		mb.add(maskMenu);
		mb.add(commandMenu);
		mb.add(targetMenu);
		mb.add(helpMenu);
		setMenuBar(mb);

		mainCanvas = new imageCanvas(this);
		
		mtools = new Panel();
		mtools.setLayout(new GridLayout(5, 1));
		mtools.add(new Button ("1"));
		mtools.add(new Button ("3"));
		mtools.add(new Button ("7"));
		mtools.add(new Button ("10"));
		mtools.add(new Button ("20"));
		
		this.add("Center", mainCanvas);
		optionMenuEnabler(false);
		commandMenuEnabler(false);
		maskMenuEnabler(false);
		targetMenuEnabler(false);
		
		preferences();
		
		setResizable(false);
		setSize(imagesize + 8, imagesize + 46);

/* *** Begin code which breaks on multi-screen desktops ***********************/
		locatex = (getToolkit().getScreenSize().width  - getSize().width)/3;
		locatey = (getToolkit().getScreenSize().height - getSize().height)/3;
		if(locatex > 0 && locatey > 0)
			setLocation(locatex, locatey);
/* *** End code which breaks on multi-screen desktops *************************/
		
		show();
	}

	public void optionMenuEnabler(boolean b) {
		printMenuItem.setEnabled(b);
		zoomMenuItem.setEnabled(b);
		contrastMenuItem.setEnabled(b);
	}
	
	public void maskMenuEnabler(boolean b) {
		enterMenuItem.setEnabled(b);
		importMenuItem.setEnabled(b);
		if(!b)
			maskFunctionsMenuEnabler(b);
	}

	public void maskFunctionsMenuEnabler(boolean b) {
		startMenuItem.setEnabled(b);
		stopMenuItem.setEnabled(b);
		exitMenuItem.setEnabled(b);
		unmaskMenuItem.setEnabled(b);
		exportMenuItem.setEnabled(b);
	}

	public void commandMenuEnabler(boolean b) {
		outputterMenuItem.setEnabled(b);
		otherMenuItem.setEnabled(b);
	}
	
	public void targetMenuEnabler(boolean b) {
		entertargetsMenuItem.setEnabled(b);
		if(!b)
			targetFunctionsMenuEnabler(b);
	}
	
	public void targetFunctionsMenuEnabler(boolean b) {
		endtargetsMenuItem.setEnabled(b);		
		brightMenuItem.setEnabled(b);		
		importtargetsMenuItem.setEnabled(b);		
		exporttargetsMenuItem.setEnabled(b);		
	}
	
	public void preferences() {
		File f;
		boolean foundFLAG = false;
		
		if((f = new File("pvs.pref")).exists()) {
			foundFLAG = true;
		}
		else {
			StringTokenizer s = new StringTokenizer(System.getProperty("java.class.path"), File.pathSeparator);
			while(s.hasMoreTokens()) {
				if((f = new File(s.nextToken(), preffile)).exists()) {
					foundFLAG = true;
					break;
				}
			}
		}

		if(!foundFLAG) {return;}

		try {
			FileReader fr = new FileReader(f);
			LineNumberReader in = new LineNumberReader(fr);
		
			boolean id = false;
			while(!id) {
				String thisline = in.readLine();
				StringTokenizer s = new StringTokenizer(thisline);
				while(s.hasMoreTokens()) {
					if(s.nextToken().equalsIgnoreCase("imagedir")) {
						imagedir = thisline.substring(thisline.indexOf(s.nextToken()));
						id = true;
						break;
					}
				}
			}

			boolean md = false;
			while(!md) {
				String thisline = in.readLine();
				StringTokenizer s = new StringTokenizer(thisline);
				while(s.hasMoreTokens()) {
					if(s.nextToken().equalsIgnoreCase("maskdir")) {
						maskdir = thisline.substring(thisline.indexOf(s.nextToken()));
						md = true;
						break;
					}
				}
			}

			boolean hd = false;
			while(!hd) {
				String thisline = in.readLine();
				StringTokenizer s = new StringTokenizer(thisline);
				while(s.hasMoreTokens()) {
					if(s.nextToken().equalsIgnoreCase("helpdir")) {
						helpdir = thisline.substring(thisline.indexOf(s.nextToken()));
						hd = true;
						break;
					}
				}
			}

			boolean lf = false;	//the logfile variable doesn't actually get used here. . . (yet)
			while(!lf) {
				String thisline = in.readLine();
				StringTokenizer s = new StringTokenizer(thisline);
				while(s.hasMoreTokens()) {
					if(s.nextToken().equalsIgnoreCase("logfile")) {
						logfile = thisline.substring(thisline.indexOf(s.nextToken()));
						lf = true;
						break;
					}
				}
			}

			boolean xy = false;
			while(!xy) {
				String thisline = in.readLine();
				StringTokenizer s = new StringTokenizer(thisline);
				while(s.hasMoreTokens()) {
					if(s.nextToken().equalsIgnoreCase("xy_plotter")) {
						xy_plotter = thisline.substring(thisline.indexOf(s.nextToken()));
						xy = true;
						break;
					}
				}
			}

			boolean of = false;
			while(!of) {
				String thisline = in.readLine();
				StringTokenizer s = new StringTokenizer(thisline);
				while(s.hasMoreTokens()) {
					if(s.nextToken().equalsIgnoreCase("outputfile")) {
						outputfile = thisline.substring(thisline.indexOf(s.nextToken()));
						of = true;
						break;
					}
				}
			}
			
			fr.close();
		}
		catch (Exception e){
			logger.log("error opening preferences file.");
		}	
	}
	
	public void newcontrast(int low, int high) {
		im.colourarray(low, high);
		im.newcontrast = true;
		im.recolour();
		mainCanvas.updateImage(im);
		if(zf instanceof zoomFrame) 
			zf.myzoomer.repaint();	
	}

	public void exittargets() {
		targetFunctionsMenuEnabler(false);
		
		targetframe.dispose();
		
		zf.smaskMenuEnabler(true);
		maskMenuEnabler(true);
		
		mainCanvas.updateTargets(null);
		zf.updateTargets(null);	
	}
	
	public void commander(String commanddirectory, String commandfilename) {	//(this is stupid.)
		String imagefile = im.filename.substring(0, im.filename.length() - 6);	//cut of the .data1 part
		String maskfile;
		String outputfilename;
		String outputdirectory;
		procDialog pd = null;
		
		if(outputfile != null) {
			outputdirectory = outputfile.substring(0, outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);
			outputfilename = outputfile.substring(outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);		
		}
		else {
			outputdirectory = commanddirectory;
			outputfilename = "output.txt";
		}
		
		if(!maskdir.endsWith(System.getProperty("file.separator")))
			 maskdir = maskdir + System.getProperty("file.separator");
		maskfile = maskdir + "default.mask";
		
		if(imagemask != null)
			maskfile = imagemask.filename;
		else if((zf != null) && (zf.myzoomer.simagemask != null))
			maskfile = zf.myzoomer.simagemask.filename;
		
		commDialog comm = new commDialog(this, maskfile, im.filename, outputfile);
		if(getToolkit().getScreenSize().width  - comm.getSize().width  > 0 &&
			 getToolkit().getScreenSize().height - comm.getSize().height > 0) {
				comm.setLocation((getToolkit().getScreenSize().width  - comm.getSize().width)/2, 
												 (getToolkit().getScreenSize().height - comm.getSize().height)/2);
		}
		comm.show();
		comm.dispose();
		
		if(comm.cancelled) {return;}
					
		//use the return from comm here
		String args[] = new String[8];  //the number should be from commDialog

		if(commanddirectory != null) {args[0] = commanddirectory + commandfilename;}
		else {args[0] = commandfilename;}

		if(comm.imagefile != null) {args[1] = comm.imagefile.substring(0, comm.imagefile.length() - 6);}
		else {args[1] = imagefile;}
		
		if(comm.xthing != null) {args[2] = comm.xthing;}
		else {args[2] = "look";}
		
		if(comm.ything != null) {args[3] = comm.ything;}
		else {args[3] = "power";}
		
		if(comm.mthing != null) {args[4] = comm.mthing;}
		else {args[4] = "on";}
		
		args[5] = Integer.toString(comm.bin);
		
		if(comm.maskfile != null) {args[6] = comm.maskfile;}
		else {args[6] = maskfile;}
		
		if(comm.outputfile != null) {
			args[7] = comm.outputfile;
			outputdirectory = comm.outputfile.substring(0, comm.outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);
			outputfilename = comm.outputfile.substring(comm.outputfile.lastIndexOf(System.getProperty("file.separator")) + 1);
		}
		else {args[7] = outputfilename;}
				
		setCursor(new Cursor(Cursor.WAIT_CURSOR));


		
		try {
			Runtime run = Runtime.getRuntime();
			Process pr = run.exec(args);

			//process meter stuff
			pd = new procDialog(this, "running " + commandfilename + "...");	
			if(locatex > 0 && locatey > 0)
				pd.setLocation(locatex + 3, locatey + 41);	//arbitrary... yuck.
			pd.show();
			int done = 0;
		
			StreamTokenizer st = new StreamTokenizer(pr.getInputStream());
			try {
				while(done < 99) {
					if(st.nextToken() == st.TT_NUMBER) {
						if((int) st.nval > done) {
							done = (int) st.nval;
							pd.process(done);
						}
					}
				}
			}
			catch(IOException e) {
				System.out.println("stupid." + e);
				pr.waitFor();		//uhm. . . right.				
			}
			pr.waitFor(); 
		}
		catch(Exception e) {
			logger.log("error running command: " + args[0]);
		}
		finally {
			if(pd != null)
				pd.dispose();
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		
		textFrame text = new textFrame(outputdirectory, outputfilename);
		text.pack();
		if(getToolkit().getScreenSize().width  - text.getSize().width  > 0 &&
			 getToolkit().getScreenSize().height - text.getSize().height > 0) {
				text.setLocation((getToolkit().getScreenSize().width  - text.getSize().width)/2, 
												 (getToolkit().getScreenSize().height - text.getSize().height)/2);
		}
		text.show();
	}

	public boolean action(Event evt, Object obj) {  //this event handling mechanism is depreciated -
		String label = (String)obj;										//i really should be registering event listeners
		if (evt.target instanceof MenuItem) {					//with the individual menus. 
			
			//FILE MENU
			
			if (label.equals("open")) {			//open a new image

				prefDialog pref = new prefDialog(this, imagesize, imagequality);
				pref.pack();
				if(locatex > 0 && locatey > 0)
					pref.setLocation(locatex + 3, locatey + 41);	
				pref.show();
				pref.dispose();  
				
				if(pref.cancelled) {return true;}
				
				if(pref.cksize.getSelectedCheckbox().getLabel() == "200 x 200") 
					imagesize = 200;
				else if(pref.cksize.getSelectedCheckbox().getLabel() == "400 x 400")
					imagesize = 400;
				else if(pref.cksize.getSelectedCheckbox().getLabel() == "800 x 800")
					imagesize = 800;
				
				if(pref.ckqual.getSelectedCheckbox().getLabel() == "good") 
					imagequality = 1;
				else if(pref.ckqual.getSelectedCheckbox().getLabel() == "better")
					imagequality = 9;
				else if(pref.ckqual.getSelectedCheckbox().getLabel() == "best")
					imagequality = 0;
				
				FileDialog f = new FileDialog(this, "load image", FileDialog.LOAD);
				if(imagedir != null)
					f.setDirectory(imagedir);
				f.show();
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				f.dispose();
				String filename = f.getFile();
				
				if(filename != null) {
					imagedir = f.getDirectory();
					filename = imagedir + filename;

					if(im instanceof importImage) {		//close it if it's already there.
						boolean done = im.closeImage();	//use this value for something. . . 
						im = null;
					}
				
					im = new importImage(this, filename, imagesize, imagequality);
					if(im.im == null) {
						setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
						return true;
					}

					mainCanvas.updateImage(im);
					if(zf instanceof zoomFrame) 
						zf.repaint();		
					
					if(targetframe != null) {
						exittargets();
					}
					
					if(imagemask != null) {
						imagemask = null;
						maskFunctionsMenuEnabler(false);
						mainCanvas.clearMask();
						if(zf != null)
							zf.updateMask(imagemask); 
						this.remove(mtools);
					}

					if(zf != null) {
						if(zf.myzoomer.simagemask != null) {
							zf.myzoomer.unsmasking();
							mainCanvas.clearSmask();
							maskMenuEnabler(true);
							zf.myzoomer.fly = false;
							zf.setResizable(true);
						}
					}

					mainCanvas.setSize(imagesize, imagesize);
					setSize(imagesize + 8, imagesize + 46);		//yuck!!! do something different here.
//					mainCanvas.setSize(mainCanvas.getPreferredSize());
//					setSize(this.getPreferredSize());
					
					if(getToolkit().getScreenSize().width  < getSize().width  + locatex ||
						 getToolkit().getScreenSize().height < getSize().height + locatey) {
							locatex = (getToolkit().getScreenSize().width  - getSize().width)/3;
							locatey = (getToolkit().getScreenSize().height - getSize().height)/3;
							if(locatex > 0 && locatey > 0)
								setLocation(locatex, locatey);
					}
							
					optionMenuEnabler(true);
					commandMenuEnabler(true);
					maskMenuEnabler(true);
				}
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				return true;
			}
			if (label.equals("print")) {    
				mainCanvas.printit(this, printprefs);
				return true;
			}			
			if (label.equals("exit")) { 
			  System.exit(0);
			  return true;
			}
			
			//OPTION MENU
			
			if (label.equals("zoom window")) {  //show or hide zoom window
				if(zf instanceof zoomFrame) {
					if(zf.isShowing())
						zf.dispose();	//switched to dispose because of the magic minimizing window... 
//					zf.hide();		//but it might not be the best solution.
					else
						zf.show();
				}
				else {
					if(mainCanvas.im != null) {
				  	zf = new zoomFrame(this);
				  	if(mainCanvas.x != -1 && mainCanvas.y != -1) {
							zf.zoomImage(mainCanvas.x, mainCanvas.y, im);
						}
						else {
							zf.zoomImage(imagesize/2, imagesize/2, im);
						}
						if(imagemask != null) {
							zf.updateMask(imagemask);
							zf.smaskMenuEnabler(false);
						}
					}
				}
				
				targetMenuEnabler(true);
			  
			  return true;
			}			
			if (label.equals("contrast")) {  //display contrast controls
				if(im != null) {
					contDialog cont = new contDialog(this, im.contrastlow, im.contrasthigh, 
																						im.defaultcontrastlow, im.defaultcontrasthigh);
					
					if(locatex > 0 && locatey > 0)	{
						if(locatey + getSize().height + cont.getSize().height < getToolkit().getScreenSize().height)
							cont.setLocation(locatex, locatey + getSize().height);
						else
							cont.setLocation(locatex + 3, locatey + 41);
					}	
					
					cont.show();					
					newcontrast(cont.low, cont.high);
					
					return true;
				}
			}
				
			//MASK MENU
			
			if (label.equals("enter mask mode")) {    //enter mask mode & start drawing
				if(im != null) {
					if(imagemask == null) {
						imagemask = new mask(this, imagesize, im.width, im.height);
						imagemask.drawing = true;
						this.add("East", mtools);
						setSize(this.getPreferredSize());
						show();
						maskFunctionsMenuEnabler(true);
						if(zf != null) {
							zf.smaskMenuEnabler(false);
							targetMenuEnabler(false);
						}
						return true;
					}
					else 
						imagemask.drawing = true;
				}
			}
			if (label.equals("start masking")) {    //start drawing
				if(im != null) {
					if(imagemask != null)
						imagemask.drawing = true;
					return true;
				}
			}			
			if (label.equals("stop masking")) {    //stop drawing
				if(imagemask != null)
					imagemask.drawing = false;
				if(zf != null)
					zf.updateMask(imagemask);
				return true;
			}			
			if (label.equals("exit mask mode")) {    //exit mask mode & destroy mask
				imagemask = null;
				mainCanvas.clearMask();
				if(zf != null)
					zf.updateMask(imagemask);
				this.remove(mtools);
				setSize(this.getPreferredSize());
				maskFunctionsMenuEnabler(false);
				if(zf != null) {
					zf.smaskMenuEnabler(true);
					targetMenuEnabler(true);
				}
				return true;
			}			
			if (label.equals("import mask")) {    //import mask from bitmap
				if(im != null) {
					FileDialog f = new FileDialog(this, "import mask", FileDialog.LOAD);
					if(maskdir != null)
						f.setDirectory(maskdir);
					f.show();
					f.dispose();
					String filename = f.getFile();
					
					if(filename != null) {
						filename = f.getDirectory() + filename;			
						imagemask = new mask(this, filename, imagesize, im.width, im.height);
						imagemask.drawing = true;
						if(zf != null) {
							zf.smaskMenuEnabler(false);
							targetMenuEnabler(false);
						}
						this.add("East", mtools);
						setSize(this.getPreferredSize());
						show();

						maskFunctionsMenuEnabler(true);
						imagemask.updateImage();
						mainCanvas.updateMask(imagemask);
						return true;
					}
				}
				return false;
			}			
			if (label.equals("export mask")) {    //export mask to bitmap
				if(im != null) {
					if(imagemask != null) {
						FileDialog f = new FileDialog(this, "export mask", FileDialog.SAVE);
						if(maskdir != null)
							f.setDirectory(maskdir);
						f.show();
						f.dispose();
						setCursor(new Cursor(Cursor.WAIT_CURSOR));
						String filename = f.getFile();
		
						if(filename != null) {
							filename = f.getDirectory() + filename;			
							imagemask.exportMask(filename);
							setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
							return true;
						}
						else {
							setCursor(new Cursor(Cursor.DEFAULT_CURSOR));						
						}
					}
				}
				return false;
			}
			
			//COMMANDS

			if (label.equals("xy plotter")) {    //do the outputter command thing.
				if(im != null) {
					if(xy_plotter != null) {
						String xy_directory = xy_plotter.substring(0, xy_plotter.lastIndexOf(System.getProperty("file.separator")) + 1);
						String xy_filename = xy_plotter.substring(xy_plotter.lastIndexOf(System.getProperty("file.separator")) + 1);
						commander(xy_directory, xy_filename);
					}
					else {
						FileDialog f = new FileDialog(this, "select xy_plotter", FileDialog.LOAD);
						f.show();
						String filename = f.getFile();
		
						if(filename != null) {
							commander(f.getDirectory(), filename);  //should get this from somewhere else. . . 
						}				
					}
					return true;
				}
				return false;
			}
			if (label.equals("other...")) {    //do the outputter command thing.
				if(im != null) {
					FileDialog f = new FileDialog(this, "select command", FileDialog.LOAD);
					f.show();
					String filename = f.getFile();
	
					if(filename != null) {
						commander(f.getDirectory(), filename);  //should get this from somewhere else. . . 
					}
					
					return true;
				}
				return false;
			}
			
			//TARGETS
			
			if(label.equals("enter target mode")) {
				if(im != null && zf != null) {
					if(targetframe != null && targetframe.isShowing()) {return true;}
				
					pointtargets targets = new pointtargets(im.filename.substring(0, im.filename.length() - 6));
					
					targetframe = new targetFrame(this, targets, mainCanvas,  zf);	//show the target thing.
					targetframe.show();
					
					zf.smaskMenuEnabler(false);
					maskMenuEnabler(false);
					
					mainCanvas.updateTargets(targets);
					zf.updateTargets(targets);
										
					targetFunctionsMenuEnabler(true);
					return true;
				}
				return false;
			}
			if(label.equals("exit target mode")) {
				exittargets();				
				return true;
			}
			if(label.equals("import targets")) {
				if(targetframe != null) 
					targetframe.importtargets();	
				return true;
			}
			if(label.equals("export targets")) {
				if(targetframe != null) 
					targetframe.mytargets.exporttargets();	
				return true;
			}
			
			//HELP MENU

			if(label.equals("general")) {
				if(help == null) {
					help = new helpFrame(helpdir, "general.help");
					help.pack();
					help.show();
				}
				else {
					if(help.isShowing())
						help.setFile(helpdir, "general.help");
					else {
						help = new helpFrame(helpdir, "general.help");
						help.pack();
						help.show();
					}
				}
				return true;
			}
			if(label.equals("mask")) {
				if(help == null) {
					help = new helpFrame(helpdir, "mask.help");				
					help.pack();
					help.show();
				}
				else {
					if(help.isShowing())
						help.setFile(helpdir, "mask.help");
					else {
						help = new helpFrame(helpdir, "mask.help");				
						help.pack();
						help.show();
					}
				}
				return true;
			}
			if(label.equals("smask")) {
				if(help == null) {
					help = new helpFrame(helpdir, "smask.help");				
					help.pack();
					help.show();
				}
				else {
					if(help.isShowing())
						help.setFile(helpdir, "smask.help");
					else {
						help = new helpFrame(helpdir, "smask.help");				
						help.pack();
						help.show();
					}
				}
				return true;
			}
			if(label.equals("targets")) {
				if(help == null) {
					help = new helpFrame(helpdir, "targets.help");				
					help.pack();
					help.show();
				}
				else {
					if(help.isShowing())
						help.setFile(helpdir, "targets.help");
					else {
						help = new helpFrame(helpdir, "targets.help");				
						help.pack();
						help.show();
					}
				}
				return true;
			}
			if(label.equals("commands")) {
				if(help == null) {
					help = new helpFrame(helpdir, "commands.help");				
					help.pack();
					help.show();
				}
				else {
					if(help.isShowing())
						help.setFile(helpdir, "commands.help");
					else {
						help = new helpFrame(helpdir, "commands.help");				
						help.pack();
						help.show();
					}
				}
				return true;
			}
			if(label.equals("about pvs...")) {
				aboutDialog about = new aboutDialog(this);
				about.pack();
				about.setLocation((getToolkit().getScreenSize().width  - about.getSize().width)/2, 
													(getToolkit().getScreenSize().height - about.getSize().height)/2);	
				about.show();
				return true;
			}

			return false;
		}
		
		//masking pensize buttons
		
		if ("1".equals(obj)) {
			pensize = 1;
			if(imagemask != null)
				imagemask.drawing = true;
		}
		if ("3".equals(obj)) {
			pensize = 3;
			if(imagemask != null)
				imagemask.drawing = true;
		}
		if ("7".equals(obj)) {
			pensize = 7;
			if(imagemask != null)
				imagemask.drawing = true;
		}
		if ("10".equals(obj)) {
			pensize = 10;
			if(imagemask != null)
				imagemask.drawing = true;
		}
		if ("20".equals(obj)) {
			pensize = 20;
			if(imagemask != null)
				imagemask.drawing = true;
		}
		return false;
	}
	
	public boolean handleEvent(Event e) {
		if (e.id == Event.WINDOW_DESTROY) {
			System.exit(0);
			return true;
		}
		if (e.id == Event.WINDOW_MOVED) {
			locatex = getLocation().x;
			locatey = getLocation().y;
			return true;
		}
		if (e.id == Event.MOUSE_DRAG  || e.id == Event.MOUSE_DOWN) {
			if(e.id == Event.MOUSE_DRAG)
				drag = true;
			else
				drag = false;

			if(imagemask != null && imagemask.drawing) {	//if we're masking
				imagemask.updateImage(mainCanvas.x-pensize, mainCanvas.y-pensize, mainCanvas.x+pensize, mainCanvas.y+pensize, unmask);
				mainCanvas.updateMask(imagemask, pensize, unmask);
				if(zf != null) {
					int tx = mainCanvas.x;							//all this is to decide if the mask pen was inside 
					int ty = mainCanvas.y;							//the zoom window - it makes redrawing scads quicker.
					int offx; int offy;
					offx = (int)(zf.getSize().width/zf.myzoomer.ratio * imagesize)/im.width;
					offy = (int)(zf.getSize().height/zf.myzoomer.ratio * imagesize)/im.height;
					int zx1 = zf.getPosition().width - offx/2;		
					int zy1 = zf.getPosition().height - offy/2;	
					int zx2 = zf.getPosition().width + offx/2; 											 
					int zy2 = zf.getPosition().height + offy/2;

					
					if((((((zx1 < tx-pensize) && (tx-pensize < zx2)) || ((zx1 < tx+pensize) && (tx+pensize < zx2))) &&
						 (((zy1 < ty-pensize) && (ty-pensize < zy2)) || ((zy1 < ty+pensize) && (ty+pensize < zy2)))) ||
						 ((((tx-pensize < zx1) && (zx1 < tx+pensize)) || ((tx-pensize < zx2) && (zx2 < tx+pensize))) &&
						 (((ty-pensize < zy1) && (zy1 < ty+pensize)) || ((ty-pensize < zy2) && (zy2 < ty+pensize))))) &&
						 (zoommaskcount%6 == 1))  //heck, this will work for now.
							zf.updateMask(imagemask);
							zoommaskcount = 0;
					zoommaskcount++;
				}
				return true;
			}
			
			if(zf instanceof zoomFrame) {		//if nothing else is happening. . . 
				zf.zoomImage(mainCanvas.x, mainCanvas.y, im);
			}
			return true;
		}
		if (e.id == Event.MOUSE_UP) {
				if(zf instanceof zoomFrame)
					zf.updateMask(imagemask);
			return true;
		} 
		
		return super.handleEvent(e);
	}

	public static void main(String args[]) {
		pvs tf = new pvs();
  }
}

