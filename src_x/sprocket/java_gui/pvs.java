//////////////////////////////////////////////////////////////////////////////////////////
//		
//			alaska sar facility
//			process validation system
//			(and ultracool imageviewing application)
//
//			creator: dann toliver
//
//			today is june16, 2000.
//			i'm thinking of adding
//			a changes log. it will
//			look something like this:
//
//			july20: last day of work on pvs.
//							completed the site where
//							it will rest for the forseeable
//							future. fixed numerous problems -
//							wiggling smasks, menu disabling 
//							at inappropriate moments, various
//							aesthetic faux pas. turning the
//							project over to the capable hands
//							of jay cable. it's been fun while
//							it lasted. hope you enjoy it.
//
//			june29:	ingests asf data. hooray.  
//
//			june25:	pvs exports images from the
//							zoom window now. it can auto
//							export whenever the scene changes,
//							generating sequences of pgm image
//							data that can be synthesized into MPEGs.
//
//			june19: targets don't move around
//							nearly as much anymore. also,
//							the yellowing accuracy has
//							increased dramatically. 
//
//			june16: added 'best quality' mode.
//							this is sick, dog.
//
//			june15:	made popup things popup in 
//							nice places; added keyboard
//							shortcuts to the menus, an
//							about box, and this changes
//							log. eating yogurt.
//
//			june13: delivered beta2 to operators
//							complete with process meters,
//							glowing blue box, and enhanced
//							flythrough mode. also updated
//							(rewrote) the help files, and
//							added some cosmetic polish.
//
//			may31:	delivered beta1 to operators,
//							to rave reviews and cries of
//							"take off your shirt!". this will 
//							be considered an auspicious start.
//
//////////////////////////////////////////////////////////////////////////////////////////


import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;

class pvs extends Frame{
	imageCanvas mainCanvas;   //try to keep track of everything at this level
	zoomFrame zf;    					//this has it's own canvas
	importImage im;  					//the far too complicated image object
	mask imagemask;  					//el mask, natch.
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
	String logfile = null;		//actually, this gets eaten in the log class itself.
	
	String xy_plotter = null;	//to make another command: put a variable here,
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
		super("this is not sprocket");
		
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
		
		locatex = (getToolkit().getScreenSize().width  - getSize().width)/3;
		locatey = (getToolkit().getScreenSize().height - getSize().height)/3;
		if(locatex > 0 && locatey > 0)
			setLocation(locatex, locatey);
		
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

//IMAGE PREFERENCES

class prefDialog extends Dialog {
	CheckboxGroup cksize = new CheckboxGroup();
	CheckboxGroup ckqual = new CheckboxGroup();
	boolean cancelled = false;

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
		south.add(new Button ("Okay"));
		add ("South", south);
		setResizable(true);
		pack();
	}
		
	public boolean handleEvent (Event e) {
		if (e.id == Event.WINDOW_DESTROY) {
			cancelled = true;
			dispose();
			return true;
		}
		return super.handleEvent(e);
	}

	public boolean action (Event e, Object o) {
		if ("Okay".equals(o)) {
			hide();
		}
		return super.action(e, o);
	}
}

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

//MAIN CANVAS

class imageCanvas extends Canvas{
	pvs mainFrame;
	importImage im;
	mask imagemask;
	smask simagemask;
	pointtargets targets;		//add a pointer to main window, or zoomFrame...
	int pensize = 0;
	Image pm;
	Image mm;
	int x = -1;
	int y = -1;
	Dimension position = new Dimension(0,0);		//these are for the glowing green box
	int zwidth  = 1;
	int zheight = 1;

	imageCanvas(pvs mainFrame) {
		this.mainFrame = mainFrame;
	}
	
	public void update(Graphics g) {
		paint(g);
	}

	public void updateImage (importImage im) {	
		this.im = im;
		repaint(); 	
	}

	public void updateMask (mask imagemask, int pensize, boolean unmask) {  
		this.imagemask = imagemask;													//redraw the just square the mask pen is on
		if(this.pensize != pensize) {												//when the user clicks, since alpha is slow.
			this.pensize = pensize;
			pm = imagemask.drawPen(pensize);
		}
		
		Image rm = im.eatImage(x, y, pensize);
		int startx = x - pensize;
		int starty = y - pensize;
		if(startx < 0) {startx = 0;}
		if(starty < 0) {starty = 0;}
		
		Graphics g = getGraphics();
		g.drawImage(rm, startx, starty, null);
		
		Graphics ga = mm.getGraphics();
		ga.drawImage(rm, startx, starty, null);

		if(!unmask) {
			g.drawImage(pm, x - pensize, y - pensize, null);
			ga.drawImage(pm, x - pensize, y - pensize, null);
		}
	}

	public void updateMask (mask imagemask) {  
		this.imagemask = imagemask;	
		repaint();
	}

	public void updateSmask (smask simagemask) {  
		this.simagemask = simagemask;	
		repaint();
	}

	public void updateTargets (pointtargets targets) {
		this.targets = targets;
		repaint();
	}

	public void updateZoom (Dimension position, int zwidth, int zheight) {
		this.position = position;
		this.zwidth  = zwidth;
		this.zheight = zheight;
		
		int startx = position.width  - zwidth/2;
		int starty = position.height - zheight/2;
		if(startx < 0) {startx = 0;}
		if(starty < 0) {starty = 0;}
		
		Graphics g = getGraphics();
		g.setColor(Color.blue);
		g.drawRect(startx, starty, zwidth, zheight);		
	}

	public void clearMask() {
		imagemask = null;
		repaint();
	}

	public void clearSmask() {
		simagemask = null;
		repaint();
	}

	public void clearTargets() {
		targets = null;
		repaint();
	}

	public void clearZoom () {
		Graphics g = getGraphics();
		
		zwidth  += 4;
		zheight += 4;
		
		int startx = position.width  - zwidth/2;
		int starty = position.height - zheight/2;
		if(startx < 0) {startx = 0;}
		if(starty < 0) {starty = 0;}
		
		CropImageFilter cif = new CropImageFilter(startx, starty, zwidth, zheight);
		Image kk = Toolkit.getDefaultToolkit().createImage(new FilteredImageSource(mm.getSource(), cif));
		g.drawImage(kk, startx, starty, null);
		
		
/*
		zwidth  += 4;
		zheight += 4;
	
		Image zm = im.eatImage(position, zwidth/2, zheight/2);

		int startx = position.width  - zwidth/2;
		int starty = position.height - zheight/2;
		if(startx < 0) {startx = 0;}
		if(starty < 0) {starty = 0;}
		
		Graphics g = getGraphics();
		g.drawImage(zm, startx, starty, null);
		
		if(targets instanceof pointtargets) {
			for(int i=0; i < targets.visibletargets.size(); i++) {
				Font f = new Font("SansSerif", Font.BOLD, 12);
				FontMetrics fm = Toolkit.getDefaultToolkit().getFontMetrics(f);
				pt now = (pt) targets.visibletargets.elementAt(i);
				if(now.actualized) {
					g.setColor(Color.red);
					g.setFont(f);
					
					int nowx = now.actualx * im.imagesize;
					nowx /= im.width;
					int nowy = now.actualy * im.imagesize;
					nowy /= im.height;
					
					int namex = fm.stringWidth(now.devname);
					int namey = fm.getHeight();

					if(((startx <= nowx - 5 && nowx - 5 <= startx + zwidth) || 
							(startx <= nowx + namex + 7 && nowx + namex + 7 <= startx + zwidth) ||
							(nowx - 5 <= startx && startx <= nowx + namex + 7)) && 
						 ((starty <= nowy - namey - 5 && nowy - namey - 5 <= starty + zheight) ||
						 	(starty <= nowy + 5 && nowy + 5 <= starty + zheight) ||
						 	(nowy - namey - 5 <= starty && starty <= nowy + 5))) {
								g.drawOval(nowx - 5, nowy - 5, 10, 10);
								g.drawString(now.devname, nowx + 7, nowy - 7);	//put some edge conditions in ...
					}
				}
				else {
					g.setColor(Color.green);
					g.setFont(f);
					
					int nowx = now.potentialx * im.imagesize;
					nowx /= im.width;
					int nowy = now.potentialy * im.imagesize;
					nowy /= im.height;
										
					int namex = fm.stringWidth(now.devname);
					int namey = fm.getHeight();

					if(((startx <= nowx - 5 && nowx - 5 <= startx + zwidth) || 
							(startx <= nowx + namex + 7 && nowx + namex + 7 <= startx + zwidth) ||
							(nowx - 5 <= startx && startx <= nowx + namex + 7)) && 
						 ((starty <= nowy - namey - 5 && nowy - namey - 5 <= starty + zheight) ||
						 	(starty <= nowy + 5 && nowy + 5 <= starty + zheight) ||
						 	(nowy - namey - 5 <= starty && starty <= nowy + 5))) {
								g.drawRect(nowx - 5, nowy - 5, 10, 10);
								g.drawString(now.devname, nowx + 7, nowy - 7);	//put some edge conditions in ...
					}
				}
			}
		}	
*/

	}

	public void paint(Graphics g) {
		mm = createImage(mainFrame.imagesize, mainFrame.imagesize);
		Graphics ga = mm.getGraphics();

		//draw image
		
		if(im instanceof importImage) 
			ga.drawImage(im.eatImage(), 0, 0, null);
		
		//draw mask
		
		if(imagemask instanceof mask) 
			ga.drawImage(imagemask.display(), 0, 0, null);
		
		//draw smask
		
		if(simagemask instanceof smask) 
			ga.drawImage(simagemask.display(), 0, 0, null);
		
		//draw targets
		
		if(targets instanceof pointtargets) {
			for(int i=0; i < targets.visibletargets.size(); i++) {
				pt now = (pt) targets.visibletargets.elementAt(i);
				if(now.actualized) {
					ga.setColor(Color.red);
					ga.setFont(new Font("SansSerif", Font.BOLD, 12));
					
					int nowx = now.actualx * im.imagesize;
					nowx /= im.width;
					int nowy = now.actualy * im.imagesize;
					nowy /= im.height;

					ga.drawOval(nowx - 5, nowy - 5, 10, 10);
					ga.drawString(now.devname, nowx + 7, nowy - 7);	//put some edge conditions in ...
				}
				else {
					ga.setColor(Color.green);
					ga.setFont(new Font("SansSerif", Font.BOLD, 12));
					
					int nowx = now.potentialx * im.imagesize;
					nowx /= im.width;
					int nowy = now.potentialy * im.imagesize;
					nowy /= im.height;
					
					ga.drawRect(nowx - 5, nowy - 5, 10, 10);
					ga.drawString(now.devname, nowx + 7, nowy - 7);	//put some edge conditions in ...
				}
			}
		}
		
		if(mm != null)
			g.drawImage(mm, 0, 0, null);
		
		//make sure the glowing box is there.
		
		if(im instanceof importImage && mainFrame.zf instanceof zoomFrame && mainFrame.zf.isShowing()) {
			clearZoom();
			updateZoom(	mainFrame.zf.getPosition(), 
									mainFrame.zf.getZoomSize().width, 
									mainFrame.zf.getZoomSize().height);
		}
		
	}

	public boolean handleEvent(Event e) {
		if (e.id == Event.MOUSE_DRAG) {x = e.x; y =  e.y;} 
		if (e.id == Event.MOUSE_DOWN) {x = e.x; y =  e.y;} 
		return super.handleEvent(e);
	}

	public void printit(Frame parent, Properties printprefs) {		//wait for java1.2 . . . 
		PrintJob job = Toolkit.getDefaultToolkit().getPrintJob(parent, "print image", printprefs);
		if(job == null) return;
		Graphics gp = job.getGraphics();
		
		gp.translate(100, 100);
		
		Dimension size = this.getSize();
		gp.drawRect(-1, -1, size.width+1, size.height+1);
		
		gp.setClip(0, 0, size.width, size.height);
		
		this.printAll(gp);
		

		gp.drawImage(im.eatImage(), 0, 0, null);

		System.out.println(job.getPageResolution() + " " + job.getPageDimension().width + " " + job.getPageDimension().height);

		gp.dispose();
		job.end();	
	}
}

//ZOOM FRAME

class zoomFrame extends Frame {
	mask imagemask;
	pvs mainFrame;
	imageCanvas mainCanvas;
	zoomCanvas myzoomer; 
	pointtargets targets;
	Label coordinates;
	
	Menu optionMenu;
	Menu ratioMenu;
	Menu smaskMenu;
	
	MenuItem r12MenuItem;
	MenuItem r25MenuItem;
	MenuItem r50MenuItem;
	MenuItem r100MenuItem;
	MenuItem r200MenuItem;
	MenuItem r400MenuItem;
	MenuItem r800MenuItem;
	MenuItem r1600MenuItem;
	
	MenuItem enterMenuItem;
	MenuItem beginMenuItem;
	MenuItem exitMenuItem;
	MenuItem importMenuItem;
	
	CheckboxMenuItem autoMenuItem;
	CheckboxMenuItem flyMenuItem;
	CheckboxMenuItem bestMenuItem;
	
	zoomFrame(pvs mainFrame) {
	  super("zoomer");
		this.mainCanvas = mainFrame.mainCanvas;
	  this.mainFrame = mainFrame;
		int locatex = mainFrame.locatex;
		int locatey = mainFrame.locatey;
		MenuBar mb = new MenuBar();
		
		optionMenu = new Menu("Option");
		autoMenuItem = new CheckboxMenuItem("auto export");
		flyMenuItem = new CheckboxMenuItem("fly through");
		bestMenuItem = new CheckboxMenuItem("best quality");
		optionMenu.add(new MenuItem("export image..."));
		optionMenu.add(autoMenuItem);
		optionMenu.add(flyMenuItem);
		optionMenu.add(bestMenuItem);
		optionMenu.add(new MenuItem("close window"));

		autoMenuItem.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				if(!myzoomer.auto) {	//do this first, because when the file dialog goes away
															//the pvs will repaint the zoom canvas, and if auto is 
															//already on your first filename will be null.
					FileDialog f = new FileDialog(zoomFrame.this, "base export name", FileDialog.SAVE);
					if(zoomFrame.this.mainFrame.imagedir != null)
						f.setDirectory(zoomFrame.this.mainFrame.imagedir);
					f.show();
					f.dispose();
					String filename = f.getFile();
					if(filename != null) {
						if(filename.lastIndexOf(".pgm") != -1)
							filename = (filename.substring(0, filename.length() - 4));
						filename = f.getDirectory() + filename;
						myzoomer.autoname = filename;
						myzoomer.autonumber = 0;
					}
				}
				myzoomer.auto = !myzoomer.auto;
			}
		});
		
		flyMenuItem.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				myzoomer.fly = !myzoomer.fly; 
				smaskMenuEnabler(!myzoomer.fly);
			}
		});
		
		bestMenuItem.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				myzoomer.best = !myzoomer.best; 
				myzoomer.repaint();
			}
		});
		
		ratioMenu = new Menu("zoom ratio");
		r12MenuItem = ratioMenu.add(new MenuItem("12.5%"));
		r25MenuItem = ratioMenu.add(new MenuItem("25%"));
		r50MenuItem = ratioMenu.add(new MenuItem("50%"));
		r100MenuItem = ratioMenu.add(new MenuItem("100%"));
		r200MenuItem = ratioMenu.add(new MenuItem("200%"));
		r400MenuItem = ratioMenu.add(new MenuItem("400%"));
		r800MenuItem = ratioMenu.add(new MenuItem("800%"));
		r1600MenuItem = ratioMenu.add(new MenuItem("1600%"));
		
		smaskMenu = new Menu("smask");
		enterMenuItem = smaskMenu.add(new MenuItem("enter smask mode", new MenuShortcut(KeyEvent.VK_S)));
		beginMenuItem = smaskMenu.add(new MenuItem("begin smasking"));
		exitMenuItem = smaskMenu.add(new MenuItem("exit smask mode"));
		smaskMenu.addSeparator();
		importMenuItem = smaskMenu.add(new MenuItem("import smask"));
		
		mb.add(optionMenu);
		mb.add(ratioMenu);
		mb.add(smaskMenu);
		setMenuBar(mb);
		
		myzoomer = new zoomCanvas(this);
		add("Center", myzoomer);
		
		coordinates = new Label("zoom window information bar");
		coordinates.setBackground(Color.lightGray);
		add("South", coordinates);

		smaskMenuEnabler(true);
		smaskFunctionsMenuEnabler(false);

	  setSize(260, 260);
		if(locatex > 0 && locatey > 0)	{
			if(locatex + mainFrame.getSize().width + getSize().width < getToolkit().getScreenSize().width)
				setLocation(locatex + mainFrame.getSize().width, locatey + 60);
			else
				setLocation(locatex + 3, locatey + 41);
		}
		
	  show();
	}

	public void ratioMenuEnabler(boolean b) {
		r12MenuItem.setEnabled(b);
		r25MenuItem.setEnabled(b);
		r50MenuItem.setEnabled(b);
		r100MenuItem.setEnabled(b);
		r200MenuItem.setEnabled(b);
		r400MenuItem.setEnabled(b);
		r800MenuItem.setEnabled(b);
		r1600MenuItem.setEnabled(b);
	}

	public void smaskMenuEnabler(boolean b) {
		enterMenuItem.setEnabled(b);
		importMenuItem.setEnabled(b);

		if(!b) {
			smaskFunctionsMenuEnabler(b);
		}
		if(b && (myzoomer.simagemask != null)) {
			smaskFunctionsMenuEnabler(b);
		}

	}

	public void smaskFunctionsMenuEnabler(boolean b) {
		beginMenuItem.setEnabled(b);
		exitMenuItem.setEnabled(b);
	}

	public Dimension getPosition() {	//have mainCanvas call this to get the coords back.
		int x = myzoomer.realx * myzoomer.im.imagesize;
		int y = myzoomer.realy * myzoomer.im.imagesize;
		x /= myzoomer.im.width;
		y /= myzoomer.im.height;
		return new Dimension(x, y);
	}

	public Dimension getZoomSize() {	//have mainCanvas call this to get the size for the box.
		int width  = myzoomer.getSize().width * myzoomer.im.imagesize;
		int height = myzoomer.getSize().height * myzoomer.im.imagesize;
		width  /= myzoomer.im.width;
		height /= myzoomer.im.height;
		width  /= myzoomer.ratio;
		height /= myzoomer.ratio;
		return new Dimension(width, height);
	}

	public void updateMask (mask imagemask) {
		this.imagemask = imagemask;
		myzoomer.updateMask(imagemask);
	}
	
	public void updateMask (mask imagemask, int pensize) {
		this.imagemask = imagemask;
//		myzoomer.updateMask(imagemask, pensize);     //for speed, but using the 'wait to refresh' right now.
	}
	
	public void updateTargets (pointtargets targets) {
		this.targets = targets;
		myzoomer.updateTargets(targets);
	}
	
	public void zoomImage(int x, int y, importImage im) {
		if(this.isShowing()) {
			int realx = x * im.width;			//use realx and realy from here on out.
			int realy = y * im.height;		
			realx /= im.imagesize;
			realy /= im.imagesize;
			myzoomer.zoomImage(realx, realy, im, imagemask);
		}
	}
	
	public boolean action(Event evt, Object obj) {
		String label = (String)obj;
		if (evt.target instanceof MenuItem) {
		
			//OPTIONS
			
			if (label.equals("export image...")) {
				FileDialog f = new FileDialog(this, "export image as...", FileDialog.SAVE);
				if(mainFrame.imagedir != null)
					f.setDirectory(mainFrame.imagedir);
				f.show();
				f.dispose();
				String filename = f.getFile();
				if(filename != null) {
					if(filename.lastIndexOf(".pgm") == -1)
						filename += ".pgm";
					filename = f.getDirectory() + filename;
					myzoomer.im.exportImage(filename, myzoomer.km);
				}
				return true;
			}
			if (label.equals("close")) {
				myzoomer.unsmasking();
				mainCanvas.clearSmask();
				this.setResizable(true);
				mainFrame.maskMenuEnabler(true);
				mainFrame.targetMenuEnabler(true);
				flyMenuItem.setState(false);
				smaskFunctionsMenuEnabler(false);
				myzoomer.fly = false;
				dispose();	//switched to dispose because of the magic minimizing window... 
//			hide();			//but it might not be the best solution.
				return true;
			}
			
			//RATIO
			
			if (label.equals("12.5%")) {myzoomer.newRatio(.125);}
			if (label.equals("25%")) {myzoomer.newRatio(.25);}
			if (label.equals("50%")) {myzoomer.newRatio(.5);}
			if (label.equals("100%")) {myzoomer.newRatio(1);}
			if (label.equals("200%")) {myzoomer.newRatio(2);}
			if (label.equals("400%")) {myzoomer.newRatio(4);}
			if (label.equals("800%")) {myzoomer.newRatio(8);}
			if (label.equals("1600%")) {myzoomer.newRatio(16);}
			
			//SMASKING

			if(myzoomer.im != null) {
				if (label.equals("enter smask mode")) {			//enter smask mode & start drawing
					if(myzoomer.simagemask == null) {
						FileDialog f = new FileDialog(this, "new smask filename", FileDialog.SAVE);
						if(mainFrame.maskdir != null) 
							f.setDirectory(mainFrame.maskdir);
						f.show();
						f.dispose();
						String filename = f.getFile();
						
						if(filename != null) {
							this.setResizable(false);
							flyMenuItem.setEnabled(false);
							smaskFunctionsMenuEnabler(true);
							mainFrame.maskMenuEnabler(false);
							mainFrame.targetMenuEnabler(false);
							filename = f.getDirectory() + filename;
							myzoomer.simagemask = new smask(filename, mainCanvas.im.imagesize, myzoomer.im.width, myzoomer.im.height);
							myzoomer.smasking();
							return true;
						}
					}
					else {
						myzoomer.smasking();
					}
				}
				if (label.equals("begin smasking")) {    //start drawing
					if(myzoomer.im != null) {
						if(myzoomer.simagemask != null)
							myzoomer.smasking();
						return true;
					}
				}			
				if (label.equals("exit smask mode")) {    //exit mask mode & destroy mask
					mainCanvas.clearSmask();
					myzoomer.unsmasking();
					smaskFunctionsMenuEnabler(false);
					mainFrame.maskMenuEnabler(true);
					mainFrame.targetMenuEnabler(true);
					return true;
				}
				if (label.equals("import smask")) {    //import smask from bitmap
					FileDialog f = new FileDialog(this, "import mask", FileDialog.LOAD);
					if(mainFrame.maskdir != null) 
						f.setDirectory(mainFrame.maskdir);
					f.show();
					f.dispose();
					String filename = f.getFile();
					
					if(filename != null) {
						smaskFunctionsMenuEnabler(true);
						mainFrame.maskMenuEnabler(false);
						mainFrame.targetMenuEnabler(false);
						filename = f.getDirectory() + filename;
						myzoomer.simagemask = new smask(filename, mainCanvas.im.imagesize, myzoomer.im.width, myzoomer.im.height, true);

						mainCanvas.updateSmask(myzoomer.simagemask);
						myzoomer.repaint();
						return true;
					}
					return false;
				}
			}
		}
		return false;
	}
	
	public boolean handleEvent(Event evt) {
		if (evt.id == Event.WINDOW_DESTROY) {
			myzoomer.unsmasking();
			mainCanvas.clearSmask();
			mainFrame.maskMenuEnabler(true);
			mainFrame.targetMenuEnabler(true);
			flyMenuItem.setState(false);
			smaskFunctionsMenuEnabler(false);
			myzoomer.fly = false;
			dispose();	//switched to dispose because of the magic minimizing window... 
//			hide();		//but it might not be the best solution.
			return true;
		}
		return super.handleEvent(evt);
	}
}

//ZOOM CANVAS

class zoomCanvas extends Canvas implements Runnable {
	importImage im;
	zoomFrame myFrame;
	smask simagemask;
	mask imagemask;
	pointtargets targets;
	Thread flyThread;
	String autoname;
	Dimension autosize;
	Image km;
	boolean nowsmasking;
	boolean targetting;
	boolean suspend;
	boolean memFLAG;
	boolean bright = true;
	boolean autobest;
	boolean auto;
	boolean best;
	boolean fly;
	double ratio = 1;
	double fracx = 0;
	double fracy = 0;
	double autoratio;
	int pensize = 0;
	int autonumber = 0;
	int autorealx = 0;
	int autorealy = 0;
	int memtarx = -1;	//the old target position
	int memtary = -1; 
	int targetx = 0;	//position of new target.
	int targety = 0;
	int startx = 0;	//position of MOUSE_DOWN event in smasking mode
	int starty = 0;
	int mousex = 0;	//current position of the mouse in flythrough mode;
	int mousey = 0;
	int range = 30;
	int flyx;
	int flyy;
	int memx = 0;	//last zoom position on main window, to keep from repainting multiple times.
	int memy = 0;
	int oldx = 0;		//need to change all this too...
	int oldy = 0;
	int realx = 0;
	int realy = 0;
//	int x = 0;
//	int y = 0;

	zoomCanvas(zoomFrame myFrame) {
		this.myFrame = myFrame;
		flyThread = new Thread(this);
	}
	
	public void zoomImage (int realx, int realy, importImage im, mask imagemask) {	
		this.imagemask = imagemask;
		this.im = im;
		
		int zwidth = (int) (this.getSize().width/(2*ratio));
		if(realx < zwidth) {realx = zwidth;}		
		if(realx > im.width - zwidth) {realx = im.width - zwidth;}
		this.realx = realx;
		
		int zheight = (int) (this.getSize().height/(2*ratio));
		if(realy < zheight) {realy = zheight;}
		if(realy > im.height - zheight) {realy = im.height - zheight;}
		this.realy = realy;
		
		repaint();
	}

	public void newRatio (double ratio) {
		this.ratio = ratio;
		repaint();
	}

	public void update(Graphics g) {
		paint(g);
	}

	public void smasking() {
		nowsmasking = true;
		setForeground(Color.blue);
		myFrame.setResizable(false);
		myFrame.ratioMenuEnabler(false);
		myFrame.flyMenuItem.setEnabled(false);
		setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
		simagemask.startSmask(getSize().width, getSize().height);
	}
	
	public void unsmasking() {
		simagemask = null;
		nowsmasking = false;
		myFrame.setResizable(true);
		myFrame.ratioMenuEnabler(true);
		myFrame.flyMenuItem.setEnabled(true);
		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		repaint();
	}
	
	public void updateMask(mask imagemask) {
		this.imagemask = imagemask;
		repaint();
	}
	
	public void updateTargets (pointtargets targets) {
		if(targets != null)
			targetting = true;
		else
			targetting = false;
			
		this.targets = targets;
		repaint();
	}
	
	public void paint(Graphics g) {
		km = createImage(getSize().width, getSize().height);
		Graphics ga = km.getGraphics();
		
		
		//error correction code...probably shouldn't go here, but this is the only spot right now.
		if(realx < getSize().width/(2*ratio))  {realx = (int) (getSize().width/(2*ratio));}
		if(realy < getSize().height/(2*ratio)) {realy = (int) (getSize().height/(2*ratio));}
		if(realx > im.width  - getSize().width/(2*ratio)) {realx = im.width  - (int) (getSize().width/(2*ratio));}
		if(realy > im.height - getSize().height/(2*ratio)) {realy = im.height - (int) (getSize().height/(2*ratio));}

		//draw zoomed image
		
		if(im != null) {
			if(best) {
				if(ratio <= 1)	{
					AreaAveragingScaleFilter asf = new AreaAveragingScaleFilter(getSize().width, getSize().height);
					Image sz = createImage(new FilteredImageSource(im.eatImage(realx, realy, (int) (getSize().width/ratio),
																(int) (getSize().height/ratio)).getSource(), asf));
					ga.drawImage(sz, 0, 0, null);
				}
				else {
					if(ratio <= 3) {
						AreaAveragingScaleFilter asf = new AreaAveragingScaleFilter(getSize().width, getSize().height);
						Image sz = createImage(new FilteredImageSource(im.eatImage(realx, realy, (int) (getSize().width/ratio),
																	(int) (getSize().height/ratio)).getSource(), asf));
						Image bz = createImage(new FilteredImageSource(sz.getSource(), new BlurFilter()));
						ga.drawImage(bz, 0, 0, null);
					}
					else {
						Image bz = im.eatImage(realx, realy, (int) (getSize().width/ratio), (int) (getSize().height/ratio));
						for(double spazio = ratio; spazio > 1; spazio /= 3) {
							bz = createImage(new FilteredImageSource(bz.getSource(), new ReplicateScaleFilter(bz.getWidth(null)*3, bz.getHeight(null)*3)));
							bz = createImage(new FilteredImageSource(bz.getSource(), new BlurFilter()));
						}
						bz = createImage(new FilteredImageSource(bz.getSource(), new AreaAveragingScaleFilter(getSize().width, getSize().height)));
						bz = createImage(new FilteredImageSource(bz.getSource(), new BlurFilter()));
						ga.drawImage(bz, 0, 0, null);
					}
				}
			}
			else {
				ReplicateScaleFilter rsf = new ReplicateScaleFilter(getSize().width, getSize().height);
				ga.drawImage(createImage(new FilteredImageSource(im.eatImage(realx, realy, (int) (getSize().width/ratio), 
																(int) (getSize().height/ratio)).getSource(), rsf)), 0, 0, null); 
			}
		}
		
		//draw zoomed mask

		if(imagemask != null)
			ga.drawImage(imagemask.display(realx, realy, (int) (getSize().width/ratio), (int) (getSize().height/ratio)),  
									0, 0, getSize().width, getSize().height, null);
		
		//draw zoomed smask
		
		if(simagemask != null)
			ga.drawImage(simagemask.display(realx, realy, (int) (getSize().width/ratio), (int) (getSize().height/ratio)),  
									0, 0, getSize().width, getSize().height, null);
	
		//draw zoomed targets
		
		if(targets != null) {
			int sizex;
			int sizey;
			sizex = (int) (getSize().width/ratio);
			sizey = (int) (getSize().height/ratio);
			
			Vector zoomtargets = targets.zoompoints(realx - sizex/2, realy - sizey/2, sizex, sizey);
			
			for(int i=0; i < zoomtargets.size(); i++) {
				pt now = (pt) zoomtargets.elementAt(i);
				if(now.actualized) {
					ga.setColor(Color.red);
					ga.setFont(new Font("SansSerif", Font.BOLD, 12));
					
					int nowx;
					int nowy;
					
					if(ratio < 90) {
						nowx = (int) ((now.actualx - realx)*ratio);	//relative position
						nowy = (int) ((now.actualy - realy)*ratio);
					}
					else {
						nowx = (int) ((now.actualx - realx)*(getSize().width/sizex));	//relative position
						nowy = (int) ((now.actualy - realy)*(getSize().height/sizey));
					}

					if(sizex/2 == sizex/2.0)		//keeps it centered even when realx is off.
						nowx += getSize().width/(sizex*2);
					if(sizey/2 == sizey/2.0) 
						nowy += getSize().height/(sizey*2);

					nowx += getSize().width/2;	//center it
					nowy += getSize().height/2;					
		
					ga.drawOval(nowx - 5, nowy - 5, 10, 10);
					ga.drawString(now.devname, nowx + 7, nowy - 7);	//put some edge conditions in ...
				}
				else {
					ga.setColor(Color.green);
					ga.setFont(new Font("SansSerif", Font.BOLD, 12));
					
					int nowx;
					int nowy;
					
					if(ratio < 90) {
						nowx = (int) ((now.potentialx - realx)*ratio);	//relative position
						nowy = (int) ((now.potentialy - realy)*ratio);
					}
					else {
						nowx = (int) ((now.potentialx - realx)*(getSize().width/sizex));	//relative position
						nowy = (int) ((now.potentialy - realy)*(getSize().height/sizey));
					}

					if(sizex/2 == sizex/2.0)		//keeps it centered even when realx is off.
						nowx += getSize().width/(sizex*2);
					if(sizey/2 == sizey/2.0) 
						nowy += getSize().height/(sizey*2);

					nowx += getSize().width/2;	//center it
					nowy += getSize().height/2;					
					
					ga.drawRect(nowx - 5, nowy - 5, 10, 10);
					ga.drawString(now.devname, nowx + 7, nowy - 7);	//put some edge conditions in ...
				}
			}
		}
		
		g.drawImage(km, 0, 0, null);
		
		//auto export
		
		if(auto) {
			if(getSize() != autosize || realx != autorealx || realy != autorealy || autoratio != ratio || best != autobest)
				im.exportImage(autoname + Integer.toString(++autonumber) + ".pgm", km);
System.out.print("");	//please don't ask me why this is here. 
											//it just needs to be. take my word for it.
			
			autosize = getSize();		//we remember things so we don't have to do them twice.
			autorealx = realx;
			autorealy = realy;
			autoratio = ratio;
			autobest = best;
		}
		
		//make the zoomie box glow.

		myFrame.mainCanvas.clearZoom();
		myFrame.mainCanvas.updateZoom(myFrame.getPosition(), 
																	myFrame.getZoomSize().width, 
																	myFrame.getZoomSize().height);		
	}

	//this is for mouse control flying around.
	//because if you don't it only flies when
	//you are actually *moving* the mouse, 
	//which is kind of uncool.

	public void start_animation() {
		if(flyThread.isAlive()) 
			flyThread.resume();
		else 
			flyThread.start();
	}
	
	public void pause_animation() {
		suspend = true;
	}
	
	public void run() {
		for(;;) {
			int foolx = 0;
			int fooly = 0;
			
			fracx += (double)(mousex - flyx)/(ratio*10);
			fracy += (double)(mousey - flyy)/(ratio*10);
			
			if(fracx > 1 || fracx < -1) {
				foolx = (int) fracx;
				fracx = fracx - foolx;
			}
			
			if(fracy > 1 || fracy < -1) {
				fooly = (int) fracy;
				fracy = fracy - fooly;
			}
			
			realx += foolx;
			realy += fooly;
				
			Graphics g = getGraphics();	
			paint(g);	//if you call repaint() it queues, and bad things happen.

			if(suspend) {
				suspend = false;
				flyThread.suspend();
			}

			try {Thread.sleep(10);}	//ten is sort of arbitrary...
			catch (InterruptedException e) {System.out.print("gogogo");}
		}
	}

	public boolean handleEvent(Event evt) {
		
		//flying
		
		if(fly) {
			if(!targetting)
				myFrame.coordinates.setText("ratio: " + ratio);
System.out.print("");  //same as above.
			
			if (evt.id == Event.KEY_PRESS) {	//should put some better code here.
				if ((char) evt.key == 'z') {
					ratio *= .98;	
					repaint();
					return true;
				}
				if ((char) evt.key == 'x') {
					ratio *= 100;
					ratio /= 98;
					repaint();
					return true;
				} 
			} 

			if (evt.id == Event.KEY_ACTION) {
				if (evt.key == Event.LEFT) {	//left arrow
					realx--;
					repaint();
					return true;
				}
				if (evt.key == Event.UP) {	//up arrow
					realy--;
					repaint();
					return true;
				}
				if (evt.key == Event.RIGHT) {	//right arrow
					realx++;
					repaint();
					return true;
				}
				if (evt.key == Event.DOWN) {	//down arrow
					realy++;
					repaint();
					return true;
				}
			}
			
			if (evt.id == Event.MOUSE_DOWN) {
				mousex = evt.x;
				mousey = evt.y;
				flyx = evt.x;
				flyy = evt.y;
				start_animation();
				
				if(!targetting)
					return true;
			}
			
			if (evt.id == Event.MOUSE_DRAG) {	
				mousex = evt.x;
				mousey = evt.y;
				return true;
			}

			if (evt.id == Event.MOUSE_UP) {	
				fracx = 0;
				fracy = 0;
				pause_animation();
				return true;
			}
		}
		
		//smasking
		
		if(nowsmasking) {
			myFrame.coordinates.setText("smasking");

			if (evt.id == Event.MOUSE_DOWN) {
				simagemask.startSmask((int) (getSize().width/ratio) + 1, (int) (getSize().height/ratio) + 1);
				startx = evt.x; starty = evt.y;
				oldx = evt.x; oldy = evt.y;
				return true;
			}

			if (evt.id == Event.MOUSE_DRAG) {			
				if(evt.x >= simagemask.zwidth*ratio)
					evt.x = (int) ((simagemask.zwidth-1)*ratio);
				if(evt.y >= simagemask.zheight*ratio)
					evt.y = (int) ((simagemask.zheight-1)*ratio);
				if(evt.y < 0)
					evt.y = 0;
				if(evt.x < 0)
					evt.x = 0;

				if(oldy != evt.y) {
					if(memFLAG == true) {
						simagemask.addLine((int) (memx/ratio), (int) (memy/ratio), (int) (oldx/ratio), (int) (oldy/ratio));
						memFLAG = false;
					}
					simagemask.addLine((int) (oldx/ratio), (int) (oldy/ratio), (int) (evt.x/ratio), (int) (evt.y/ratio));
				}
				else if(memFLAG == false) {
					memx = evt.x;
					memy = evt.y;
					memFLAG = true;
				}	
				addLine(evt.x, evt.y);
				oldx = evt.x; oldy = evt.y;
				return true;
			}

			if (evt.id == Event.MOUSE_UP) {
				if(evt.x >= simagemask.zwidth*ratio)
					evt.x = (int) ((simagemask.zwidth-1)*ratio);
				if(evt.y >= simagemask.zheight*ratio)
					evt.y = (int) ((simagemask.zheight-1)*ratio);
				if(evt.y < 0)
					evt.y = 0;
				if(evt.x < 0)
					evt.x = 0;

				if(memFLAG == true) {
					simagemask.addLine((int) (memx/ratio), (int) (memy/ratio), (int) (oldx/ratio), (int) (oldy/ratio));
				}
				simagemask.addLine((int) (evt.x/ratio), (int) (evt.y/ratio), (int) (startx/ratio), (int) (starty/ratio));
				oldx = evt.x; oldy = evt.y;
				addLine(startx, starty);
//				simagemask.endSmask(x, y);
				simagemask.endSmask(realx, realy);
				nowsmasking = false;
				memFLAG = false;
				myFrame.setResizable(true);
				myFrame.ratioMenuEnabler(true);
				myFrame.flyMenuItem.setEnabled(true);
				myFrame.mainCanvas.updateSmask(simagemask);
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				repaint();
				return true;
			}
		}
		
		//bright
		
		if(targetting && bright) {
			if (evt.id == Event.MOUSE_UP) {
				memtarx = -1;
				memtary = -1;
				repaint();
			}		
			if (evt.id == Event.MOUSE_DOWN || evt.id == Event.MOUSE_DRAG) {
//				Dimension d = im.brightest((int) (evt.x/ratio), (int) (evt.y/ratio), x, y, range);
				Dimension d = im.brightest((int) (evt.x/ratio), (int) (evt.y/ratio), realx, realy, range);

				if(d == null) {return false;}	//just in case...
				int evtx = (int) (d.width  * getSize().width /(int)(getSize().width /ratio));
				int evty = (int) (d.height * getSize().height/(int)(getSize().height/ratio));

				targetx = realx;		
				targety = realy;
				targetx -= (int) (getSize().width/(2*ratio));	
				targety -= (int) (getSize().height/(2*ratio));
				targetx += (d.width);
				targety += (d.height);
				
				if((memtarx != targetx || memtary != targety) && (memtarx != -1 || memtary != -1))
					repaint();
				
				Graphics g = this.getGraphics();
				g.setColor(Color.yellow);
				int xratio = getSize().width /(int)(getSize().width /ratio);
				int yratio = getSize().height/(int)(getSize().height/ratio);
				if(xratio < 2) {xratio = 2;}
				if(yratio < 2) {yratio = 2;}
				g.fillRect(evtx, evty, xratio, yratio);
				
				myFrame.coordinates.setText("x: " + targetx + " " + "y: " + targety + " " + "ratio: " + ratio);
				
				return true;
			}
		}
		
		//not flying and not smasking and not bright...
		
		if (evt.id == Event.MOUSE_DOWN || evt.id == Event.MOUSE_DRAG) {
			targetx = realx;		
			targety = realy;
			targetx -= (int) (getSize().width/(2*ratio));		//to put x in the middle of the zwidth*zheight block.
			targety -= (int) (getSize().height/(2*ratio));
			targetx += (int) (evt.x/ratio);
			targety += (int) (evt.y/ratio);
			
			myFrame.coordinates.setText("x: " + targetx + " " + "y: " + targety + " " + "ratio: " + ratio);
			
			return true;
		}
		
		return super.handleEvent(evt);
	}	
	
	public void addLine(int x, int y) {
		Graphics g = this.getGraphics();
		g.drawLine(oldx, oldy, x, y);
	}
}

//IMAGE

class importImage  {
	Image om;
	Image pm;
	Image im;
	Image zm;
	int memx = 0;		//stores realx
	int memy = 0; 	//stores realy
	int width = 0;
	int height = 0;
	int zwidth;
	int zheight;
	int imagesize;
	int imagequality;
	int asfpadding = 192;
	int contrastlow = 0;
	int contrasthigh = 55;
	int defaultcontrastlow = 0;
	int defaultcontrasthigh = 55;
	long fullhigh;  
	long fulllow;
	pvs mainFrame;
	String filename;
	String metafilename;
	RandomAccessFile raf;
	boolean newcontrast;
	boolean rafFLAG = false;
	byte[] greyarray = new byte[256];
	int[] averagearray = new int[256];
	byte[] indexarray;
	IndexColorModel cm; 
	
	//sends the file to the right reader based on it's name.	
	importImage (pvs mainFrame, String filename, int imagesize, int imagequality) { 
		this.mainFrame = mainFrame;
		this.imagesize = imagesize;
		this.imagequality = imagequality;

		//grab the right files

		if(filename.endsWith("metadata")) {
			metafilename = filename;
			filename = filename.substring(0,filename.length() - 8) + "data1";
			this.filename = filename;	 
			this.metafilename = metafilename;
			readpvsImage();	 
		}
		else if(filename.endsWith("data1")) {
			metafilename = filename.substring(0,filename.length() - 5) + "metadata";
			this.filename = filename;	 
			this.metafilename = metafilename;
			readpvsImage();	 
		}
		else if(filename.endsWith("D")) {
			metafilename = filename.substring(0,filename.length() - 1) + "L";
//			metafilename = filename.substring(0,filename.length() - 1) + "converted.metadata";
			this.filename = filename;	 
			this.metafilename = metafilename;
			readasfImage();	 
		}
		else if(filename.endsWith("L")) {
			metafilename = filename;
//			metafilename = filename.substring(0,filename.length() - 1) + "converted.metadata";  
			filename = filename.substring(0,filename.length() - 1) + "D";
			this.filename = filename;	 
			this.metafilename = metafilename;
			readasfImage();	 
		}
		else {  //put a dialog here, or something...
			logger.log("improper filename format.");
			return;
		}
	}
	
	//reads pvs image files.
	public void readpvsImage() {
		int array[];
		long high = 0;
		long low = 2147483647;

		//parse out metadata
		
		try {
			FileReader fr = new FileReader(metafilename);
			LineNumberReader in = new LineNumberReader(fr);

			boolean numpixels = false;
			while(!numpixels) {
				StringTokenizer s = new StringTokenizer(in.readLine());
				while(s.hasMoreTokens())
					if(s.nextToken().equalsIgnoreCase("number_of_pixels")) {
						String equalsign = s.nextToken();  //this takes care of the equals sign.
						String quotednumber = s.nextToken();
						String number = quotednumber.substring(1,quotednumber.length()-1);
						width = Integer.parseInt(number);
						numpixels = true;
						break;
					}
			}

			boolean numlines = false;
			while(!numlines) {
				StringTokenizer s = new StringTokenizer(in.readLine());
				while(s.hasMoreTokens())
					if(s.nextToken().equalsIgnoreCase("number_of_lines")) {
						String equalsign = s.nextToken();  //here's that equals sign again...
						String quotednumber = s.nextToken();
						String number = quotednumber.substring(1,quotednumber.length()-1);
						height = Integer.parseInt(number);
						numlines = true;
						break;
					}
			}
			fr.close();
		}
		catch (IOException e){
			logger.log("error parsing the metadata.");
		}
		
		//now we've got what we needed.

		if(imagequality >= height/imagesize)
			imagequality = 0;
		double fwidth = width;
		double fheight = height;
		double wratio = fwidth/imagesize;   
		double hratio = fheight/imagesize;
		long[] longarray = new long[imagesize];
		int[] countarray = new int[imagesize];
		int oldi = 0;
			
		//process meter stuff
		procDialog pd = new procDialog(mainFrame, "opening image...");
		int locatex = mainFrame.getLocation().x;	//get from above...	
		int locatey = mainFrame.getLocation().y;
		if(locatex > 0 && locatey > 0)
			pd.setLocation(locatex + 3, locatey + 41);	
		pd.show();
		int done = 0;
			
		try {
			FileInputStream fis = new FileInputStream(filename);
			BufferedInputStream bis = new BufferedInputStream(fis);
			DataInputStream from = new DataInputStream(bis);
			
			array = new int[imagesize*imagesize];

			if(imagequality == 0) {
				byte[] section = new byte[width*4];
				for(int i=0; i < height; i++) {
					//process meter stuff
					if((i*100)/height > done) {
						done = (i*100)/height;
						pd.process(done);
					}
					
					if((int) (i/hratio) > oldi || i == height-1) {
						for(int j=0; j < imagesize; j++) {
							array[oldi*imagesize+j] = (int) (longarray[j]/countarray[j]);
							if(high * countarray[j] < longarray[j]) {high = longarray[j]/countarray[j];}
							if(low  * countarray[j] > longarray[j]) {low  = longarray[j]/countarray[j];}
						}
						longarray = new long[imagesize];
						countarray = new int[imagesize];
					}
					
					from.read(section);
					for(int j=0; j < width; j++) {
						countarray[(int) (j/wratio)]++;
							int part1 = section[4*j];
							int part2 =	section[4*j+1];
							int part3 = section[4*j+2];
							int part4 =	section[4*j+3];
							if(part1 < 0)	{part1 += 256;}
							if(part2 < 0)	{part2 += 256;}
							if(part3 < 0)	{part3 += 256;}
							if(part4 < 0)	{part4 += 256;}
							longarray[(int) (j/wratio)] +=	part1*16777216 + 
																							part2*65536 +
																							part3*256 +
																							part4;
					}

					oldi = (int) (i/hratio);
				}
			}
			else {
				byte[] section = new byte[width*4];
				for(int i=0; i < height; i++) {
					//process meter stuff
					if((i*100)/height > done) {
						done = (i*100)/height;
						pd.process(done);
					}
				
					if((int) (i/hratio) > oldi || i == height-1) {
						for(int j=0; j < imagesize; j++) {
							array[oldi*imagesize+j] = (int) (longarray[j]/countarray[j]);
							if(high * countarray[j] < longarray[j]) {high = longarray[j]/countarray[j];}
							if(low  * countarray[j] > longarray[j]) {low  = longarray[j]/countarray[j];}
						}
						longarray = new long[imagesize];
						countarray = new int[imagesize];
					}
					
					if((int) ((i+imagequality)/hratio) > oldi || i > height - imagequality) {
						from.read(section);		
						for(int j=0; j < width; j++) {
							countarray[(int) (j/wratio)]++;
							int part1 = section[4*j];
							int part2 =	section[4*j+1];
							int part3 = section[4*j+2];
							int part4 =	section[4*j+3];
							if(part1 < 0)	{part1 += 256;}
							if(part2 < 0)	{part2 += 256;}
							if(part3 < 0)	{part3 += 256;}
							if(part4 < 0)	{part4 += 256;}
							longarray[(int) (j/wratio)] +=	part1*16777216 + 
																							part2*65536 +
																							part3*256 +
																							part4;
						}
					}
					else
						from.skipBytes(width*4);
					
					oldi = (int) (i/hratio);
				}
			}
			
			pd.dispose();		//process meter stuff
								
			fullhigh = high;
			fulllow = low;
			from.close();
			int stddev = 0;
			int mean = 0;
			double sum = 0;
			indexarray = new byte[imagesize*imagesize];

			if(high-low != 0)		//avoid division by zero errors.
				for(int i=0; i < imagesize*imagesize; i++) {
					if(array[i] < low) {array[i] = (int) low;}
					indexarray[i] = (byte) (255*(array[i] - low)/(high-low));
					averagearray[(int) (255*(array[i] - low)/(high-low))]++;
					mean += (int) (255*(array[i] - low)/(high-low));
				}
			else
				for(int i=0; i < imagesize*imagesize; i++) {
					indexarray[i] = (byte) 0;
					averagearray[0]++;
				}
			
			mean = (int) (mean/(imagesize*imagesize));			
			for(int i=0; i < 256; i++) {
				sum += averagearray[i]*(i - mean)*(i-mean);
			}
			stddev = (int) java.lang.Math.sqrt(sum/(imagesize*imagesize));
					
			defaultcontrastlow = mean - (2*stddev);
			defaultcontrasthigh = mean + (2*stddev);

			if(defaultcontrastlow < 0)
				defaultcontrastlow = 0;
			if(defaultcontrastlow > 255)
				defaultcontrastlow = 255;
			if(defaultcontrasthigh < 0)
				defaultcontrasthigh = 0;
			if(defaultcontrasthigh > 255)
				defaultcontrasthigh = 255;

			contrastlow = defaultcontrastlow;
			contrasthigh = defaultcontrasthigh;
			
			colourarray(contrastlow, contrasthigh);
					
		}
		catch(Exception e) {
			indexarray = new byte[imagesize*imagesize];  //eck. do something nice here.
			Random ran = new Random();
			ran.nextBytes(indexarray);
			logger.log("error creating image.");
		}	
					
		im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize, 
					cm, indexarray, 0, imagesize)); 
	}
	
	//reads asf image files.
	public void readasfImage() {
		byte cwidth;
		byte cheight;
		int array[];
		long high = 0;
		long low = 2147483647;

		//parse out metadata
		
		try {
			FileInputStream metaread = new FileInputStream(metafilename);
			metaread.skip(27436);
			
			for(int i = 100000000; i >= 1; i /= 10) {
				cwidth = (byte) metaread.read();
				if(48 <= cwidth && cwidth <= 57) {
					width += i*(cwidth-48);
				}
			}
			
			for(int i = 100000000; i >= 1; i /= 10) {
				cheight = (byte) metaread.read();
				if(48 <= cheight && cheight <= 57) {
					height += i*(cheight-48);
				}
			}

			metaread.close();
		}
		catch (IOException e){
			logger.log("error parsing the metadata.");
		}
		
		//now we've got what we needed.

		if(imagequality >= height/imagesize)
			imagequality = 0;
		double fwidth = width;
		double fheight = height;
		double wratio = fwidth/imagesize;   
		double hratio = fheight/imagesize;
		long[] longarray = new long[imagesize];
		int[] countarray = new int[imagesize];
		int oldi = 0;
			
		//process meter stuff
		procDialog pd = new procDialog(mainFrame, "opening image...");
		int locatex = mainFrame.getLocation().x;	//get from above...	
		int locatey = mainFrame.getLocation().y;
		if(locatex > 0 && locatey > 0)
			pd.setLocation(locatex + 3, locatey + 41);	
		pd.show();
		int done = 0;
			
		try {
			FileInputStream fis = new FileInputStream(filename);
			BufferedInputStream bis = new BufferedInputStream(fis);
			DataInputStream from = new DataInputStream(bis);

			byte[] section = new byte[width + asfpadding];
			from.read(section);		//get rid of junk row.		

			array = new int[imagesize*imagesize];

			if(imagequality == 0) {
				for(int i=0; i < height; i++) {
					//process meter stuff
					if((i*100)/height > done) {
						done = (i*100)/height;
						pd.process(done);
					}
					
					if((int) (i/hratio) > oldi || i == height-1) {
						for(int j=0; j < imagesize; j++) {
							array[oldi*imagesize+j] = (int) (longarray[j]/countarray[j]);
							if(high * countarray[j] < longarray[j]) {high = longarray[j]/countarray[j];}
							if(low  * countarray[j] > longarray[j]) {low  = longarray[j]/countarray[j];}
						}
						longarray = new long[imagesize];
						countarray = new int[imagesize];
					}
					
					from.read(section);
					for(int j=0; j < width; j++) {
						countarray[(int) (j/wratio)]++;
						longarray[(int) (j/wratio)] += section[j + asfpadding];
					}

					oldi = (int) (i/hratio);
				}
			}
			else {
				for(int i=0; i < height; i++) {
					//process meter stuff
					if((i*100)/height > done) {
						done = (i*100)/height;
						pd.process(done);
					}
				
					if((int) (i/hratio) > oldi || i == height-1) {
						for(int j=0; j < imagesize; j++) {
							array[oldi*imagesize+j] = (int) (longarray[j]/countarray[j]);
							if(high * countarray[j] < longarray[j]) {high = longarray[j]/countarray[j];}
							if(low  * countarray[j] > longarray[j]) {low  = longarray[j]/countarray[j];}
						}
						longarray = new long[imagesize];
						countarray = new int[imagesize];
					}
					
					if((int) ((i+imagequality)/hratio) > oldi || i > height - imagequality) {
						from.read(section);		
						for(int j=0; j < width; j++) {
							countarray[(int) (j/wratio)]++;
							longarray[(int) (j/wratio)] += section[j + asfpadding];
						}
					}
					else
						from.skipBytes(width + asfpadding);
					
					oldi = (int) (i/hratio);
				}
			}
			
			pd.dispose();		//process meter stuff
								
			fullhigh = high;
			fulllow = low;
			from.close();
			int stddev = 0;
			int mean = 0;
			double sum = 0;
			indexarray = new byte[imagesize*imagesize];

			if(high-low != 0)		//avoid division by zero errors.
				for(int i=0; i < imagesize*imagesize; i++) {
					if(array[i] < low) {array[i] = (int) low;}
					indexarray[i] = (byte) (255*(array[i] - low)/(high-low));
					averagearray[(int) (255*(array[i] - low)/(high-low))]++;
					mean += (int) (255*(array[i] - low)/(high-low));
				}
			else
				for(int i=0; i < imagesize*imagesize; i++) {
					indexarray[i] = (byte) 0;
					averagearray[0]++;
				}
			
			mean = (int) (mean/(imagesize*imagesize));			
			for(int i=0; i < 256; i++) {
				sum += averagearray[i]*(i - mean)*(i-mean);
			}
			stddev = (int) java.lang.Math.sqrt(sum/(imagesize*imagesize));
					
			defaultcontrastlow = mean - (2*stddev);
			defaultcontrasthigh = mean + (2*stddev);

			if(defaultcontrastlow < 0)
				defaultcontrastlow = 0;
			if(defaultcontrastlow > 255)
				defaultcontrastlow = 255;
			if(defaultcontrasthigh < 0)
				defaultcontrasthigh = 0;
			if(defaultcontrasthigh > 255)
				defaultcontrasthigh = 255;

			contrastlow = defaultcontrastlow;
			contrasthigh = defaultcontrasthigh;
			
			colourarray(contrastlow, contrasthigh);
					
		}
		catch(Exception e) {
			indexarray = new byte[imagesize*imagesize];  //eck. do something nice here.
			Random ran = new Random();
			ran.nextBytes(indexarray);
			logger.log("error creating image.");
		}	
					
		im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize, 
					cm, indexarray, 0, imagesize)); 
	}

	public Image eatImage() {
		return im;
	}

	public Image eatImage(int x, int y, int pensize) {  //called by updateMask.
		int starter = (y-pensize)*imagesize + (x-pensize);
		int xsize = pensize*2;
		int ysize = pensize*2;
		
		//eh, some junk for boundary conditions.
		
		if(starter < 0) {starter = x - pensize;}  
		if(starter < 0) {starter = 0;}
		if(y - pensize < 0) {ysize += y - pensize;}
		if(y + pensize >= imagesize) {ysize = imagesize + pensize - y;}
		if(x - pensize < 0) {xsize += x - pensize; if(starter > 0) {starter -= x-pensize;}}
		if(x + pensize >= imagesize) {xsize = imagesize + pensize - x;}
		if((ysize > 0) && (xsize > 0))
			return Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(xsize, ysize, 
						  cm, indexarray, starter, imagesize)); 
		else   
			return Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(0, 0, 
						  cm, indexarray, starter, imagesize));   //idiot value
	}

	public Image eatImage(Dimension position, int width, int height) {  //called by updateMask.
		int x = position.width;
		int y = position.height;
		int starter = (y-height)*imagesize + (x-width);
		int xsize = width*2;
		int ysize = height*2;
		
		//eh, some junk for boundary conditions.
		
		if(starter < 0) {starter = x - width;}  
		if(starter < 0) {starter = 0;}
		if(y - height < 0) {ysize += y - height;}
		if(y + height >= imagesize) {ysize = imagesize + height - y;}
		if(x - width < 0) {xsize += x - width; if(starter > 0) {starter -= x-width;}}
		if(x + width >= imagesize) {xsize = imagesize + width - x;}
		
		Image i;
		
		if((ysize > 0) && (xsize > 0))
			i = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(xsize, ysize, 
						  cm, indexarray, starter, imagesize)); 
		else   
			i = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(0, 0, 
						  cm, indexarray, starter, imagesize));   //idiot value

		return i;
	}

	public Image eatImage(int realx, int realy, int zwidth, int zheight) {
		realx -= zwidth/2;		//to put x in the middle of the zwidth*zheight block.
		realy -= zheight/2;
//		x--;		//eck. don't know if i still need this.
//		y--;
		boolean targetting = mainFrame.zf.myzoomer.targetting;

		if((memx != realx) || (memy != realy) || newcontrast || targetting) {	//check to see if we 
																																					//really need to refresh.
			if(filename.endsWith("data1")) {
				newpvszoom(realx, realy, zwidth, zheight);	 
			}
			else if(filename.endsWith("D")) {
				newasfzoom(realx, realy, zwidth, zheight);	 
			}
		}

		newcontrast = false;
		this.zwidth = zwidth;
		this.zheight = zheight;
		
		return zm;
	}
	
	protected void newpvszoom(int realx, int realy, int zwidth, int zheight) {
		boolean firstFLAG = false;
		int first = (width*realy + realx) * 4;
		if(first < 0)  {
			firstFLAG = true;
			if(realx < 0) {first = 0;}
			else {first = realx * 4;}
		}
		if(first > 4*width*height) {first = 0;} //should do something nicer here. . . 
		int now = first;
		
		if(!rafFLAG) {
			if(filename != null) {
				try {
					raf = new RandomAccessFile(filename, "r");
					rafFLAG = true;
				}
				catch(Exception e) {
					logger.log("error opening file: " + filename);
				}	
			}
		}

		long[] zoomarray = new long[zwidth * zheight]; //zoom window image array
		byte[] indexzoomarray = new byte[zwidth * zheight];
		if(filename != null) {		
			try {
				raf.seek(first); 	//seeks in bytes...four byte integer. 
				int start = 0;			//only needs to be done once
				int end = zwidth;
				if(realx < 0) {start = 0 - realx;}
				if(realx > width - zwidth) {end = width - realx;}
				int sizer = end - start;
				int index;
				
				if(sizer > 0) {
					byte[] newline = new byte[sizer*4];
					for(int i=0; i < zheight; i++) {
						if((realy+i < 0) || (realy+i >= height)) {;}
						else {
							if(!firstFLAG) 
								raf.skipBytes(start*4);			
							raf.read(newline);
							for(int j = 0; j < sizer; j++) {
								index = i*zwidth + j + start;
														
								//could loop these . . .   ...maybe later.
								
								if(newline[j*4] < 0) 
									zoomarray[index] =	(256 + newline[j*4]) * 16777216;
								else
									zoomarray[index] =	newline[j*4] * 16777216;
								
								if(newline[j*4+1] < 0) 
									zoomarray[index] +=	(256 + newline[j*4+1]) * 65536;
								else
									zoomarray[index] +=	newline[j*4+1] * 65536;
								
								if(newline[j*4+2] < 0) 
									zoomarray[index] +=	(256 + newline[j*4+2]) * 256;
								else
									zoomarray[index] +=	newline[j*4+2] * 256;
								
								if(newline[j*4+3] < 0)
									zoomarray[index] +=	(256 + newline[j*4+3]);
								else
									zoomarray[index] +=	newline[j*4+3];
							}
							now += width*4;
							raf.seek(now);
						}
					}
					float divisor = (fullhigh-fulllow);		//test version;
					for(int i=0; i < zwidth * zheight; i++) {
						if(zoomarray[i] > fullhigh) {zoomarray[i] = fullhigh;}
						indexzoomarray[i] = (byte) ((float)(255*(zoomarray[i] - fulllow))/divisor);
					}
				}
			}
			catch(Exception e) {
				logger.log("error creating zoomed (full-res) image.");
			}
		}
		
		zm = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(zwidth, zheight,
				  cm, indexzoomarray, 0, zwidth));

		memx = realx;
		memy = realy;
	}
	
	protected void newasfzoom(int realx, int realy, int zwidth, int zheight) {
		boolean firstFLAG = false;
		int first = ((width + asfpadding)*realy + realx);
		if(first < 0)  {
			firstFLAG = true;
			if(realx < 0) {first = 0;}
			else {first = realx;}
		}
		if(first > (width + asfpadding)*height) {first = asfpadding;} //should do something nicer here. . . 
		first += (width + asfpadding);
		int now = first;
		
		if(!rafFLAG) {
			if(filename != null) {
				try {
					raf = new RandomAccessFile(filename, "r");
					rafFLAG = true;
				}
				catch(Exception e) {
					logger.log("error opening file: " + filename);
				}	
			}
		}

		long[] zoomarray = new long[zwidth * zheight]; //zoom window image array
		byte[] indexzoomarray = new byte[zwidth * zheight];
		if(filename != null) {		
			try {
				raf.seek(first); 		//seeks in bytes...four byte integer. 
				int start = 0;			//only needs to be done once
				int end = zwidth;
				if(realx < 0) {start = 0 - realx;}
				if(realx > width - zwidth) {end = width - realx;}
				int sizer = end - start;
				int index;
				
				if(sizer > 0) {
					byte[] newline = new byte[sizer];
					for(int i=0; i < zheight; i++) {
						if((realy+i < 0) || (realy+i >= height)) {;}
						else {
							if(!firstFLAG) 
								raf.skipBytes(start+asfpadding);			
							raf.read(newline);
							for(int j = 0; j < sizer; j++) {
								index = i*zwidth + j + start;
								if(newline[j] < 0) 
									zoomarray[index] =	(256 + newline[j]);
								else
									zoomarray[index] =	newline[j];
							}
							now += (width + asfpadding);
							raf.seek(now);
						}
					}
					float divisor = (fullhigh-fulllow);		//test version;
					for(int i=0; i < zwidth * zheight; i++) {
						if(zoomarray[i] > fullhigh) {zoomarray[i] = fullhigh;}
						indexzoomarray[i] = (byte) ((float)(255*(zoomarray[i] - fulllow))/divisor);
					}
				}
			}
			catch(Exception e) {
				logger.log("error creating zoomed (full-res) image.");
			}
		}
		
		zm = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(zwidth, zheight,
				  cm, indexzoomarray, 0, zwidth));

		memx = realx;
		memy = realy;
	}

	public void recolour() {
		im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize,
					cm, indexarray, 0, imagesize));
	}

	public Dimension brightest(int evtx, int evty, int realx, int realy, int range) {	//returns the brightest value within range pixels
//		boolean firstFLAG;
		long charlie = 0;
		long high = Long.MIN_VALUE;
		Dimension etarget = null;

		realx -= zwidth/2;		//to put x in the middle of the zwidth*zheight block.
		realy -= zheight/2;
//		x--;
//		y--;

		int ylow = evty - range;
		if(ylow < 0) {ylow = 0;}
		int xlow = evtx - range;
		if(xlow < 0) {xlow = 0;}
		int yhigh = evty + range;
		if(yhigh >= zheight) {yhigh = zheight - 1;}
		int xhigh = evtx + range;
		if(xhigh >= zwidth) {xhigh = zwidth - 1;}
		
		if(xhigh - xlow < 1) {return new Dimension(-1,-1);}
		if(yhigh - ylow < 1) {return new Dimension(-1,-1);}
		
		int first = (width*realy + realx) * 4;
		if(first < 0)  {
//			firstFLAG = true;
			if(realx < 0) {first = 0;}
			else {first = realx * 4;}
		}
		first += width * ylow * 4;
		first += xlow * 4;
		
		byte[] newline = new byte[(xhigh-xlow) * 4];	//yep, it's the four byte thing again.

		if(!rafFLAG) {
			if(filename != null) {
				try {
					System.out.println("grrr.");
					raf = new RandomAccessFile(filename, "r");
					rafFLAG = true;
				}
				catch(Exception e) {
					logger.log("error opening file: " + filename + " for brightest.");
				}	
			}
		}
		
		try {
			//okay, get the proper starting place
			//read in one line
			//convert to int's
			//save the position of the highest
			//skip the right amount
			//do it over.
			
			raf.seek(first); 
			int now = first;
			
			for(int i=ylow; i < yhigh; i++) {
//					if(!firstFLAG) 
//						raf.skipBytes(start*4);			//what?! why?
				raf.read(newline);

				for(int j=0; j < (xhigh - xlow); j++) {

					if(newline[j*4] < 0)
						charlie =	(256 + newline[j*4]) * 16777216;
					else
						charlie =	newline[j*4] * 16777216;
					
					if(newline[j*4+1] < 0)
						charlie +=	(256 + newline[j*4+1]) * 65536;
					else
						charlie +=	newline[j*4+1] * 65536;
					
					if(newline[j*4+2] < 0)
						charlie +=	(256 + newline[j*4+2]) * 256;
					else
						charlie +=	newline[j*4+2] * 256;
					
					if(newline[j*4+3] < 0)
						charlie +=	(256 + newline[j*4+3]);
					else
						charlie +=	newline[j*4+3];

					if(charlie > high) {
						high = charlie;
						etarget = new Dimension(j+xlow, i);
					}

				}				
				now += width*4;
				raf.seek(now);
			}
		}
		catch(Exception e) {
			e.printStackTrace();
			System.out.println(e);
		}
		return etarget;
	}

	public void exportImage(String filename, Image km) {
		int kwidth  = km.getWidth(null);
		int kheight = km.getHeight(null);
		int[] pixelarray = new int[kwidth*kheight];
		byte[] bytearray = new byte[kwidth*kheight];
		PixelGrabber pg = new PixelGrabber(km, 0, 0, kwidth, kheight, pixelarray, 0, kwidth);
//		PixelGrabber pg = new PixelGrabber(zm, 0, 0, zwidth, zheight, false);

		try {
			pg.grabPixels();
			for(int i = 0; i < kwidth*kheight; i++)
				bytearray[i] = (byte) (pixelarray[i] & 0x000000FF);
//			bytearray = (byte[]) pg.getPixels();
			
			FileWriter out = new FileWriter(filename);
			
			out.write("P5 ");
			out.write(kwidth + " " + kheight + " " + "255\n");
			
			out.flush();
			out.close();
			
			
			FileOutputStream fos = new FileOutputStream(filename, true);
			BufferedOutputStream to = new BufferedOutputStream(fos);
			
			to.write(bytearray);
			
			to.flush();
			to.close();
		}
		catch(Exception e) {
			logger.log("error exporting image.");
		}
	}

	public void colourarray(int low, int high) {
		if(low < 0)
			low = 0;
		if(low > 255)
			low = 255;
		if(high < 0)
			high = 0;
		if(high > 255)
			high = 255;
		
		contrastlow = low;
		contrasthigh = high;
		
		if(high-low != 0) {		//yeah, it's wanky. but it prevents div by zero errors.
			double ratio = 255;
			ratio /= high - low;
			for(int i = 0; i < 256; i++) {
				if(i < low)
					greyarray[i] = (byte) 0;
				else if(i > high)
					greyarray[i] = (byte) 255;
				else
					greyarray[i] = (byte) ((i - low) * ratio);
			}
		}
		else {
			for(int i = 0; i < 256; i++) {
				if(i < low)
					greyarray[i] = (byte) 0;
				else if(i > high)
					greyarray[i] = (byte) 255;
				else
					greyarray[i] = (byte) low;
			}
		}

		cm = new IndexColorModel(8, 256, greyarray, greyarray, greyarray);
	}

	public boolean closeImage() {
		if(raf != null) {
			try {
				raf.close();
				rafFLAG = false;
			}
			catch(Exception e) {
				logger.log("error closing image file.");
				return false;
			}
		}
		return true;
	}
}

//MASK

class mask {
	int imagesize;   //our lowres window
	String filename;
	int width;			//the real image
	int height;
	Image im;
	pvs mainFrame;
	boolean drawing;
	boolean rafFLAG;
	byte[] imagearray;
	IndexColorModel cm;

	mask (pvs mainFrame, int imagesize, int width, int height) {
		this.imagesize = imagesize;
		this.width = width;
		this.height = height;
		this.mainFrame = mainFrame;
	
		imagearray = new byte[imagesize*imagesize];

		byte[] red = {(byte) 0, (byte) 255};
		byte[] blue = {(byte) 0, (byte) 0};
		byte[] green = {(byte) 0, (byte) 0};
		byte[] alpha = {(byte) 0, (byte) 70};

		cm = new IndexColorModel(1, 2, red, green, blue, alpha);
	}

	mask (pvs mainFrame, String filename, int imagesize, int width, int height) {
		this.imagesize = imagesize;
		this.filename = filename;
		this.mainFrame = mainFrame;

		this.width = width;
		this.height = height;
		double fwidth = width;
		double fheight = height;
		double wratio = fwidth/imagesize;
		double hratio = fheight/imagesize;

		int wack = width/8;		//try this for now...
		if(width%8 != 0)
			wack++;
		byte[] inputarray = new byte[wack];
		imagearray = new byte[imagesize*imagesize];
		int[] countarray = new int[imagesize];

		try {
			FileInputStream fis = new FileInputStream(filename);
			BufferedInputStream bis = new BufferedInputStream(fis);
			DataInputStream from = new DataInputStream(bis);

			int oldi = -1;

			for(int i=0; i < height; i++) {
				if((int) (i/hratio) > oldi) {
					for(int j=0; j < imagesize; j++) //100? why 100...?
						if(countarray[j] > hratio*wratio/100) {imagearray[oldi*imagesize+j] = 1;}
					countarray = new int[imagesize];
				}
				from.read(inputarray);
				for(int j=0; j < wack; j++) {
					int bus = inputarray[j];
					if(bus < 0) {bus += 128; countarray[(int) ((j*8)/wratio)]++;}
					if((bus >= 64) && (((j*8+1)/wratio) < imagesize) ) {bus -= 64; countarray[(int) ((j*8+1)/wratio)]++;}
					if((bus >= 32) && (((j*8+2)/wratio) < imagesize) ) {bus -= 32; countarray[(int) ((j*8+2)/wratio)]++;}
					if((bus >= 16) && (((j*8+3)/wratio) < imagesize) ) {bus -= 16; countarray[(int) ((j*8+3)/wratio)]++;}
					if((bus >= 8) && (((j*8+4)/wratio) < imagesize) ) {bus -= 8; countarray[(int) ((j*8+4)/wratio)]++;}
					if((bus >= 4) && (((j*8+5)/wratio) < imagesize) ) {bus -= 4; countarray[(int) ((j*8+5)/wratio)]++;}
					if((bus >= 2) && (((j*8+6)/wratio) < imagesize) ) {bus -= 2; countarray[(int) ((j*8+6)/wratio)]++;}
					if((bus >= 1) && (((j*8+7)/wratio) < imagesize) ) {bus -= 1; countarray[(int) ((j*8+7)/wratio)]++;}
				}
				oldi = (int) (i/hratio);
			}

			from.close();
		}
		catch(Exception e) {
			logger.log("error opening mask file.");
		}

		byte[] red = {(byte) 0, (byte) 255};
		byte[] blue = {(byte) 0, (byte) 0};
		byte[] green = {(byte) 0, (byte) 0};
		byte[] alpha = {(byte) 0, (byte) 70};

		cm = new IndexColorModel(1, 2, red, green, blue, alpha);
	}

	public void exportMask(String filename) {
		int[] b = {1,2,4,8,16,32,64,128};
		int oldi = -1;
		int modi = 0;
		int wack = width/8;
		if(width%8 != 0)
			wack++;
		int chunk = 100;			//number of rows to take at a time
		double fwidth = width;
		double fheight = height;
		double wratio = fwidth/imagesize;
		double hratio = fheight/imagesize;
		this.filename = filename;

		//process meter stuff
		procDialog pd = new procDialog(mainFrame, "exporting mask...");	
		int locatex = mainFrame.getLocation().x;	//get from above...	
		int locatey = mainFrame.getLocation().y;
		if(locatex > 0 && locatey > 0)
			pd.setLocation(locatex + 3, locatey + 41);
		pd.show();
		int done = 0;
			
		try {
			FileOutputStream fos = new FileOutputStream(filename);
			BufferedOutputStream bos = new BufferedOutputStream(fos);
			DataOutputStream to = new DataOutputStream(bos);

			byte[] outputarray = new byte[wack];
			byte[] bigoutputarray = new byte[wack*chunk];

			for(int i=0; i < height; i++) {
				//process meter stuff
				if((i*100)/height > done) {
					done = (i*100)/height;
					pd.process(done);
				}
					
				if(i/hratio > oldi) {
					outputarray = new byte[wack];
					for(int j=0; j < width; j++) {
						if(imagearray[(int) ((int) (i/hratio) * imagesize + j/wratio)] == 1)
							outputarray[(int) j/8] += b[j%8];
					}
				}
				oldi = (int) (i/hratio);

				modi = i%chunk;		//this doesn't seem to give the sort of performance increase i had hoped...
				for(int k=0; k < wack; k++)			//write a block of chunk rows at a time.
					bigoutputarray[modi*wack + k] = outputarray[k];
				if(modi == chunk-1) {
					to.write(bigoutputarray);
					bigoutputarray = new byte[wack*chunk];
				}
			}
			if(modi != chunk-1) {									//finish off the last chunk
				byte[] newoutputarray = new byte[(modi+1)*wack];
				for(int l=0; l < (modi+1)*wack; l++)
					newoutputarray[l] = bigoutputarray[l];
				to.write(newoutputarray);
			}

			to.flush();
			to.close();
		}
		catch(Exception e) {
			logger.log("error exporting mask.");
		}
		finally {
			pd.dispose();	//kill the process meter at the end.
		}
	}

	public void updateImage(int x1, int y1, int x2, int y2, boolean transparent) {
		for(int i = y1; i < y2; i++) {
			for(int j = x1; j < x2; j++) {
				if((i >= 0) && (i < imagesize) && (j >= 0) && (j < imagesize)){
					if(transparent) {imagearray[i*imagesize + j] = (byte) 0;}
					else {imagearray[i*imagesize + j] = (byte) 1;}
				}
			}
		}

		im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize,
					cm, imagearray, 0, imagesize));
	}

	public void updateImage() {
		im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize,
					cm, imagearray, 0, imagesize));
	}

	public Image drawPen(int pensize) {
		byte[] penarray = new byte[pensize*pensize*4];
		for(int i = 1; i < pensize*pensize*4; i++){
			penarray[i] = 1;
		}

		return Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(pensize*2, pensize*2,
						cm, penarray, 0, pensize*2));
	}

	public Image display() {
		return im;
	}

	public Image display(int realx, int realy, int zwidth, int zheight) {
		realx = realx-zwidth/2;
		realy = realy-zheight/2;

		int jwidth 	= zwidth*imagesize;		//scaled down width and height
		int jheight = zheight*imagesize;
		jwidth 	/= width;
		jheight	/= height;
		jwidth  += 2;		//two is just for kicks
		jheight += 2;		//to make sure it's big enough.
		
		int x = realx * imagesize;	//scaled down x and y
		int y = realy * imagesize;
		x /= width;
		y /= height;
		
//System.out.println("realx:"+realx+" realy:"+realy+" x:"+x+" y:"+y);
//System.out.println("zwidth:"+zwidth+" zheight:"+zheight+" jwidth:"+jwidth+" jheight:"+jheight);
//System.out.println("con x:"+((x*width)/imagesize)+" con y:"+((y*height)/imagesize)+" con jw:"+((jwidth*width)/imagesize)+" con jy:"+((jheight*height)/imagesize));
//System.out.println("cropx:"+(realx - (x*width)/imagesize)+" cropy:"+(realy - (y*height)/imagesize));
		
		Image i, j, k;
		
		i = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(jwidth, jheight,
					cm, imagearray, y*imagesize + x, imagesize));
					
		//okay, now we've got this tiny, chunky, blocky image.
		//let's make it bigger.
		
		ReplicateScaleFilter rsf = new ReplicateScaleFilter((jwidth*width)/imagesize, (jheight*height)/imagesize);
		j = Toolkit.getDefaultToolkit().createImage(new FilteredImageSource(i.getSource(), rsf));
		
		//now we need to crop it to size...
		
		CropImageFilter cif = new CropImageFilter(realx - ((x*width)/imagesize), realy - ((y*height)/imagesize), zwidth, zheight);
		k = Toolkit.getDefaultToolkit().createImage(new FilteredImageSource(j.getSource(), cif));
		
		return k;
	}
}

//SMASK

class smask {
	int imagesize;   //our lowres window
	String filename;
	int width;			//the real image
	int height;
	int zwidth;
	int zheight;
	Image im;		//lowres image - only change when we update the zoom window.
	Image zm;		//zoom window - where all the good stuff happens.
	boolean drawing;  //are we drawing right now?
	boolean rafFLAG;
	byte[] imagearray;  //this is just for the lowres window.
	IndexColorModel cm;
	RandomAccessFile raf;
	boolean[][] smaskarray;

	smask (String filename, int imagesize, int width, int height) {  //begin a new smask
		this.filename = filename;			//eventually, unify these calls and both import
		this.width = width;						//and start a new smask with the same menu command.
		this.height = height;
		this.imagesize = imagesize;
		int wack = width/8;		//try this for now...
		if(width%8 != 0)
			wack++;

		imagearray = new byte[imagesize*imagesize];  //hello, kitty. . .

		createColour();

		im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize,
					cm, imagearray, 0, imagesize));

		try {		//here we write the new smask file, so we have something to work with later.
			FileOutputStream fos = new FileOutputStream(filename);
			BufferedOutputStream bos = new BufferedOutputStream(fos);
			DataOutputStream to = new DataOutputStream(bos);

			byte[] outputarray = new byte[wack];  //hmmm. fix this.
			for(int i=0; i < height; i++) {
				to.write(outputarray);
			}
			
			to.close();
		}
		catch(Exception e) {
			logger.log("error opening smask file.");
		}
	}

	smask (String filename, int imagesize, int width, int height, boolean erg) { //import smask
		this.imagesize = imagesize;
		this.filename = filename;
		this.width = width;
		this.height = height;

		importSmask(filename);
		createColour();

		im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize,
				  cm, imagearray, 0, imagesize));

	}

	public void startSmask(int zwidth, int zheight) {
		this.zwidth = zwidth;
		this.zheight = zheight;
		smaskarray = new boolean[zheight][zwidth];   //it's backwards, see?
	}

	private void createColour() {
		byte[] red = {(byte) 0, (byte) 255};
		byte[] blue = {(byte) 0, (byte) 0};
		byte[] green = {(byte) 0, (byte) 0};
		byte[] alpha = {(byte) 0, (byte) 70};

		cm = new IndexColorModel(1, 2, red, green, blue, alpha);
	}

	public void addLine(int oldx, int oldy, int x, int y) {
		if(oldx < 1) {oldx = 1;}
		if(x < 1) {x = 1;}
		if(smaskarray != null) {
			double miny = Math.min(oldy, y);
			double maxy = Math.max(oldy, y);

			if(oldy == y) {
				double minx = Math.min(oldx, x);
				double maxx = Math.max(oldx, x);
				for(int i = (int) minx; i <= maxx; i++)
					smaskarray[y][i] = true;
			}
			else if(oldx == x)
				for(int i = (int) miny; i <= maxy; i++) 
					smaskarray[i][x] = true;
			else {
				double minx; double maxx;
				if(miny == oldy) {minx = oldx; maxx = x;}
				else						 {minx = x; maxx = oldx;}
				double slope = (maxx - minx)/(maxy - miny);
				for(int i = 0; i < maxy-miny; i++) 
					smaskarray[(int)(miny + i)][(int)(minx + (int)(i*slope))] = true;
				smaskarray[(int)maxy][(int)maxx] = true;
			}
		}
	}

	public void endSmask(int realx, int realy) {	
		realx -= zwidth/2;		//to put x in the middle of the zwidth*zheight block.
		realy -= zheight/2;
//		x--;
		
		decipherSmask(realx, realy);
		smaskarray = null;
		importSmask(filename);
	}

	private void decipherSmask(int realx, int realy) {
		Vector crossings = new Vector(20);
		Vector newspan = new Vector(10);
		Vector span = new Vector(10);
		Vector bad = new Vector(20);
		int firstbad = 0;

		for(int i=1; i < zheight-1; i++) {    //ech. this might go out of image bounds....
			boolean badFlag = false;
			int next = 1;
			int segments = 0;
			int spanindex = 0;
			int goindex = 0;
			int stop = 0;
			int go = 0;			

			for(int j=0; j < zwidth; j++)   //count points in current row.
				if(smaskarray[i][j] == true) {
					Integer jop = new Integer(j);
					crossings.addElement(jop); 
					if(smaskarray[i][j-1] != true)
						segments++;
				}

			if(crossings.size() == 0)	//no points, goto next line
				continue;  

//if no previous points, try to start a span

			if(span.size() == 0)	{		
				next = 0;  //put stop and go together at first.
				 
				while(next < crossings.size()) {
					go = ((Integer)crossings.elementAt(next)).intValue();
					
					while(next < crossings.size()) {  //get a better stop.
						stop = ((Integer)crossings.elementAt(next)).intValue();
	
						if(next + 1 == crossings.size())	//break at the end.
							break;
					
						if(((Integer)crossings.elementAt(next+1)).intValue() != stop + 1)  //ignore them at the end, too.
							break;

						next++;
					}
					
					//now we've got the right stop for the go.
						
					span.addElement(new spanpair(go, stop));
					
					next++;
				}

				continue;
			}
// okay, we've got a span for sure now.

			if(crossings.size() == 1)	{ //no points, goto next line
				go = ((Integer)crossings.elementAt(0)).intValue();
				span.removeAllElements();
				span.addElement(new spanpair(go, go));
				continue;   
			}
			
//sanity check to ensure no wackiness is occuring

			next = 1;
			while(spanindex < span.size() && badFlag == false && next < crossings.size()) {
				go = ((Integer)crossings.elementAt(next-1)).intValue();
				goindex = next-1; 
				 
				while(next < crossings.size()) {  //get a good stop.
					stop = ((Integer)crossings.elementAt(next)).intValue();
					
					if(stop != go + next - goindex)	//ignore beginning consecutive points. 
						break;
						
					next++;
				}

				while(next < crossings.size()) {  //get a better stop.
					stop = ((Integer)crossings.elementAt(next)).intValue();

					if(next + 1 == crossings.size())	//break at the end.
						break;
					
					if(((Integer)crossings.elementAt(next+1)).intValue() != stop + 1)  //ignore ending consec points.
						break;
						
					next++;
				}
				
				//okay, we've got a stop and go, now check them with the span.
				
				spanpair pj = (spanpair)span.elementAt(spanindex);
				
				if(Math.min((pj.stop - go), (stop - pj.go)) < Math.min((pj.stop - pj.go), (stop - go))/2) 
					badFlag = true;
				
				next += 2;	
				spanindex++;
			}
			next = 1; //reset these for the next part
			spanindex = 0;
					
//if there's a problem, stop here.

			if(segments != span.size()*2 || badFlag) {
				while(next < crossings.size()) {	//do all that good stuff.
					go = ((Integer)crossings.elementAt(next-1)).intValue();
					goindex = next-1; 
					 
					while(next < crossings.size()) {  //get a good stop.
						stop = ((Integer)crossings.elementAt(next)).intValue();
						
						if(stop != go + next - goindex)	//ignore beginning consecutive points. 
							break;
							
						next++;
					}
	
					while(next < crossings.size()) {  //get a better stop.
						stop = ((Integer)crossings.elementAt(next)).intValue();
	
						if(next + 1 == crossings.size())	//break at the end.
							break;
						
						if(((Integer)crossings.elementAt(next+1)).intValue() != stop + 1)  //ignore ending consec points.
							break;
							
						next++;
					}
					
					newspan.addElement(new spanpair(go, stop));
					
					next += 2;				
				}
				if(firstbad == 0)
					firstbad = i;
			}

//no problem, do this.

			else {
				while(next < crossings.size()) {	//do all that good stuff.
					go = ((Integer)crossings.elementAt(next-1)).intValue();
					goindex = next-1; 
					 
					while(next < crossings.size()) {  //get a good stop.
						stop = ((Integer)crossings.elementAt(next)).intValue();
						
						if(stop != go + next - goindex)	//ignore beginning consecutive points. 
							break;
							
						next++;
					}
	
					while(next < crossings.size()) {  //get a better stop.
						stop = ((Integer)crossings.elementAt(next)).intValue();
	
						if(next + 1 == crossings.size())	//break at the end.
							break;
						
						if(((Integer)crossings.elementAt(next+1)).intValue() != stop + 1)  //ignore ending consec points.
							break;
							
						next++;
					}
					
					exportSmask(realx, realy, i, go, stop);
					newspan.addElement(new spanpair(go, stop));
					for(int j=go; j < stop; j++)
						smaskarray[i][j] = true;
					
					next += 2;				
				}
				if(firstbad != 0) {
					bad.addElement(new spanpair(firstbad - 1, i));
					firstbad = 0;
				}
			}
				
			crossings.removeAllElements();
			span.removeAllElements();
			span = (Vector) newspan.clone();
			newspan.removeAllElements();
		}
		
		if(bad.size() != 0) {
			for(int i=0; i < bad.size(); i++) {
				spanpair badpair = (spanpair)bad.elementAt(i);
				int go = 0; int stop = 0; boolean done = false;
				
				while(!done) {	//go through the whole line

					while(go < zwidth) {  //get a good go.

						if(smaskarray[badpair.go][go] && smaskarray[badpair.stop][go])	
							break;
							
						go++;
					}
					
					stop = go;
	
					while(stop < zwidth) {  //get a good stop.
	
						if(!(smaskarray[badpair.go][stop] || smaskarray[badpair.stop][stop]))	
							break;
							
						stop++;
					}
					
					if(stop >= zwidth)
						break;

					for(int j=badpair.go + 1; j < badpair.stop; j++)
						exportSmask(realx, realy, j, go, stop);
					
					go++;
				}
			}
		}
	}
		
	private void exportSmask(int realx, int realy, int i, int go, int stop) {   
		int wack = width/8;		//try this for now...
		if(width%8 != 0)
			wack++;
		int first = realy * wack + realx/8;
		
		if(!rafFLAG) {
			if(filename != null) {
				try {
					raf = new RandomAccessFile(filename, "rw");
					rafFLAG = true;
				}
				catch(Exception e) {
					logger.log("error opening file: " + filename + " for smask.");
				}
			}
		}


		try {
			byte[] outputarray;
			String outputstring;

			int modsize = (stop - go)%8;
			int modstop = stop%8;
			int modgo = go%8;

			if(stop/8 - go/8 == 0) {
				outputarray = new byte[1];
				byte gobyte = 0;
				raf.seek(first + i*wack + go/8);
				raf.read(outputarray);
				for(int k = modgo; k <= modstop; k++) 
					gobyte += (int) Math.pow(2, (7-k));

				outputarray[0] = (byte) (gobyte | outputarray[0]);

				raf.seek(first + i*wack + go/8);
				raf.write(outputarray);
			}
			else if(stop/8 - go/8 == 1) {
				outputarray = new byte[2];
				raf.seek(first + i*wack + go/8);
				raf.read(outputarray);

				byte gobyte = 0;
				int substitute = 1;
				while(modgo < 8) {
					gobyte += substitute;
					substitute *= 2;
					modgo++;
				}
				
				byte stopbyte = -128;
				substitute = 64;
				modstop--;
				while(modstop > 0) {
					stopbyte += substitute;
					substitute /= 2;
					modstop--;
				}

				outputarray[0] = (byte) (outputarray[0] | gobyte);
				outputarray[1] = (byte) (outputarray[1] | stopbyte);

				raf.seek(first + i*wack + go/8);
				raf.write(outputarray);
			}
			else if(modsize == 0 && modgo == 0) {
				int outsize = (stop - go)/8;
				outputarray = new byte[outsize];
				for(int j = 0; j < outsize; j++) 
					outputarray[j] = (byte) -1;

				raf.seek(first + i*wack + go/8);
				raf.write(outputarray);
			}
			else if(modgo == 0) {
				int outsize = (stop - go)/8 + 1;
				outputarray = new byte[outsize];
				raf.seek(first + i*wack + go/8);
				raf.read(outputarray);
				
				byte stopbyte = -128;
				int substitute = 64;
				modstop--;
				while(modstop > 0) {
					stopbyte += substitute;
					substitute /= 2;
					modstop--;
				}
				
				for(int j = 0; j < outsize - 1; j++) 
					outputarray[j] = (byte) -1;

				outputarray[outsize-1] = (byte) (outputarray[outsize-1] | stopbyte);
			
				raf.seek(first + i*wack + go/8);
				raf.write(outputarray);
			}
			else if(modstop == 0) {
				int outsize = (stop - go)/8 + 1;
				outputarray = new byte[outsize];
				raf.seek(first + i*wack + go/8);
				raf.read(outputarray);
				
				byte gobyte = 0;
				int substitute = 1;
				while(modgo < 8) {
					gobyte += substitute;
					substitute *= 2;
					modgo++;
				}
				
				for(int j = 1; j < outsize; j++) 
					outputarray[j] = (byte) -1;

				outputarray[0] = (byte) (outputarray[0] | gobyte);
			
				raf.seek(first + i*wack + go/8);
				raf.write(outputarray);
			}
			else {
				int outsize = ((stop - modstop) - (go - modgo))/8 + 1;
				outputarray = new byte[outsize];
				raf.seek(first + i*wack + go/8);
				raf.read(outputarray);

				byte stopbyte = -128;
				int substitute = 64;
				modstop--;
				while(modstop > 0) {
					stopbyte += substitute;
					substitute /= 2;
					modstop--;
				}

				byte gobyte = 0;
				substitute = 1;
				while(modgo < 8) {
					gobyte += substitute;
					substitute *= 2;
					modgo++;
				}
				outputarray[0] = (byte) (outputarray[0] | gobyte);

				for(int j = 1; j < outsize - 1; j++)
					outputarray[j] = (byte) -1;

				outputarray[outsize-1] = (byte) (outputarray[outsize-1] | stopbyte);
				
				raf.seek(first + i*wack + go/8);
				raf.write(outputarray);
			}
			go = stop;
		}
		catch(Exception e) {
			logger.log("error exporting smask file.");
		}
	}

	private void importSmask(String filename) {
		double fwidth = width;
		double fheight = height;
		double wratio = fwidth/imagesize;   
		double hratio = fheight/imagesize;
		int wack = width/8;		//try this for now...
		if(width%8 != 0)
			wack++;
		byte[] inputarray = new byte[wack];
		imagearray = new byte[imagesize*imagesize];
		int[] countarray = new int[imagesize];

		try {
			FileInputStream fis = new FileInputStream(filename);
			BufferedInputStream bis = new BufferedInputStream(fis);
			DataInputStream from = new DataInputStream(bis);

			int oldi = -1;
			
			for(int i=0; i < height; i++) {
				if((int) (i/hratio) > oldi) {
					for(int j=0; j < imagesize; j++) 
						if(countarray[j] > hratio*wratio/100) {imagearray[oldi*imagesize+j] = 1;}
					countarray = new int[imagesize];
				}
				from.read(inputarray);
				for(int j=0; j < wack; j++) { 	//here it is again. . . 
					int bus = inputarray[j];
					if(bus < 0) {bus += 128; countarray[(int) ((j*8)/wratio)]++;}
					if(bus >= 64) {bus -= 64; countarray[(int) ((j*8+1)/wratio)]++;}
					if(bus >= 32) {bus -= 32; countarray[(int) ((j*8+2)/wratio)]++;}
					if(bus >= 16) {bus -= 16; countarray[(int) ((j*8+3)/wratio)]++;}
					if(bus >= 8) {bus -= 8; countarray[(int) ((j*8+4)/wratio)]++;}
					if(bus >= 4) {bus -= 4; countarray[(int) ((j*8+5)/wratio)]++;}
					if(bus >= 2) {bus -= 2; countarray[(int) ((j*8+6)/wratio)]++;}
					if(bus >= 1) {bus -= 1; countarray[(int) ((j*8+7)/wratio)]++;}
					
					if(bus != 0) {System.out.print("what?! ");}
				}
				oldi = (int) (i/hratio);
			}

			from.close();
		}
		catch(Exception e) {
			logger.log("error importing smask file.");
		}	
		
		im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize, 
				  cm, imagearray, 0, imagesize)); 

	}

	public Image display() {
		return im;
	}

	public Image display(int realx, int realy, int zwidth, int zheight) {   //preadjust for ratio, expand or contract up there
		realx -= zwidth/2;		//to put x in the middle of the zwidth*zheight block.
		realy -= zheight/2;
		
		newzoom(realx, realy, zwidth, zheight, realx%8);  //i know, putting the mod8 in here is stupid.
		return zm;																				//but i get a JIT error if i don't . . . 
	}
	
	protected void newzoom(int realx, int realy, int zwidth, int zheight, int modrealx) {
		boolean firstFLAG = false;
		byte[] zoomarray = new byte[zwidth * zheight];

		int wack = width/8;	
		if(width%8 != 0)
			wack++;
		int first = wack*realy;  //divide by eight for bits, not ints.
		if(first < 0)  
			first = 0;
		if(first > width*height/8) {first = 0;} //now we're outside the bounds of the continuum.
		
		if(raf == null) {		///eh, change all this.
			if(filename != null) {
				try {
					raf = new RandomAccessFile(filename, "rw");
				}
				catch(Exception e) {
					logger.log("error opening file: " + filename + " for smask zoom.");
				}
			}
		}

		try {
			raf.seek(first); 	
		
			byte[] inputarray = new byte[wack*zheight];  //pop it all at once. . . 
	
			raf.read(inputarray);
	
			for(int i=0; i < zheight; i++) {
				for(int j=0; j < zwidth/8 + 2; j++) { 	 
					if(i*wack + realx/8 + j > 0) {				
						int bus = inputarray[i*wack + realx/8 + j];
						int where = j*8 - modrealx;
						
						if(bus < 0) {
							bus += 128; 
							if(where > 0 && where < zwidth) {
								zoomarray[i*zwidth + where] = 1;
							}
						}
						
						if(bus >= 64) {
							bus -= 64; 
							if(where + 1 > 0 && where + 1 < zwidth) {
								zoomarray[i*zwidth + where + 1] = 1;
							}
						}
						
						if(bus >= 32) {
							bus -= 32; 
							if(where + 2 > 0 && where + 2 < zwidth) {
								zoomarray[i*zwidth + where + 2] = 1;
							}
						}
						
						if(bus >= 16) {
							bus -= 16; 
							if(where + 3 > 0 && where + 3 < zwidth) {
								zoomarray[i*zwidth + where + 3] = 1;
							}
						}
						
						if(bus >= 8) {
							bus -= 8; 
							if(where + 4 > 0 && where + 4 < zwidth) {
								zoomarray[i*zwidth + where + 4] = 1;
							}
						}
						
						if(bus >= 4) {
							bus -= 4; 
							if(where + 5 > 0 && where + 5 < zwidth) {
								zoomarray[i*zwidth + where + 5] = 1;
							}
						}
						
						if(bus >= 2) {
							bus -= 2; 
							if(where + 6 > 0 && where + 6 < zwidth) {
								zoomarray[i*zwidth + where + 6] = 1;
							}
						}
						
						if(bus >= 1) {
							bus -= 1; 
							if(where + 7 > 0 && where + 7 < zwidth) {
								zoomarray[i*zwidth + where + 7] = 1;
							}
						}
					}
				}
			}
		}
		catch(Exception e) {
			logger.log("error displaying zoom smask.");
		}
		zm = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(zwidth, zheight, 
				  cm, zoomarray, 0, zwidth)); 
	}
}

class spanpair {
	int go;		//beginning of span
	int stop;		//end of span
	
	spanpair (int go, int stop) {
		this.go = go;
		this.stop = stop;
	}
}

//POINT TARGETS
class pointtargets {
	String filename;
	private Vector alltargets;
	public Vector visibletargets;
	private Vector trgs;


	pointtargets(String filename) {
		this.filename = filename;
		int numpts = 0;
		try {
			FileInputStream in = new FileInputStream(
                        	new File (filename + Constants.ground_target_file));
                        TargetIngester t = new TargetIngester( in);
                        Vector v = t.getElements();

			trgs = v;
			
				//Save Targets
                        alltargets = new Vector();
                        int ii;

                        for ( ii=0; ii < v.size(); ii++ )
                          {
				Target T = (Target)v.elementAt(ii);
				T.print();
                                pt no = new pt(T.cs_dev_id, (int)( T.projected_image_x),
                                                (int)(T.projected_image_y), T);
                                alltargets.add(no);
                          }
			
		}
		catch (Exception e){
			logger.log("error opening point targets (" + e + ").");
		   }
		resetvisible();
	}
	
	public void importtargets() {
		try {
                        FileInputStream in = new FileInputStream(
                              new File (filename + Constants.ground_target_file_located));
                        TargetIngester t = new TargetIngester( in);
                        Vector v = t.getElements();
			trgs = v;
                        alltargets = new Vector();
                        int ii;

                        for ( ii=0; ii < v.size(); ii++ )
                          {
                                Target T = (Target)v.elementAt(ii);
                                T.print();
                                pt no = new pt(T.cs_dev_id, (int)( T.projected_image_x),
                                                (int)(T.projected_image_y), T);

				if( (T.actual_image_x != 0) || ( T.actual_image_y != 0) )
					actualize(T.cs_dev_id, 
						(int)(T.actual_image_x+.5), 
						(int)(T.actual_image_y+.5));
				
                                alltargets.add(no);
                          }
		
		}
		catch (Exception e){
			logger.log("error importing point targets.");
		}
		resetvisible();
	}
	
	public void exporttargets() {
		String outfile = filename + ".ground_target_final";		
			//filename extension for output will probably change.
		String end = System.getProperty("line.separator");
		try {
			PrintStream ps = new PrintStream (new FileOutputStream(outfile));
			for(int i=0; i < visibletargets.size(); i++) {
				pt now = (pt) visibletargets.elementAt(i);
				if(now.actualized) {	
					Target T = (Target)trgs.elementAt(i);
					T.actual_image_x = now.actualx;
					T.actual_image_y = now.actualy;
					T.print(ps);
				     }
			}

			
			
			ps.close();
		}
		catch(IOException e) {
			logger.log("error exporting point targets.");
		}
	}
	
	public void resetvisible() {
		visibletargets = (Vector) alltargets.clone();
	}
	
	public void remove(String devname) {
		for(int i=0; i < visibletargets.size(); i++) {
			pt now = (pt) visibletargets.elementAt(i);
			if(now.devname == devname) {
				visibletargets.removeElementAt(i);
				break;
			}
		}
	}
	
	public void actualize(String devname, int actualx, int actualy) {
		for(int i=0; i < visibletargets.size(); i++) {
			pt now = (pt) visibletargets.elementAt(i);
			if(now.devname.equals(devname)) {
				now.actualx = actualx;
				now.actualy = actualy;
				now.actualized = true;
				break;
			}
		}
	}
	
	public Vector zoompoints(int realx, int realy, int zwidth, int zheight) { //give me the preadjusted x and y.
		Vector zoomvector = new Vector(30);	//30 is just whatever.
		
		for(int i=0; i < visibletargets.size(); i++) {
			pt now = (pt) visibletargets.elementAt(i);
			if(now.actualized) {
				if((now.actualx > realx && now.actualx < realx+zwidth) && (now.actualy > realy && now.actualy < realy+zheight))
					zoomvector.addElement(visibletargets.elementAt(i));
			
			}
			else {
				if((now.potentialx > realx && now.potentialx < realx+zwidth) && (now.potentialy > realy && now.potentialy < realy+zheight))
					zoomvector.addElement(visibletargets.elementAt(i));
			}
		}
		
		return zoomvector;
	}
}

class pt {
	String devname;
	int actualx;
	int actualy;
	int potentialx;
	int potentialy;
	boolean actualized;
	Target t;


	
	pt (String devname, int potentialx, int potentialy, Target T) {
		this.devname = devname;
		this.potentialx = potentialx;
		this.potentialy = potentialy;
		T = t;
	}
	
	public void actualize(int actualx, int actualy) {
		this.actualx = actualx;
		this.actualy = actualy;
		actualized = true;
	}
}

class logger {
	static String preffile = "pvs.pref";	//echoed in pvs()...
	static String end = System.getProperty("line.separator");
	static String logfile = null;
	
	public static void log (String statement) {	//maybe i should display the exception also?
		Date dt = new Date();
		
		//get the log filename from the preferences file

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
		
			boolean lf = false;
			while(!lf) {
				String thisline = in.readLine();
				StringTokenizer s = new StringTokenizer(thisline);
				while(s.hasMoreTokens())
					if(s.nextToken().equalsIgnoreCase("logfile")) {
						logfile = thisline.substring(thisline.indexOf(s.nextToken()));
						lf = true;
						break;
					}
			}
			
			fr.close();
		}
		catch (Exception e){
			System.err.println(e);
			System.out.println("preferences? i thought you said praeter naturam!");
		}
		
		//write to the logfile
		
		if(logfile == null)
			logfile = "pvs.log";
		
		try {

			System.err.println(dt.toLocaleString() + " " + statement );
			FileWriter to = new FileWriter(logfile, true);
			to.write(dt.toLocaleString() + " " + statement + end);
			to.flush();
			to.close();

		}
		catch(IOException e) {
			System.err.println(e);
			System.out.println("uh, houston. . . i think we have a problem.");
		}
	}
}

//this isn't mine. it came from the java AWT reference book.

class BlurFilter extends ImageFilter {
	private int savedWidth, savedHeight, savedPixels[];
	private static ColorModel defaultCM = ColorModel.getRGBdefault();
	public void setDimensions (int width, int height) {
		savedWidth=width;
		savedHeight=height;
		savedPixels=new int [width*height];
		consumer.setDimensions (width, height);
	}
	public void setColorModel (ColorModel model) {
// Change color model to model you are generating
		consumer.setColorModel (defaultCM);
	}
	public void setHints (int hintflags) {
// Set new hints, but preserve SINGLEFRAME setting
		consumer.setHints(TOPDOWNLEFTRIGHT | COMPLETESCANLINES |
											SINGLEPASS | (hintflags & SINGLEFRAME));
	}
	private void setThePixels(int x, int y, int width, int height,
														ColorModel cm, Object pixels, int offset, int scansize) {
		int sourceOffset = offset;
		int destinationOffset = y * savedWidth + x;
		boolean bytearray = (pixels instanceof byte[]);
		for (int yy=0;yy<height;yy++) {
			for (int xx=0;xx<width;xx++)
				if (bytearray)
					savedPixels[destinationOffset++]=
						cm.getRGB(((byte[])pixels)[sourceOffset++]&0xff);
			else
				savedPixels[destinationOffset++]=
					cm.getRGB(((int[])pixels)[sourceOffset++]);
			sourceOffset += (scansize - width);
			destinationOffset += (savedWidth - width);
		}
	}
	public void setPixels(int x, int y, int width, int height,
												ColorModel cm, byte pixels[], int offset, int scansize) {
		setThePixels (x, y, width, height, cm, pixels, offset, scansize);
	}
	public void setPixels(int x, int y, int width, int height,
												ColorModel cm, int pixels[], int offset, int scansize) {
		setThePixels (x, y, width, height, cm, pixels, offset, scansize);
	}
	public void imageComplete (int status) {
		if ((status == IMAGEABORTED) || (status == IMAGEERROR)) {
			consumer.imageComplete (status);
			return;
		} 
		else {
			int pixels[] = new int [savedWidth];
			int position, sumArray[], sumIndex;
			sumArray = new int [9]; // maxsize - vs. Vector for performance
			for (int yy=0;yy<savedHeight;yy++) {
				position=0;
				int start = yy * savedWidth;
				for (int xx=0;xx<savedWidth;xx++) {
					sumIndex=0;
					sumArray[sumIndex++] = savedPixels[start+xx];
					if (yy != (savedHeight-1))
						sumArray[sumIndex++] = savedPixels[start+xx+savedWidth];
					if (yy != 0)
						sumArray[sumIndex++] = savedPixels[start+xx-savedWidth];
					if (xx != (savedWidth-1))
						sumArray[sumIndex++] = savedPixels[start+xx+1];
					if (xx != 0)
						sumArray[sumIndex++] = savedPixels[start+xx-1];
					if ((yy != 0) && (xx != 0))
						sumArray[sumIndex++] = savedPixels[start+xx-savedWidth-1];
					if ((yy != (savedHeight-1)) && (xx != (savedWidth-1)))
						sumArray[sumIndex++] = savedPixels[start+xx+savedWidth+1];
					if ((yy != 0) && (xx != (savedWidth-1)))
						sumArray[sumIndex++] = savedPixels[start+xx-savedWidth+1];
					if ((yy != (savedHeight-1)) && (xx != 0))
						sumArray[sumIndex++] = savedPixels[start+xx+savedWidth-1];
					pixels[position++] = avgPixels(sumArray, sumIndex);
				}
				consumer.setPixels (0, yy, savedWidth, 1, defaultCM,
														pixels, 0, savedWidth);
			}
			consumer.imageComplete (status);
		}
	}
	private int avgPixels (int pixels[], int size) {
		float redSum=0, greenSum=0, blueSum=0, alphaSum=0;
		for (int i=0;i<size;i++)
			try {
				int pixel = pixels[i];
				redSum   += defaultCM.getRed   (pixel);
				greenSum += defaultCM.getGreen (pixel);
				blueSum  += defaultCM.getBlue  (pixel);
				alphaSum += defaultCM.getAlpha (pixel);
			} 
			catch (ArrayIndexOutOfBoundsException e) {
				System.out.println ("Ooops"); 
			}
			int redAvg   = (int)(redSum   / size);
			int greenAvg = (int)(greenSum / size);
			int blueAvg  = (int)(blueSum  / size);
			int alphaAvg = (int)(alphaSum / size);
			return ((0xff << 24) | (redAvg << 16) |
							(greenAvg << 8)  | (blueAvg << 0));
	}
}

