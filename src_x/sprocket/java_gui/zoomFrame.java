
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


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

