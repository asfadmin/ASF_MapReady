
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


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

