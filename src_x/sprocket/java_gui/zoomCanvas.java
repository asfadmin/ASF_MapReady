/* ****************************************************************************
NAME: zoomCanvas.java

DESCRIPTION:
   The image within the zoom window.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
            07/03  P. Denny     Split bloated pvs.java up into
                                 files named after their classes
            07/03  P. Denny     Replaced depricated action and handleEvent
                                 methods with appropriate Listeners

**************************************************************************** */

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//ZOOM CANVAS

class zoomCanvas extends Canvas implements Runnable, KeyListener,
                                           MouseListener, MouseMotionListener {
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
   private volatile boolean threadSuspended;
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
   int memtarx = -1;   //the old target position
   int memtary = -1; 
   int targetx = 0;   //position of new target.
   int targety = 0;
   int startx = 0;   //position of MOUSE_DOWN event in smasking mode
   int starty = 0;
   int mousex = 0;   //current position of the mouse in flythrough mode;
   int mousey = 0;
   int range = 30;
   int flyx;
   int flyy;
   int memx = 0;   //last zoom position on main window, to keep from repainting multiple times.
   int memy = 0;
   int oldx = 0;      //need to change all this too...
   int oldy = 0;
   int realx = 0;
   int realy = 0;

   zoomCanvas(zoomFrame myFrame) {
      this.myFrame = myFrame;
      flyThread = new Thread(this);
      
     
      addKeyListener(this);
      addMouseListener(this);
      addMouseMotionListener(this);
   }
   
   public void zoomImage (int realx, int realy, importImage im, mask imagemask) {   
      this.imagemask = imagemask;
      this.im = im;
      
      int zwidth = (int) (this.getWidth()/(2*ratio));
      if(realx < zwidth) {realx = zwidth;}      
      if(realx > im.width - zwidth) {realx = im.width - zwidth;}
      this.realx = realx;
      
      int zheight = (int) (this.getHeight()/(2*ratio));
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
      simagemask.startSmask(getWidth(), getHeight());
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
      km = createImage(getWidth(), getHeight());
      Graphics ga = km.getGraphics();
      
      
      //error correction code...probably shouldn't go here, but this is the only spot right now.
      if(realx < getWidth()/(2*ratio))  {realx = (int) (getWidth()/(2*ratio));}
      if(realy < getHeight()/(2*ratio)) {realy = (int) (getHeight()/(2*ratio));}
      if(realx > im.width - getWidth()/(2*ratio))
         {realx = im.width - (int) (getWidth()/(2*ratio));}
      if(realy > im.height - getHeight()/(2*ratio))
         {realy = im.height - (int) (getHeight()/(2*ratio));}

      //draw zoomed image
      if(im != null) {
         if(best) {
            if(ratio <= 1)   {
               AreaAveragingScaleFilter asf = new AreaAveragingScaleFilter(getWidth(), getHeight());
               Image sz = createImage(new FilteredImageSource(im.eatImage(realx, realy, (int) (getWidth()/ratio),
                                                (int) (getHeight()/ratio)).getSource(), asf));
               ga.drawImage(sz, 0, 0, null);
            }
            else {
               if(ratio <= 3) {
                  AreaAveragingScaleFilter asf = new AreaAveragingScaleFilter(getWidth(), getHeight());
                  Image sz = createImage(new FilteredImageSource(im.eatImage(realx, realy, (int) (getWidth()/ratio),
                                                   (int) (getHeight()/ratio)).getSource(), asf));
                  Image bz = createImage(new FilteredImageSource(sz.getSource(), new BlurFilter()));
                  ga.drawImage(bz, 0, 0, null);
               }
               else {
                  Image bz = im.eatImage(realx, realy, (int) (getWidth()/ratio), (int) (getHeight()/ratio));
                  for(double spazio = ratio; spazio > 1; spazio /= 3) {
                     bz = createImage(new FilteredImageSource(bz.getSource(), new ReplicateScaleFilter(bz.getWidth(null)*3, bz.getHeight(null)*3)));
                     bz = createImage(new FilteredImageSource(bz.getSource(), new BlurFilter()));
                  }
                  bz = createImage(new FilteredImageSource(bz.getSource(), new AreaAveragingScaleFilter(getWidth(), getHeight())));
                  bz = createImage(new FilteredImageSource(bz.getSource(), new BlurFilter()));
                  ga.drawImage(bz, 0, 0, null);
               }
            }
         }
         else {
            ReplicateScaleFilter rsf = new ReplicateScaleFilter(getWidth(), getHeight());
            ga.drawImage(createImage(new FilteredImageSource(im.eatImage(realx, realy, (int) (getWidth()/ratio), 
                                                (int) (getHeight()/ratio)).getSource(), rsf)), 0, 0, null); 
         }
      }
      
   // Draw zoomed mask
      if(imagemask != null)
         ga.drawImage(imagemask.display(realx, realy,
                      (int) (getWidth()/ratio),
                      (int) (getHeight()/ratio)),  
                      0, 0, getWidth(), getHeight(), null);

   // Draw zoomed smask
      if(simagemask != null)
         ga.drawImage(simagemask.display(realx, realy, (int) (getWidth()/ratio), (int) (getHeight()/ratio)),  
                           0, 0, getWidth(), getHeight(), null);
   
   // Draw zoomed targets
      if(targets != null) {
         int sizex;
         int sizey;
         sizex = (int) (getWidth()/ratio);
         sizey = (int) (getHeight()/ratio);
         
         Vector zoomtargets = targets.zoompoints(realx - sizex/2, realy - sizey/2, sizex, sizey);
         
         for(int i=0; i < zoomtargets.size(); i++) {
            pt now = (pt) zoomtargets.elementAt(i);
            if(now.actualized) {
               ga.setColor(Color.red);
               ga.setFont(new Font("SansSerif", Font.BOLD, 12));
               
               int nowx;
               int nowy;
               
               if(ratio < 90) {
                  nowx = (int) ((now.actualx - realx)*ratio);   //relative position
                  nowy = (int) ((now.actualy - realy)*ratio);
               }
               else {
                  nowx = (int) ((now.actualx - realx)*(getWidth()/sizex));   //relative position
                  nowy = (int) ((now.actualy - realy)*(getHeight()/sizey));
               }

               if(sizex/2 == sizex/2.0)      //keeps it centered even when realx is off.
                  nowx += getWidth()/(sizex*2);
               if(sizey/2 == sizey/2.0) 
                  nowy += getHeight()/(sizey*2);

               nowx += getWidth()/2;   //center it
               nowy += getHeight()/2;               
      
               ga.drawOval(nowx - 5, nowy - 5, 10, 10);
               ga.drawString(now.devname, nowx + 7, nowy - 7);   //put some edge conditions in ...
            }
            else {
               ga.setColor(Color.green);
               ga.setFont(new Font("SansSerif", Font.BOLD, 12));
               
               int nowx;
               int nowy;
               
               if(ratio < 90) {
                  nowx = (int) ((now.potentialx - realx)*ratio);   //relative position
                  nowy = (int) ((now.potentialy - realy)*ratio);
               }
               else {
                  nowx = (int) ((now.potentialx - realx)*(getWidth()/sizex));   //relative position
                  nowy = (int) ((now.potentialy - realy)*(getHeight()/sizey));
               }

               if(sizex/2 == sizex/2.0)      //keeps it centered even when realx is off.
                  nowx += getWidth()/(sizex*2);
               if(sizey/2 == sizey/2.0) 
                  nowy += getHeight()/(sizey*2);

               nowx += getWidth()/2;   //center it
               nowy += getHeight()/2;               
               
               ga.drawRect(nowx - 5, nowy - 5, 10, 10);
               ga.drawString(now.devname, nowx + 7, nowy - 7);   //put some edge conditions in ...
            }
         }
      }
      
      g.drawImage(km, 0, 0, null);
      
   // Auto export
      if (auto) {
         if (getSize() != autosize || realx != autorealx || realy != autorealy
             || autoratio != ratio || best != autobest) {
            im.exportImage(autoname + Integer.toString(++autonumber) + ".pgm", km);
         }
         System.out.print("");   // Please don't ask me why this is here. 
                                 // It just needs to be, take my word for it.

         autosize = getSize(); // Remember things so we don't have to do them twice.
         autorealx = realx;
         autorealy = realy;
         autoratio = ratio;
         autobest = best;
      }
      
   // Make the zoomie box glow.
      myFrame.mainCanvas.clearZoom();
      myFrame.mainCanvas.updateZoom(myFrame.getPosition(),
                                    myFrame.getZoomSize().width,
                                    myFrame.getZoomSize().height);      
   }

   //this is for mouse control flying around.
   //because if you don't it only flies when
   //you are actually *moving* the mouse, 
   //which is kind of uncool.

   public synchronized void start_animation() {
      threadSuspended = false;
      if (flyThread.isAlive()) { notify(); }
      else                     { flyThread.start(); }
   }
   
   public synchronized void pause_animation() {
      threadSuspended = true;
   }
   
   public void run() {
      while (true) {
         int foolx = 0;
         int fooly = 0;
         
         fracx += (double)(mousex - flyx)/(ratio*10);
         fracy += (double)(mousey - flyy)/(ratio*10);
         
         if (fracx > 1 || fracx < -1) {
            foolx = (int) fracx;
            fracx = fracx - foolx;
         }
         
         if (fracy > 1 || fracy < -1) {
            fooly = (int) fracy;
            fracy = fracy - fooly;
         }
         
         realx += foolx;
         realy += fooly;

         Graphics g = getGraphics();   
         paint(g);   //if you call repaint() it queues, and bad things happen.

         try {
            Thread.currentThread().sleep(10);

//            if (threadSuspended) {
               synchronized (this) {
                  while (threadSuspended)
                     wait();
                }
//            }
         }
         catch (InterruptedException e){
            System.out.println("SProCKET: Zoom window flythrough failure.");
         }
         repaint();
      }
   }

   public void addLine(int x, int y) {
      Graphics g = this.getGraphics();
      g.drawLine(oldx, oldy, x, y);
   }



   // KEY LISTENER ROUTINES **************************************************
   public void keyReleased (KeyEvent keyEvt) { }
   public void keyTyped    (KeyEvent keyEvt) { }
   public void keyPressed (KeyEvent keyEvt) {
      if (fly) {
         if(!targetting)
            { myFrame.coordinates.setText("ratio: " + ratio); }
         System.out.print("");  // It just needs to be here, okay??

         if (keyEvt.getKeyCode() == KeyEvent.VK_Z) {
            ratio *= .98;   
            repaint();
         }
         if (keyEvt.getKeyCode() == KeyEvent.VK_X) {
            ratio *= 100;
            ratio /= 98;
            repaint();
         }
         if (keyEvt.getKeyCode() == KeyEvent.VK_KP_LEFT) {  // Left arrow
            realx--;
            repaint();
         }
         if (keyEvt.getKeyCode() == KeyEvent.VK_KP_UP) {    // Up arrow
            realy--;
            repaint();
         }
         if (keyEvt.getKeyCode() == KeyEvent.VK_KP_RIGHT) { // Right arrow
            realx++;
            repaint();
         }
         if (keyEvt.getKeyCode() == KeyEvent.VK_KP_DOWN) {  // Down arrow
            realy++;
            repaint();
         }
      }
   }

   // MOUSE INPUT LISTENER ROUTINES ********************************************
   public void mouseClicked(MouseEvent mouseEvt) { }
   public void mouseEntered(MouseEvent mouseEvt) { }
   public void mouseExited (MouseEvent mouseEvt) { }
   public void mouseMoved  (MouseEvent mouseEvt) { }
   public synchronized void mousePressed(MouseEvent mouseEvt) {
   // Fly through
      if (fly) {
         if(!targetting)
            { myFrame.coordinates.setText("ratio: " + ratio); }
         System.out.print("");  // It just needs to be here, okay??
         mousex = mouseEvt.getX();
         mousey = mouseEvt.getY();
         flyx = mouseEvt.getX();
         flyy = mouseEvt.getY();
         mouseEvt.consume();
         start_animation();
         if(!targetting)
            return;
      }
      
   // Smasking
      if (nowsmasking) {
         myFrame.coordinates.setText("Smasking");
         simagemask.startSmask((int)(getWidth()/ratio) + 1,
                               (int)(getHeight()/ratio) + 1);
         startx = mouseEvt.getX();
         starty = mouseEvt.getY();
         oldx = mouseEvt.getX();
         oldy = mouseEvt.getY();
      }

   // Bright
      if(targetting && bright) {
//         Dimension d = im.brightest((int)(mouseEvt.getX()/ratio),
//                                    (int)(mouseEvt.getY()/ratio),
//                                    x, y, range);
         Dimension d = im.brightest((int)(mouseEvt.getX()/ratio),
                                    (int)(mouseEvt.getY()/ratio),
                                    realx, realy, range);

         if (d==null) {return;} // Just in case...
         int evtx = (int)(d.width * getWidth()
                        / (int)(getWidth() /ratio));
         int evty = (int)(d.height * getHeight()
                        / (int)(getHeight()/ratio));

         targetx = realx;      
         targety = realy;
         targetx -= (int) (getWidth()  / (2*ratio));   
         targety -= (int) (getHeight() / (2*ratio));
         targetx += (d.width);
         targety += (d.height);

         if ((memtarx != targetx || memtary != targety)
             && (memtarx != -1 || memtary != -1))
            { repaint(); }

         Graphics g = this.getGraphics();
         g.setColor(Color.yellow);
         int xratio = getWidth() /(int)(getWidth() /ratio);
         int yratio = getHeight()/(int)(getHeight()/ratio);
         if(xratio < 2) {xratio = 2;}
         if(yratio < 2) {yratio = 2;}
         g.fillRect(evtx, evty, xratio, yratio);

         myFrame.coordinates.setText("x: " + targetx + " " + "y: " + targety
                                     + " " + "ratio: " + ratio);
      }

   // Not flying, not smasking, and not bright...
      targetx = realx;      
      targety = realy;
      targetx -= (int) (getWidth()/(2*ratio));      //to put x in the middle of the zwidth*zheight block.
      targety -= (int) (getHeight()/(2*ratio));
      targetx += (int) (mouseEvt.getX()/ratio);
      targety += (int) (mouseEvt.getY()/ratio);
      myFrame.coordinates.setText("x: " + targetx + " " + "y: " + targety
                                  + " " + "ratio: " + ratio);
   }

   public synchronized void mouseReleased(MouseEvent mouseEvt) {
   // Fly through
      if (fly) {
         fracx = 0;
         fracy = 0;
         mouseEvt.consume();
         pause_animation();
      }
      
   // Smasking
      if (nowsmasking) {
         int tempMouseX = mouseEvt.getX();
         int tempMouseY = mouseEvt.getY();

         if (tempMouseX >= simagemask.zwidth*ratio)
            tempMouseX = (int) ((simagemask.zwidth-1)*ratio);
         if (tempMouseX < 0)
            tempMouseX = 0;

         if (tempMouseY >= simagemask.zheight*ratio)
            tempMouseY = (int) ((simagemask.zheight-1)*ratio);
         if (tempMouseY < 0)
            tempMouseY = 0;

         if (memFLAG == true) {
            simagemask.addLine((int)(memx/ratio), (int)(memy/ratio),
                               (int)(oldx/ratio), (int)(oldy/ratio));
         }
         simagemask.addLine((int)(tempMouseX/ratio),
                            (int)(tempMouseY/ratio),
                            (int)(startx/ratio), (int) (starty/ratio));
         oldx = tempMouseX;
         oldy = tempMouseY;
         addLine(startx, starty);
//         simagemask.endSmask(x, y);
         simagemask.endSmask(realx, realy);
         nowsmasking = false;
         memFLAG = false;
         myFrame.setResizable(true);
         myFrame.ratioMenuEnabler(true);
         myFrame.flyMenuItem.setEnabled(true);
         myFrame.mainCanvas.updateSmask(simagemask);
         setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
         repaint();
      }

   // Bright
      if (targetting && bright) {
         memtarx = -1;
         memtary = -1;
         repaint();
      }
   }

   public void mouseDragged(MouseEvent mouseEvt) {
   // Fly through
      if (fly) {
         mousex = mouseEvt.getX();
         mousey = mouseEvt.getY();
      }

   // Smasking
      if (nowsmasking) {
         int tempMouseX = mouseEvt.getX();
         int tempMouseY = mouseEvt.getY();

         if(tempMouseX >= simagemask.zwidth*ratio)
            tempMouseX = (int) ((simagemask.zwidth-1)*ratio);
         if(tempMouseX < 0)
            tempMouseX = 0;

         if(tempMouseY >= simagemask.zheight*ratio)
            tempMouseY = (int) ((simagemask.zheight-1)*ratio);
         if(tempMouseY < 0)
            tempMouseY = 0;

         if (oldy != mouseEvt.getY()) {
            if(memFLAG == true) {
               simagemask.addLine((int)(memx/ratio), (int)(memy/ratio),
                                  (int)(oldx/ratio), (int)(oldy/ratio));
               memFLAG = false;
            }
            simagemask.addLine((int)(oldx/ratio), (int)(oldy/ratio),
                               (int)(tempMouseX/ratio),
                               (int)(tempMouseY/ratio));
         }
         else if (memFLAG == false) {
            memx = mouseEvt.getX();
            memy = mouseEvt.getY();
            memFLAG = true;
         }   
         addLine(mouseEvt.getX(), mouseEvt.getY());
         oldx = tempMouseX;
         oldy = tempMouseY;
      }
   }
}

