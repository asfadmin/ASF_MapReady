/* ****************************************************************************
NAME: imageCanvas.java

DESCRIPTION:
   

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


//MAIN CANVAS

class imageCanvas extends Canvas implements MouseListener, MouseMotionListener {
   pvs mainFrame;
   importImage im;
   mask imagemask;
   smask simagemask;
   pointtargets targets;      //add a pointer to main window, or zoomFrame...
   int pensize = 0;
   Image pm;
   Image mm;
   int x = -1;
   int y = -1;
   Dimension position = new Dimension(0,0); //these are for the glowing green box
   int zwidth  = 1;
   int zheight = 1;

   imageCanvas(pvs mainFrame) {
      addMouseListener(this);
      addMouseMotionListener(this);
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
      this.imagemask = imagemask;
      if(this.pensize != pensize) { // Just redraw the square the mask pen is on
         this.pensize = pensize;    //  when the user clicks, since alpha is slow
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
               ga.drawString(now.devname, nowx + 7, nowy - 7);   //put some edge conditions in ...
            }
            else {
               ga.setColor(Color.green);
               ga.setFont(new Font("SansSerif", Font.BOLD, 12));
               
               int nowx = now.potentialx * im.imagesize;
               nowx /= im.width;
               int nowy = now.potentialy * im.imagesize;
               nowy /= im.height;
               
               ga.drawRect(nowx - 5, nowy - 5, 10, 10);
               ga.drawString(now.devname, nowx + 7, nowy - 7);   //put some edge conditions in ...
            }
         }
      }
      
      if(mm != null)
         g.drawImage(mm, 0, 0, null);

      //make sure the glowing box is there.
      if (im instanceof importImage && mainFrame.zf instanceof zoomFrame
          && mainFrame.zf.isVisible()) {
         clearZoom();
         updateZoom(mainFrame.zf.getPosition(), 
                    mainFrame.zf.getZoomWidth(), 
                    mainFrame.zf.getZoomHeight());
      }
   }

   public void printit(Frame parent, Properties printprefs) {      //wait for java1.2 . . . 
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
   
   // MOUSE LISTENER ROUTINES **************************************************
   public void mouseClicked (MouseEvent mouseEvt) { }
   public void mouseEntered (MouseEvent mouseEvt) { }
   public void mouseExited  (MouseEvent mouseEvt) { }
   public void mousePressed (MouseEvent mouseEvt) {
      x = mouseEvt.getX();
      y = mouseEvt.getY();
   }
   public void mouseReleased(MouseEvent mouseEvt) { }
   
   // MOUSE MOTION LISTENER ROUTINES *******************************************
   public void mouseMoved (MouseEvent mouseEvt) { }
   public void mouseDragged (MouseEvent mouseEvt) {
      x = mouseEvt.getX();
      y = mouseEvt.getY();
   }
}

