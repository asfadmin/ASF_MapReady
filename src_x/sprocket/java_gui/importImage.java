/* *****************************************************************************
NAME: importImage.java

DESCRIPTION:
   Reads the image for sprocket. Can read either asf CEOS or sprocket data.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
            07/03  P. Denny     Split bloated pvs.java up into
                                 files named after their classes
            07/03  P. Denny     Replaced depricated action and handleEvent
                                 methods with appropriate Listeners
            08/03  P. Denny     Read .img (32-bit big endian float) instead of
                                 .data1 (32-bit host endian int). Done for both
                                 main canvas and zoom canvas.

***************************************************************************** */

import java.awt.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;

//IMAGE

class importImage {
   Image om;
   Image pm;
   Image im;
   Image zm;
   int memx = 0;   //stores realx
   int memy = 0;   //stores realy
   int width = 0;
   int height = 0;
   int zwidth;
   int zheight;
   int imagesize;
   int imagequality;
   int asfpadding = 192;
   int contrastLow = 0;
   int contrastHigh = 55;
   int defaultContrastLow = 0;
   int defaultContrastHigh = 55;
   float fullmax;
   float fullmin;
   pvs mainFrame;
   String fileName;
   String baseFileName;
   String metaFileName;
   RandomAccessFile raf;
   boolean newcontrast;
   boolean rafFLAG = false;
   byte[] greyarray = new byte[256];
   byte[] byteImageArray;
   IndexColorModel cm;

// Sends the file to the correct reader based on it's name.
   importImage (pvs _mainFrame, String _filename, int _imagesize,
                int _imagequality) {
      mainFrame = _mainFrame;
      imagesize = _imagesize;
      imagequality = _imagequality;

   // Grab the correct files
      if (_filename.endsWith(".metadata")) {
         baseFileName = _filename.substring(0,_filename.length() - 9);
         metaFileName = baseFileName + ".metadata";
         fileName = baseFileName + ".img";
         readpvsImage();
      }
      else if (_filename.endsWith(".img")) {
         baseFileName = _filename.substring(0,_filename.length() - 4);
         metaFileName = baseFileName + ".metadata";
         fileName = baseFileName + ".img";
         readpvsImage();
      }
      else if (_filename.endsWith(".D")) {
         baseFileName = _filename.substring(0,_filename.length() - 2);
         metaFileName = baseFileName + ".L";
         fileName = baseFileName + ".D";
         readasfImage();
      }
      else if (_filename.endsWith(".L")) {
         baseFileName = _filename.substring(0,_filename.length() - 2);
         metaFileName = baseFileName + ".L";
         fileName = baseFileName + ".D";
         readasfImage();
      }
      else {  //put a dialog here, or something...
         logger.log("Improper fileName format.");
         return;
      }
   }

// Read pvs image files ********************************************************
   public void readpvsImage() {

   // Parse metadata
      try {
         FileReader fr = new FileReader(metaFileName);
         LineNumberReader in = new LineNumberReader(fr);

         boolean numpixels = false;
         while (!numpixels) {
            StringTokenizer s = new StringTokenizer(in.readLine());
            while (s.hasMoreTokens())
               if (s.nextToken().equalsIgnoreCase("number_of_pixels")) {
                  String equalsign = s.nextToken();  //this takes care of the equals sign.
                  String quotednumber = s.nextToken();
                  String number = quotednumber.substring(1,quotednumber.length()-1);
                  width = Integer.parseInt(number);
                  numpixels = true;
                  break;
               }
         }
         boolean numlines = false;
         while (!numlines) {
            StringTokenizer s = new StringTokenizer(in.readLine());
            while (s.hasMoreTokens())
               if (s.nextToken().equalsIgnoreCase("number_of_lines")) {
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
      catch (IOException e) {
         logger.log("Error parsing the metadata.");
      }

   // Now we've got the metadata we need; lets do some image stuff!
      if (imagequality >= height/imagesize)
         imagequality = 0;

   // Process meter stuff
      procDialog pd = new procDialog(mainFrame, "Opening image...");
      int locatex = mainFrame.getX();
      int locatey = mainFrame.getY();
      if (locatex > 0 && locatey > 0)
         pd.setLocation(locatex+3, locatey+41);
      pd.setVisible(true);
      int done = 0;

   // Read & decode image data file
      try {
         FileInputStream fis = new FileInputStream(fileName);
         BufferedInputStream bis = new BufferedInputStream(fis);
         DataInputStream from = new DataInputStream(bis);
         double widthRatio = (double)width/(double)imagesize;
         double heightRatio = (double)height/(double)imagesize;
         byte[] byteLine = new byte[width*4];
         float[] floatLine = new float[width];
         float[] imageLineInFloats = new float[imagesize];
         float[] imageInFloats = new float[imagesize*imagesize];
         int[] countArray = new int[imagesize];
         int[] histogram = new int[256];
         int oldii = 0;
         float max = Float.MIN_VALUE;
         float min = Float.MAX_VALUE;

      // Actual reading part
         for (int ii=0; ii<height; ii++) {

         // Process meter stuff
            if ((ii*100)/height > done) {
               done = (ii*100)/height;
               pd.process(done);
            }
         // Read line of samples (pixels) into an array (or skip the line)
            if ((imagequality==0) || (int)((ii+imagequality)/heightRatio)>oldii
                                                  || ii>(height-imagequality)) {
            // Get the data line in bytes
               from.read(byteLine);
               float fVal;
               for (int jj=0; jj<width; jj++) {
               // Convert the bytes to floats
                  fVal = Float.intBitsToFloat(((byteLine[jj*4  ]&0xff)<<24)
                                            | ((byteLine[jj*4+1]&0xff)<<16)
                                            | ((byteLine[jj*4+2]&0xff)<<8)
                                            |  (byteLine[jj*4+3]&0xff));
               // Add all the data values into image bins
                  imageLineInFloats[(int)(jj/widthRatio)] += fVal;
               // Keep track of how many values went into each bin for averaging
                  countArray[(int)(jj/widthRatio)]++;
               }
            }
            else {
               from.skipBytes(width*4);
            }
         // Fill the image with floats and get max & min as we process lines
            if ((int)(ii/heightRatio) > oldii || ii == height-1) {
               int index;
               for (int jj=0; jj<imagesize; jj++) {
                  index = oldii*imagesize+jj;
                  imageInFloats[index] = imageLineInFloats[jj]/countArray[jj];
                  if (max<imageInFloats[index])   max = imageInFloats[index];
                  if (min>imageInFloats[index])   min = imageInFloats[index];
               }
               imageLineInFloats = new float[imagesize];
               countArray = new int[imagesize];
            }
            oldii = (int)(ii/heightRatio);
         }
         from.close();   // Close data file stream
         pd.dispose();   // Destroy process meter

      // Done reading, figure statistics now
         fullmax = max;
         fullmin = min;
         int stddev = 0;
         int mean = 0;
         double sum = 0;
         byteImageArray = new byte[imagesize*imagesize];

      // Fit freshly read binary data to byte range & get some stats
         if (max-min != 0) {
            int fit2byte;
            for (int ii=0; ii < imagesize*imagesize; ii++) {
               fit2byte = (int)(255 * (imageInFloats[ii]-min) / (max-min));
               byteImageArray[ii] = (byte)fit2byte;
               histogram[fit2byte]++;
               mean += fit2byte;
            }
         }
         else {
            for (int ii=0; ii < imagesize*imagesize; ii++) {
               byteImageArray[ii] = (byte) 0;
               histogram[0]++;
            }
         }

      // Get mean & standard deviation (to set default constrast levels)
         mean = (int) (mean/(imagesize*imagesize));
         for (int ii=0; ii<256; ii++) {
            sum += histogram[ii] * (ii-mean) * (ii-mean);
         }
         stddev = (int) java.lang.Math.sqrt(sum/(imagesize*imagesize));

      // Set image contrast values (to make it pretty)
         defaultContrastLow = mean - (2*stddev);
         defaultContrastHigh = mean + (2*stddev);
         if (defaultContrastLow < 0)
            defaultContrastLow = 0;
         if (defaultContrastLow > 255)
            defaultContrastLow = 255;
         if (defaultContrastHigh < 0)
            defaultContrastHigh = 0;
         if (defaultContrastHigh > 255)
            defaultContrastHigh = 255;
         contrastLow = defaultContrastLow;
         contrastHigh = defaultContrastHigh;

         colourarray(contrastLow, contrastHigh);
      }
      catch(Exception e) {
      // Create a fake fuzzy image and let the user know things got hosed
         byteImageArray = new byte[imagesize*imagesize];
         Random ran = new Random();
         ran.nextBytes(byteImageArray);
         logger.log("Error creating image... exception: " + e.toString());
         pd.dispose(); // Blow away process meter
      }

   // Everything is prepared, let java draw the image
      MemoryImageSource mis = new MemoryImageSource(imagesize, imagesize, cm,
                                                    byteImageArray, 0,
                                                    imagesize);
      im = Toolkit.getDefaultToolkit().createImage(mis);
   }

// Read ASF image files ********************************************************
   public void readasfImage() {
      byte cwidth;
      byte cheight;
      int array[];
      long max = 0;
      long min = 2147483647;

   // Parse out metadata (.L file)
      try {
         FileInputStream metaread = new FileInputStream(metaFileName);
         metaread.skip(27436);
         for (int i = 100000000; i >= 1; i /= 10) {
            cwidth = (byte) metaread.read();
            if (48 <= cwidth && cwidth <= 57) {
               width += i*(cwidth-48);
            }
         }
         for (int i = 100000000; i >= 1; i /= 10) {
            cheight = (byte) metaread.read();
            if (48 <= cheight && cheight <= 57) {
               height += i*(cheight-48);
            }
         }
         metaread.close();
      }
      catch (IOException e){
         logger.log("Error parsing the metadata.");
      }

   // Now we've got what we needed.
      if (imagequality >= height/imagesize)
         imagequality = 0;
      double fwidth = width;
      double fheight = height;
      double wratio = fwidth/imagesize;
      double heightRatio = fheight/imagesize;
      long[] longArray = new long[imagesize];
      int[] countArray = new int[imagesize];
      int[] histogram = new int[256];
      int oldi = 0;

   // Process meter stuff
      procDialog pd = new procDialog(mainFrame, "opening image...");
      int locatex = mainFrame.getX();
      int locatey = mainFrame.getY();
      if (locatex > 0 && locatey > 0)
         pd.setLocation(locatex + 3, locatey + 41);
      pd.setVisible(true);
      int done = 0;

      try {
         FileInputStream fis = new FileInputStream(fileName);
         BufferedInputStream bis = new BufferedInputStream(fis);
         DataInputStream from = new DataInputStream(bis);

         byte[] section = new byte[width + asfpadding];
         from.read(section);      //get rid of junk row.

         array = new int[imagesize*imagesize];

         if (imagequality == 0) {
            for (int i=0; i < height; i++) {
               //process meter stuff
               if ((i*100)/height > done) {
                  done = (i*100)/height;
                  pd.process(done);
               }

               if ((int) (i/heightRatio) > oldi || i == height-1) {
                  for (int j=0; j < imagesize; j++) {
                     array[oldi*imagesize+j] = (int) (longArray[j]/countArray[j]);
                     if (max * countArray[j] < longArray[j])
                        { max = longArray[j]/countArray[j]; }
                     if (min  * countArray[j] > longArray[j])
                        { min  = longArray[j]/countArray[j]; }
                  }
                  longArray = new long[imagesize];
                  countArray = new int[imagesize];
               }

               from.read(section);
               for (int j=0; j < width; j++) {
                  countArray[(int) (j/wratio)]++;
                  longArray[(int) (j/wratio)] += section[j + asfpadding];
               }

               oldi = (int) (i/heightRatio);
            }
         }
         else {
            for (int i=0; i < height; i++) {
               //process meter stuff
               if ((i*100)/height > done) {
                  done = (i*100)/height;
                  pd.process(done);
               }

               if ((int) (i/heightRatio) > oldi || i == height-1) {
                  for (int j=0; j < imagesize; j++) {
                     array[oldi*imagesize+j] = (int) (longArray[j]/countArray[j]);
                     if (max * countArray[j] < longArray[j])
                        { max = longArray[j]/countArray[j]; }
                     if (min  * countArray[j] > longArray[j])
                        { min  = longArray[j]/countArray[j]; }
                  }
                  longArray = new long[imagesize];
                  countArray = new int[imagesize];
               }

               if ((int) ((i+imagequality)/heightRatio) > oldi || i > height - imagequality) {
                  from.read(section);
                  for (int j=0; j < width; j++) {
                     countArray[(int) (j/wratio)]++;
                     longArray[(int) (j/wratio)] += section[j + asfpadding];
                  }
               }
               else
                  from.skipBytes(width + asfpadding);

               oldi = (int) (i/heightRatio);
            }
         }

         pd.dispose();      //process meter stuff

         fullmax = (float)max;
         fullmin = (float)min;
         from.close();
         int stddev = 0;
         int mean = 0;
         double sum = 0;
         byteImageArray = new byte[imagesize*imagesize];

         if (max-min != 0)      //avoid division by zero errors.
            for (int i=0; i < imagesize*imagesize; i++) {
               if (array[i] < min) {array[i] = (int) min;}
               byteImageArray[i] = (byte) (255*(array[i] - min)/(max-min));
               histogram[(int) (255*(array[i] - min)/(max-min))]++;
               mean += (int) (255*(array[i] - min)/(max-min));
            }
         else
            for (int i=0; i < imagesize*imagesize; i++) {
               byteImageArray[i] = (byte) 0;
               histogram[0]++;
            }

         mean = (int) (mean/(imagesize*imagesize));
         for (int i=0; i < 256; i++) {
            sum += histogram[i]*(i - mean)*(i-mean);
         }
         stddev = (int) java.lang.Math.sqrt(sum/(imagesize*imagesize));

         defaultContrastLow = mean - (2*stddev);
         defaultContrastHigh = mean + (2*stddev);

         if (defaultContrastLow < 0)
            defaultContrastLow = 0;
         if (defaultContrastLow > 255)
            defaultContrastLow = 255;
         if (defaultContrastHigh < 0)
            defaultContrastHigh = 0;
         if (defaultContrastHigh > 255)
            defaultContrastHigh = 255;

         contrastLow = defaultContrastLow;
         contrastHigh = defaultContrastHigh;

         colourarray(contrastLow, contrastHigh);

      }
      catch(Exception e) {
         byteImageArray = new byte[imagesize*imagesize];  //eck. do something nice here.
         Random ran = new Random();
         ran.nextBytes(byteImageArray);
         logger.log("error creating image.");
      }

      im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize,
               cm, byteImageArray, 0, imagesize));
   }

// *****************************************************************************
   public Image eatImage() {
      return im;
   }

// Called by updateMask ********************************************************
   public Image eatImage(int x, int y, int pensize) {
      int starter = (y-pensize)*imagesize + (x-pensize);
      int xsize = pensize*2;
      int ysize = pensize*2;

   // eh, some junk for boundary conditions.
      if (starter < 0) {starter = x - pensize;}
      if (starter < 0) {starter = 0;}
      if (y - pensize < 0) {ysize += y - pensize;}
      if (y + pensize >= imagesize) {ysize = imagesize + pensize - y;}
      if (x - pensize < 0) {xsize += x - pensize; if (starter > 0) {starter -= x-pensize;}}
      if (x + pensize >= imagesize) {xsize = imagesize + pensize - x;}
      if ((ysize > 0) && (xsize > 0))
         return Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(xsize, ysize,
                    cm, byteImageArray, starter, imagesize));
      else
         return Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(0, 0,
                    cm, byteImageArray, starter, imagesize));   //idiot value
   }

// Called by updateMask ********************************************************
   public Image eatImage(Dimension position, int width, int height) {
      int x = position.width;
      int y = position.height;
      int starter = (y-height)*imagesize + (x-width);
      int xsize = width*2;
      int ysize = height*2;

   // eh, some junk for boundary conditions.
      if (starter < 0) {starter = x - width;}
      if (starter < 0) {starter = 0;}
      if (y - height < 0) {ysize += y - height;}
      if (y + height >= imagesize) {ysize = imagesize + height - y;}
      if (x - width < 0) {xsize += x - width; if (starter > 0) {starter -= x-width;}}
      if (x + width >= imagesize) {xsize = imagesize + width - x;}

      Image i;

      if ((ysize > 0) && (xsize > 0))
         i = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(xsize, ysize,
                    cm, byteImageArray, starter, imagesize));
      else
         i = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(0, 0,
                    cm, byteImageArray, starter, imagesize));   //idiot value

      return i;
   }

// *****************************************************************************
   public Image eatImage(int realx, int realy, int zwidth, int zheight) {
      realx -= zwidth/2;    // Put x in the middle of the zwidth*zheight block
      realy -= zheight/2;
      boolean targetting = mainFrame.zf.zoomedCanvas.targetting;

      //check to see if we really need to refresh.
      if ((memx != realx) || (memy != realy) || newcontrast || targetting) {
         if (fileName.endsWith("img")) {
            newpvszoom(realx, realy, zwidth, zheight);
         }
         else if (fileName.endsWith("D")) {
            newasfzoom(realx, realy, zwidth, zheight);
         }
      }

      newcontrast = false;
      this.zwidth = zwidth;
      this.zheight = zheight;

      return zm;
   }

// *****************************************************************************
   protected void newpvszoom(int realx, int realy, int zwidth, int zheight) {
      boolean firstFLAG = false;
      int first = (width*realy + realx) * 4;
      if (first < 0)  {
         firstFLAG = true;
         if (realx < 0) {first = 0;}
         else {first = realx * 4;}
      }
      if (first > 4*width*height) {first = 0;} //should do something nicer here. . .
      int now = first;

      if (!rafFLAG) {
         if (fileName != null) {
            try {
               raf = new RandomAccessFile(fileName, "r");
               rafFLAG = true;
            }
            catch(Exception e) {
               logger.log("Error opening file: " + fileName);
            }
         }
      }

      float[] zoomArray = new float[zwidth * zheight];     //zoom image array
      byte[] zoomImageArray = new byte[zwidth * zheight];
      if (fileName != null) {
         try {
            int start = 0;         // Only needs to be done once
            int end = zwidth;
            if (realx < 0) {start = 0 - realx;}
            if (realx > width-zwidth) {end = width - realx;}
            int sizer = end - start;
            int index;

            raf.seek(first);       // Seeks in bytes...four byte float.
            if (sizer > 0) {
               byte[] newline = new byte[sizer*4];
               for (int ii=0; ii<zheight; ii++) {
                  if ((realy+ii < 0) || (realy+ii >= height)) {
                     /*nuthin*/;
                  }
                  else {
                     if (!firstFLAG) {
                        raf.skipBytes(start*4);
                     }
                     raf.read(newline);
                  // Convert every 4 bytes to 1 float
                     for (int jj=0; jj<sizer; jj++) {
                        index = ii*zwidth + jj + start;
                        zoomArray[index] = Float.intBitsToFloat(
                                                   ((newline[jj*4  ]&0xff)<<24)
                                                 | ((newline[jj*4+1]&0xff)<<16)
                                                 | ((newline[jj*4+2]&0xff)<<8)
                                                 |  (newline[jj*4+3]&0xff));
                     }
                     now += width*4;
                     raf.seek(now);
                  }
               }
               float divisor = fullmax-fullmin;      //test version;
               for (int ii=0; ii < zwidth*zheight; ii++) {
                  if (zoomArray[ii] > fullmax) {zoomArray[ii] = fullmax;}
                  zoomImageArray[ii] = (byte) ((255*(zoomArray[ii]-fullmin))/divisor);
               }
            }
         }
         catch(Exception e) {
            logger.log("error creating zoomed (full-res) image.");
         }
      }

      zm = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(zwidth, zheight,
              cm, zoomImageArray, 0, zwidth));

      memx = realx;
      memy = realy;
   }

// *****************************************************************************
   protected void newasfzoom(int realx, int realy, int zwidth, int zheight) {
      boolean firstFLAG = false;
      int first = ((width + asfpadding)*realy + realx);
      if (first < 0)  {
         firstFLAG = true;
         if (realx < 0) {first = 0;}
         else {first = realx;}
      }
      if (first > (width + asfpadding)*height) {first = asfpadding;} //should do something nicer here. . .
      first += (width + asfpadding);
      int now = first;

      if (!rafFLAG) {
         if (fileName != null) {
            try {
               raf = new RandomAccessFile(fileName, "r");
               rafFLAG = true;
            }
            catch(Exception e) {
               logger.log("error opening file: " + fileName);
            }
         }
      }

      long[] zoomarray = new long[zwidth * zheight]; //zoom window image array
      byte[] zoomImageArray = new byte[zwidth * zheight];
      if (fileName != null) {
         try {
            raf.seek(first);       //seeks in bytes...four byte integer.
            int start = 0;         //only needs to be done once
            int end = zwidth;
            if (realx < 0) {start = 0 - realx;}
            if (realx > width - zwidth) {end = width - realx;}
            int sizer = end - start;
            int index;

            if (sizer > 0) {
               byte[] newline = new byte[sizer];
               for (int i=0; i < zheight; i++) {
                  if ((realy+i < 0) || (realy+i >= height)) {;}
                  else {
                     if (!firstFLAG)
                        raf.skipBytes(start+asfpadding);
                     raf.read(newline);
                     for (int j = 0; j < sizer; j++) {
                        index = i*zwidth + j + start;
                        if (newline[j] < 0)
                           zoomarray[index] =   (256 + newline[j]);
                        else
                           zoomarray[index] =   newline[j];
                     }
                     now += (width + asfpadding);
                     raf.seek(now);
                  }
               }
               float divisor = (fullmax-fullmin);      //test version;
               for (int i=0; i < zwidth * zheight; i++) {
                  if (zoomarray[i] > (long)fullmax) {zoomarray[i] = (long)fullmax;}
                  zoomImageArray[i] = (byte) ((float)(255*(zoomarray[i] - fullmin))/divisor);
               }
            }
         }
         catch(Exception e) {
            logger.log("error creating zoomed (full-res) image.");
         }
      }

      zm = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(zwidth, zheight,
              cm, zoomImageArray, 0, zwidth));

      memx = realx;
      memy = realy;
   }

// *****************************************************************************
   public void recolour() {
      im = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(imagesize, imagesize,
               cm, byteImageArray, 0, imagesize));
   }

// *****************************************************************************
   public Dimension brightest(int evtx, int evty, int realx, int realy, int range) {   //returns the brightest value within range pixels
//      boolean firstFLAG;
      long charlie = 0;
      long max = Long.MIN_VALUE;
      Dimension etarget = null;

      realx -= zwidth/2;      //to put x in the middle of the zwidth*zheight block.
      realy -= zheight/2;
//      x--;
//      y--;

      int ymin = evty - range;
      if (ymin < 0) {ymin = 0;}
      int xmin = evtx - range;
      if (xmin < 0) {xmin = 0;}
      int ymax = evty + range;
      if (ymax >= zheight) {ymax = zheight - 1;}
      int xmax = evtx + range;
      if (xmax >= zwidth) {xmax = zwidth - 1;}

      if (xmax - xmin < 1) {return new Dimension(-1,-1);}
      if (ymax - ymin < 1) {return new Dimension(-1,-1);}

      int first = (width*realy + realx) * 4;
      if (first < 0)  {
//         firstFLAG = true;
         if (realx < 0) {first = 0;}
         else {first = realx * 4;}
      }
      first += width * ymin * 4;
      first += xmin * 4;

      byte[] newline = new byte[(xmax-xmin) * 4];   //yep, it's the four byte thing again.

      if (!rafFLAG) {
         if (fileName != null) {
            try {
               System.out.println("grrr.");
               raf = new RandomAccessFile(fileName, "r");
               rafFLAG = true;
            }
            catch(Exception e) {
               logger.log("error opening file: " + fileName + " for brightest.");
            }
         }
      }

      try {
         //okay, get the proper starting place
         //read in one line
         //convert to int's
         //save the position of the maxest
         //skip the right amount
         //do it over.

         raf.seek(first);
         int now = first;

         for (int i=ymin; i < ymax; i++) {
         //   if (!firstFLAG)
         //      raf.skipBytes(start*4);         //what?! why?
            raf.read(newline);

            for (int j=0; j < (xmax - xmin); j++) {

               if (newline[j*4] < 0)
                  charlie =   (256 + newline[j*4]) * 16777216;
               else
                  charlie =   newline[j*4] * 16777216;

               if (newline[j*4+1] < 0)
                  charlie +=   (256 + newline[j*4+1]) * 65536;
               else
                  charlie +=   newline[j*4+1] * 65536;

               if (newline[j*4+2] < 0)
                  charlie +=   (256 + newline[j*4+2]) * 256;
               else
                  charlie +=   newline[j*4+2] * 256;

               if (newline[j*4+3] < 0)
                  charlie +=   (256 + newline[j*4+3]);
               else
                  charlie +=   newline[j*4+3];

               if (charlie > max) {
                  max = charlie;
                  etarget = new Dimension(j+xmin, i);
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

// *****************************************************************************
   public void exportImage(String fileName, Image km) {
      int kwidth  = km.getWidth(null);
      int kheight = km.getHeight(null);
      int[] pixelarray = new int[kwidth*kheight];
      byte[] bytearray = new byte[kwidth*kheight];
      PixelGrabber pg = new PixelGrabber(km, 0, 0, kwidth, kheight, pixelarray, 0, kwidth);
//      PixelGrabber pg = new PixelGrabber(zm, 0, 0, zwidth, zheight, false);

      try {
         pg.grabPixels();
         for (int i = 0; i < kwidth*kheight; i++)
            bytearray[i] = (byte) (pixelarray[i] & 0x000000FF);
//         bytearray = (byte[]) pg.getPixels();

         FileWriter out = new FileWriter(fileName);

         out.write("P5 ");
         out.write(kwidth + " " + kheight + " " + "255\n");

         out.flush();
         out.close();


         FileOutputStream fos = new FileOutputStream(fileName, true);
         BufferedOutputStream to = new BufferedOutputStream(fos);

         to.write(bytearray);

         to.flush();
         to.close();
      }
      catch(Exception e) {
         logger.log("error exporting image.");
      }
   }

// *****************************************************************************
   public void colourarray(int min, int max) {
      if (min < 0)
         min = 0;
      if (min > 255)
         min = 255;
      if (max < 0)
         max = 0;
      if (max > 255)
         max = 255;

      contrastLow = min;
      contrastHigh = max;

      if (max-min != 0) {      //yeah, it's wanky. but it prevents div by zero errors.
         double ratio = 255;
         ratio /= max - min;
         for (int i = 0; i < 256; i++) {
            if (i < min)
               greyarray[i] = (byte) 0;
            else if (i > max)
               greyarray[i] = (byte) 255;
            else
               greyarray[i] = (byte) ((i - min) * ratio);
         }
      }
      else {
         for (int i = 0; i < 256; i++) {
            if (i < min)
               greyarray[i] = (byte) 0;
            else if (i > max)
               greyarray[i] = (byte) 255;
            else
               greyarray[i] = (byte) min;
         }
      }

      cm = new IndexColorModel(8, 256, greyarray, greyarray, greyarray);
   }

// *****************************************************************************
   public boolean closeImage() {
      if (raf != null) {
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
