
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


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

				if ( (imagequality==0) 
				  || ( (int)((i+imagequality)/hratio)>oldi || i>(height-imagequality) ) ) {
					from.read(section);
					for(int j=0; j < width; j++) {
						countarray[(int) (j/wratio)]++;
						int part1 = section[4*j];
						int part2 = section[4*j+1];
						int part3 = section[4*j+2];
						int part4 = section[4*j+3];
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
				else {
					from.skipBytes(width*4);
				}
				oldi = (int) (i/hratio);
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

		//check to see if we really need to refresh.
		if((memx != realx) || (memy != realy) || newcontrast || targetting) {
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
