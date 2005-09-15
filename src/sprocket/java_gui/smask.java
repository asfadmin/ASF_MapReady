
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;




class spanpair {
	int go;		//beginning of span
	int stop;		//end of span
	
	spanpair (int go, int stop) {
		this.go = go;
		this.stop = stop;
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

