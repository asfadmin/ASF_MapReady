
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


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
