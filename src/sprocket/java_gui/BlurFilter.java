
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//this came from the java AWT reference book.

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

