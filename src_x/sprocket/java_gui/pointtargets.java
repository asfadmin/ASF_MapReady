
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


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

