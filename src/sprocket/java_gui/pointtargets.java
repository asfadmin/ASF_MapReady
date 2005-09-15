
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;


//POINT TARGETS

class pointtargets {
   String fileName;
   private Vector alltargets;
   public Vector visibletargets;
   private Vector trgs;


   pointtargets(String _basefilename) {
      fileName = _basefilename;
      int numpts = 0;
      try {
         FileInputStream in = new FileInputStream(
                           new File (fileName + Constants.ground_target_file));
         TargetIngester t = new TargetIngester(in);
         Vector v = t.getElements();

         trgs = v;

         //Save Targets
         alltargets = new Vector();

         for (int ii=0; ii < v.size(); ii++ ) {
            Target T = (Target)v.elementAt(ii);
            T.print();
            pt no = new pt(T.cs_dev_id,
                           (int)(T.projected_image_x),
                           (int)(T.projected_image_y),
                           T);
            alltargets.add(no);
         }
      }
      catch (Exception e) {
         logger.log("error opening point targets (" + e + ").");
      }
      resetvisible();
   }
   
   public void importTargets() {
      try {
         FileInputStream in = new FileInputStream(
                    new File (fileName + Constants.ground_target_file_located));
         TargetIngester t = new TargetIngester( in);
         Vector v = t.getElements();

         trgs = v;
         alltargets = new Vector();
         for (int ii=0; ii < v.size(); ii++ ) {
            Target T = (Target)v.elementAt(ii);
            T.print();
            pt no = new pt(T.cs_dev_id,
                           (int)(T.projected_image_x),
                           (int)(T.projected_image_y),
                           T);
            if( (T.actual_image_x != 0) || ( T.actual_image_y != 0) ) {
               actualize(T.cs_dev_id,
                         (int)(T.actual_image_x + 0.5),
                         (int)(T.actual_image_y + 0.5));
            }
            alltargets.add(no);
         }
      }
      catch (Exception e){
         logger.log("Error importing point targets.");
      }
      resetvisible();
   }
   
   public void exportTargets() {
      String outfile = fileName + Constants.ground_target_file_located;      
      try {
         PrintStream ps = new PrintStream (new FileOutputStream(outfile));
         for (int i=0; i < visibletargets.size(); i++) {
            pt now = (pt) visibletargets.elementAt(i);
            if (now.actualized) {   
               Target T = (Target)trgs.elementAt(i);
               T.actual_image_x = now.actualx;
               T.actual_image_y = now.actualy;
               T.print(ps);
            }
         }
         ps.close();
      }
      catch(IOException e) {
         logger.log("Error exporting point targets.");
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
   
   public void actualize  (String devname, int actualx, int actualy) {
      for (int i=0; i < visibletargets.size(); i++) {
         pt now = (pt) visibletargets.elementAt(i);
         if (now.devname.equals(devname)) {
            now.actualx = actualx;
            now.actualy = actualy;
            now.actualized = true;
            break;
         }
      }
   }

   //give me the preadjusted x and y.
   public Vector zoompoints(int realx, int realy, int zwidth, int zheight) { 
      Vector zoomvector = new Vector(30);   //30 is just whatever.
      
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

/* ***********************************************************************/
// CLASS PT
class pt {
   String devname;
   int actualx;
   int actualy;
   int potentialx;
   int potentialy;
   boolean actualized;
   Target t;

   pt (String _devname, int _potentialx, int _potentialy, Target _T) {
      this.devname = _devname;
      this.potentialx = _potentialx;
      this.potentialy = _potentialy;
      _T = t;
   }
   
   public void actualize(int _actualx, int _actualy) {
      this.actualx = _actualx;
      this.actualy = _actualy;
      actualized = true;
   }
}

