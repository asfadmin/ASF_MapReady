import java.awt.* ;
import java.util.* ;

public class MainTargetSelection
  {
	private static void ussage ( )
	  {
		System.err.println("Ussage: TargetSelector <basename>");
	  }
	public static void main ( String [] args)
         {
	    if ( args.length != 1 )
	      {
		ussage();
		return;
	      }
	
		/* Read the metadata */
   	   Metadata meta = new Metadata (args[0]);	

		/* set the images center time */
	   ASF_Time t = new ASF_Time ( meta.center_gmt );
	   ASF_Time early = new ASF_Time ( meta.center_gmt);
		/* Subtract a week from it */
	   early.adjust_by_seconds(-60*24*60*60);

		/* Create a "root" frame */
	   Frame f = new Frame();

           SelectArea sa = new SelectArea ("Area Selection", f);
           sa.resize(200,300);
           sa.setLocation(200,200);
           sa.show();
	   Area area = sa.getSelectedItem();

	  Spawner S = new Spawner ( "list_ground_targets.sql \"" + area.cs_id
                        + "\" \"" +
                        early.toASFDate() + "\" \"" + t.toASFDate() + "\"");
	  
           TargetReader r = new TargetReader ( S.In);
           Vector v = r.getElements();

           Map m = new HashMap();
           for ( int a = 0; a < v.size(); a ++ )
                {
                    String alpha = ((Target)v.elementAt(a)).cs_dev_id;
                    //System.err.println("Working on " + alpha);
                    java.util.List l = (java.util.List)m.get(alpha);
                    if (l==null)
                          m.put(alpha, l=new ArrayList());
                    l.add(v.elementAt(a));
                }

           SelectTargets foo = new SelectTargets("foo!", m, 
		args[0]+Constants.ground_target_file);
           foo.resize(400,400);
           foo.setLocation(200,200);
           foo.show();
	 }
  }
