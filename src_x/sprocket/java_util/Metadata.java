import java.io.*;
import java.util.*;

/******************************************************
*  Metadata
*       Reads and stores a few metadata values.
*
*******************************************************/

public class Metadata {
	public String converter_version;
	public String system_version;
        public String date_of_processing ;
        public String center_gmt ;
        public String platform;
        public String frequency ;
        public String polarization ;
        public double track_angle ;
        public double clock_angle ;
        public double number_of_pixels ;
        public double number_of_lines;
        public double pixel_size_range ;
        public double pixel_size_az ;
        public double platform_altitude;
        public String projection;                    
        public double slant_range_to_first_pixel;
        public double earth_radius_at_center;
        public double earth_radius_at_nadir;
        public String image_format;
	public String basename;
	public String ground_targets_org;
	public String ground_targets_located;
	public String metadata_file;
	public double far_early_lat;
	public double far_early_long;
	public double near_early_lat;
	public double near_early_long;
	public double far_late_lat;
	public double far_late_long;
	public double near_late_lat;
	public double near_late_long;
 	public double ellipsoid_major_axis ;
	public double ellipsoid_minor_axis ;
	public String asending_or_desending;
	Map value_map;
	List value_list;

	public Metadata (String base )
	  {

	  basename = new String ( base );
	  metadata_file = base +  Constants.metafile;
	  ground_targets_org = base + Constants.ground_target_file;
	  ground_targets_located = base + Constants.ground_target_file_located;
	  java.util.List tk, va;
	  tk = new ArrayList();
          va = new ArrayList();

		/* Read metadata */

	  try {
	  	InputStream mf = new FileInputStream  (metadata_file); 
		MetaReader mr = new MetaReader(mf);
		tk = mr.tokens;
		va = mr.values;
	     }
	  catch ( Exception e )
    	    {
		System.err.println(
			"Error occured while reading from \"" +
			metadata_file + "\" ("+e+")"
		    );
  	    }

		/* Create a map and place all the metadata items in it */
	Map m = new HashMap();
	 for ( int a = 0; a < tk.size(); a++)
		m.put( tk.get(a), new Integer(a));

		/* Store all the metadata parameters */
        system_version = obtain_value(va, m, "system_version");
	//date_of_processing = obtain_value(va, m,"date_of_processing");
        center_gmt = obtain_value(va,m, "center_gmt");
	platform = obtain_value ( va,m, "platform");
	frequency = obtain_value(va,m,  "frequency");
	polarization = obtain_value (va,m,  "polarization" );
	track_angle = Double.parseDouble(obtain_value (va,m,  "track_angle" ));
	clock_angle = Double.parseDouble(obtain_value (va,m,  "clock_angle" ));
	number_of_pixels = Double.parseDouble(obtain_value (va,m,   "number_of_pixels" ));
	number_of_lines = Double.parseDouble(obtain_value (va,m,  "number_of_lines" ));
	pixel_size_range = Double.parseDouble(obtain_value (va,m,  "pixel_size_range" ));
	pixel_size_az	= Double.parseDouble(obtain_value (va,m,  "pixel_size_az" ));
	platform_altitude = Double.parseDouble(obtain_value (va,m,   "platform_altitude" ));
	projection = obtain_value (va,m,  "projection" );
	slant_range_to_first_pixel = Double.parseDouble(obtain_value(va,m, 
		"slant_range_to_first_pixel" ));

	earth_radius_at_center = Double.parseDouble(obtain_value(va,m, "earth_radius_at_center" ));
	earth_radius_at_nadir = Double.parseDouble(obtain_value(va,m, "earth_radius_at_nadir"));
 	asending_or_desending = obtain_value ( va, m, "ascending/descending");

	image_format = obtain_value(va,m, "image_format");

	far_early_lat = Double.parseDouble(
			obtain_value(va, m, "far_early_lat"));
	far_early_long = Double.parseDouble(
			obtain_value(va,m, "far_early_long"));
	near_early_lat = Double.parseDouble(
			obtain_value(va,m, "near_early_lat"));
	near_early_long = Double.parseDouble(
			obtain_value(va,m, "near_early_long"));
	far_late_lat = Double.parseDouble(
			obtain_value(va,m, "far_late_lat"));
	far_late_long = Double.parseDouble(
			obtain_value(va,m, "far_late_long"));
	near_late_lat = Double.parseDouble(
			obtain_value(va,m, "near_late_lat"));
	near_late_long = Double.parseDouble(
			obtain_value(va,m, "near_late_long"));
        ellipsoid_major_axis = Double.parseDouble(
                        obtain_value(va,m, "ellipsoid_major_axis"));
        ellipsoid_minor_axis = Double.parseDouble(
                        obtain_value(va,m, "ellipsoid_minor_axis"));


 	value_map = m;	
	value_list = va;


		/* Store names */
	  } 
	private String obtain_value ( List va, Map m, String fubar)
	  {
	     String foo = "";
	     try {
		foo =  (String ) va.get(((Integer)m.get(fubar)).intValue() );
		//System.err.println(fubar + "=" + foo);
		//return (String ) va.get(((Integer)m.get(fubar)).intValue() );
	       }
	     catch ( java.lang.NullPointerException e)
		{
		   System.err.println("ERROR:\t Could not read metadata item \""+fubar+"\"");
		}
		return foo;
	  }

	double obtain_double ( String name )
           {
		String val = obtain_value (value_list, value_map, name);
		return (Double.parseDouble(val) );
	   }
	int obtain_int ( String name )
           {
                String val = obtain_value (value_list, value_map, name);
                return (Integer.parseInt(val) );
           }

	private void print_value ( PrintStream p, String token, String value)
	  {
		p.println(token + "\t=\t\"" + value + "\"");	
	  }
	private void print_value ( PrintStream p, String token, double value)
          {
                p.println(token + "\t=\t\"" + value + "\"");
          }


	public void print ( PrintStream p)
	  {
	        print_value ( p, "converter_version", converter_version);
        	print_value ( p, "system_version", system_version);
        	print_value (p, "date_of_processing", date_of_processing);
		print_value (p, "center_gmt", center_gmt);
        	print_value (p, "platform", platform);
        	print_value (p, "frequency", frequency) ;
         	print_value (p, "polarization", polarization);
                print_value (p, "track_angle", track_angle);
                print_value (p, "clock_angle", clock_angle);
                print_value (p, "number_of_pixels", number_of_pixels);
                print_value (p, "number_of_lines", number_of_lines);
                print_value (p, "pixel_size_range", pixel_size_range);
                print_value (p, "pixel_size_az", pixel_size_az);
                print_value (p, "platform_altitude", platform_altitude);
                print_value (p, "projection", projection);
                print_value (p, "slant_range_to_first_pixel", slant_range_to_first_pixel);
                print_value (p, "earth_radius_at_center", earth_radius_at_center);
                print_value (p, "earth_radius_at_nadir", earth_radius_at_nadir);
                print_value (p, "image_format", image_format);
                print_value (p, "basename", basename);
                print_value (p, "ground_targets_org", ground_targets_org);
                print_value (p, "ground_targets_located", ground_targets_located);
                print_value (p, "metadata_file", metadata_file);
                print_value (p, "far_early_lat", far_early_lat);
                print_value (p, "far_early_long", far_early_long);
                print_value (p, "near_early_lat", near_early_lat);
                print_value (p, "near_early_long", near_early_long);
                print_value (p, "far_late_lat", far_late_lat);
                print_value (p, "far_late_long", far_late_long);
                print_value (p, "near_late_lat", near_late_lat);
                print_value (p, "near_late_long", near_late_long);
          }
   }
