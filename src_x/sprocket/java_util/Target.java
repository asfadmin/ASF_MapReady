import java.util.* ;
import java.awt.* ;
import java.io.* ;

public class Target {
	String cs_id, cs_dev_id, cs_maint_date, cs_entry_date, cs_dev_type, cs_cr_offset;
	String cs_pre_fltdir, cs_pre_tilt, cs_pre_level, cs_pre_bsight, cs_pre_xrcs;
	String cs_post_fltdir, cs_post_tilt, cs_post_level, cs_post_bsight, cs_post_xrcs;
	String cs_lat, cs_long, cs_elev, cs_magdec, cs_cmnts, opr_name;
	double op_x, op_y;
	double zone;
	double projected_image_x, projected_image_y;
	double actual_image_x, actual_image_y;

	public Target ()
	 {}

	public Target ( StreamTokenizer st ) throws IOException
	{
		cs_id=ReadValue(st);
		cs_dev_id=ReadValue(st);
		cs_maint_date=ReadValue(st);
		cs_entry_date=ReadValue(st);
		cs_dev_type=ReadValue(st);
		cs_cr_offset=ReadValue(st);
		cs_pre_fltdir=ReadValue(st);
		cs_pre_tilt=ReadValue(st);
		cs_pre_level=ReadValue(st);
		cs_pre_bsight=ReadValue(st);
		cs_pre_xrcs=ReadValue(st);
		cs_post_fltdir=ReadValue(st);
		cs_post_tilt=ReadValue(st);
		cs_post_level=ReadValue(st);
		cs_post_bsight=ReadValue(st);
		cs_post_xrcs=ReadValue(st);
		cs_lat=ReadValue(st);
		cs_long=ReadValue(st);
		cs_elev=ReadValue(st);
		cs_magdec=ReadValue(st);
		cs_cmnts=ReadValue(st);
		opr_name=ReadValue(st);
		op_x = 0.0;
		op_y = 0.0;
		zone = 0.0;
		projected_image_x =0.0;
		projected_image_y = 0.0;
		actual_image_x = 0.0;
		actual_image_y = 0.0;
	}

	private void eat_white_space ( StreamTokenizer st  ) throws IOException
	{
		while ( (st.ttype != st.TT_EOF) && (st.ttype != '\"') && st.ttype != st.TT_WORD &&  (st.ttype != '\''))
			st.nextToken();
	}

	private String ReadValue( StreamTokenizer st) throws IOException
	{
		String val;

		/* Skip the white space and other junk... */
		eat_white_space(st);
		if ( st.sval != null)
			val = new String ( st.sval );
		else
			val = new String (" ");
		st.nextToken();

		eat_white_space(st);
		//System.err.println("ReadValue (" + val + ")");
		return val;
	}

	private String read_element ( StreamTokenizer st, String tag ) throws IOException
	{
		String token, value, equals;
		token = "foo!";
		try {
		   token=ReadValue(st);
		   equals=ReadValue(st);
		   value=ReadValue(st);
		}
		catch (IOException e) {
		   System.err.println("ERROR:\tA error was encountered while");
		   System.err.println("ERROR:\treading token \"" +token +"\"");
		   throw e;
		}
		//System.err.println(token+ "=" + value);
		return value;
	}

	private double extract_double ( String field_name, String field )
	{
		try {
			return Double.parseDouble(field);
		}
		catch (NumberFormatException e) {
			System.err.println("ERROR:Could not convert field \""
			                   + field_name
			                   + "\" with a value of \""
			                   + field +"\"");
			return 0;
		}
	}

	public void injest_target ( StreamTokenizer st ) throws IOException
	{
		cs_id = read_element ( st, "CS_ID");
		cs_dev_id = read_element ( st, "CS_DEV_ID");
		cs_maint_date = read_element ( st, "CS_MAINT_DATE");
		cs_entry_date = read_element ( st, "CS_ENTRY_DATE");
		cs_dev_type = read_element ( st, "CS_DEV_TYPE");
		cs_cr_offset = read_element ( st, "CS_CR_OFFSET");
		cs_pre_fltdir = read_element ( st, "CS_PRE_FLTDIR");
		cs_pre_tilt = read_element ( st, "CS_PRE_TILT");
		cs_pre_level = read_element ( st, "CS_PRE_LEVEL");
		cs_pre_bsight = read_element ( st, "CS_PRE_BSIGHT");
		cs_pre_xrcs = read_element ( st, "CS_PRE_XRCS");
		cs_post_fltdir = read_element ( st, "CS_POST_FLTDIR");
		cs_post_tilt = read_element ( st, "CS_POST_TILT");
		cs_post_level = read_element ( st, "CS_POST_LEVEL");
		cs_post_bsight = read_element ( st, "CS_POST_BSIGHT");
		cs_post_xrcs = read_element ( st, "CS_POST_XRCS");
		cs_lat = read_element ( st, "CS_LAT");
		cs_long = read_element ( st, "CS_LONG");
		cs_elev = read_element ( st, "CS_ELEV");
		op_x = extract_double ( "OP_X", read_element ( st, "OP_X"));
		op_y = extract_double ( "OP_X", read_element ( st, "OP_Y"));
		zone = extract_double ( "ZONE", read_element ( st, "ZONE"));
		projected_image_x = extract_double ( "PROJECTED_IMAGE_X",
				read_element ( st, "PROJECTED_IMAGE_X"));
		projected_image_y=Double.parseDouble(read_element (
			st, "PROJECTED_IMAGE_Y"));
		actual_image_x=Double.parseDouble(read_element (
			st, "ACTUAL_IMAGE_X"));
		actual_image_y=Double.parseDouble(read_element (
			st, "ACTUAL_IMAGE_Y"));
		cs_magdec = read_element ( st, "CS_MAGDEC");
		cs_cmnts = read_element ( st, "CS_CMNTS");
		opr_name = read_element ( st, "OPR_NAME");
	}

	public void print ( )
	{
		System.err.println("'" + cs_id + "' '" + cs_dev_id + "' '"
		                   + opr_name+"'" );
		System.err.println();
	}
	public void print ( PrintStream p)
	{
		print_element ( p, "CS_ID", cs_id);
		print_element ( p, "CS_DEV_ID", cs_dev_id);
		print_element ( p, "CS_MAINT_DATE",  cs_maint_date);
		print_element ( p, "CS_ENTRY_DATE", cs_entry_date);
		print_element ( p, "CS_DEV_TYPE", cs_dev_type);
		print_element ( p, "CS_CR_OFFSET", cs_cr_offset);
		print_element ( p, "CS_PRE_FLTDIR", cs_pre_fltdir);
		print_element ( p, "CS_PRE_TILT", cs_pre_tilt);
		print_element ( p, "CS_PRE_LEVEL", cs_pre_level);
		print_element ( p, "CS_PRE_BSIGHT", cs_pre_bsight);
		print_element ( p, "CS_PRE_XRCS", cs_pre_xrcs);
		print_element ( p, "CS_POST_FLTDIR", cs_post_fltdir);
		print_element ( p, "CS_POST_TILT", cs_post_tilt);
		print_element ( p, "CS_POST_LEVEL", cs_post_level);
		print_element ( p, "CS_POST_BSIGHT", cs_post_bsight);
		print_element ( p, "CS_POST_XRCS", cs_post_xrcs);
		print_element ( p, "CS_LAT", cs_lat);
		print_element ( p, "CS_LONG", cs_long);
		print_element ( p, "CS_ELEV", cs_elev);
		print_element ( p, "OP_X", Double.toString(op_x));
		print_element ( p, "OP_Y", Double.toString(op_y));
		print_element ( p, "ZONE", zone);
		print_element ( p, "PROJECTED_IMAGE_X", projected_image_x);
		print_element ( p, "PROJECTED_IMAGE_Y", projected_image_y);
		print_element ( p, "ACTUAL_IMAGE_X", actual_image_x);
		print_element ( p, "ACTUAL_IMAGE_Y", actual_image_y);
		print_element ( p, "CS_MAGDEC", cs_magdec);
		print_element ( p, "CS_CMNTS", cs_cmnts);
		print_element ( p, "OPR_NAME", opr_name);
		p.println(" ");
	}

	private void print_element( PrintStream p, String keyword, String value)
	{
		if ( value.equals(""))
			p.println("\t" + keyword + "\t=\t\"" + "NULL" + "\"");
		else
			p.println("\t" + keyword + "\t=\t\"" + value + "\"");
	}

	private void print_element( PrintStream p, String keyword, double value)
	{
		p.println("\t" + keyword + "\t=\t\"" + value + "\"");
	}
}
