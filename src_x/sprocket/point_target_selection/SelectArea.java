import java.awt.* ;
import java.io.* ;
import java.util.* ;

public class SelectArea extends Dialog {
	protected String area;
	protected Vector area_list;
	protected java.awt.List area_list_wid;
	protected TextField info_area;
	protected Button cancel, ok;
	protected Area selected_item;

	public Area getSelectedItem()
	  {
	    return selected_item;
          }

	public SelectArea (String title, Frame f)
	  {

		super(f, title, true);
			/* Structure window */
		Panel top, bottom, button_row;
		top = new Panel(new BorderLayout());
		bottom = new Panel(new BorderLayout());
		button_row = new Panel();
		top.add(bottom, BorderLayout.SOUTH);
	
		top.add(new Label ("Please select the region"), BorderLayout.NORTH);

			/* Obtain the possible areas */
		Spawner S = new Spawner ( "list_sites.sql");
             	AreaReader r = new AreaReader ( S.In);
             	area_list= r.getElements();
			
			/* Display the Area's names */
		area_list_wid = new java.awt.List ( area_list.size() );
		for ( int a = 0; a < area_list.size(); a++)
		  area_list_wid.add(((Area)area_list.elementAt(a)).cs_name);
		top.add(area_list_wid, BorderLayout.CENTER);

			/* Create a info window */
		info_area = new TextField();
		info_area.setEditable(false);
	
		bottom.add(info_area, BorderLayout.CENTER);
		cancel = new Button ( "Cancel");
		ok = new Button("Ok");
		button_row.add(ok);
		button_row.add(cancel);
		bottom.add(button_row, BorderLayout.SOUTH);
		add(top);
	  }

	public boolean action ( Event e, Object arg )
	  {
	   //System.err.println("Event = " + e.target);
           if ( e.target == cancel )
	      {
	        this.hide();
		this.dispose();
		return true;
	      }
	   if ( e.target == ok )
	     {
		//preform some ok stuff here
                int item = area_list_wid.getSelectedIndex();
                selected_item = (Area)area_list.elementAt(item);

		if ( selected_item != null)
		  {
		     	this.hide();
                	this.dispose();
                	return true;
		  }
		return true;
	     }
	   if (e.target == area_list_wid)
	     {
		System.err.println("List selected.");
		int item = area_list_wid.getSelectedIndex();
		selected_item = (Area)area_list.elementAt(item);
		info_area.setText( selected_item.cs_desc  );
		return true;	
	     }
	   return false;
	  }

	public static void main ( String [] args)
	 {
	/*
	   SelectArea f = new SelectArea (" Foo! ");
	   f.resize(200,300);
	   f.setLocation(200,200);
	   f.show();
	*/
	 }
	}

