import java.util.* ;
import java.io.* ;
import java.awt.* ;
import java.awt.event.* ;

public class SelectTargets extends Frame implements ActionListener {
   TextField comments;
   Button okButton, cancelButton;
   ArrayList targets, target_boxes;
   PrintStream printer;
   
   public SelectTargets ( String title , Map m, String bing)
   {
      super(title);

      /* Use the border layout */
      setLayout(new BorderLayout());

      /* The main point target display */   
      ScrollPane sp = new ScrollPane();
      add(sp, BorderLayout.CENTER);

      /* Label for the main target display */   
      add(new Label(
         "Please select the point targets to be used during analysis"), 
         BorderLayout.NORTH);
   
      Panel p = new Panel();
      p.setLayout(new GridLayout(m.size()+1,1));
      sp.add(p);

      /* Allocate space for arrays -
       *    targets      - a list of all the targets 
       *    target_boxes - a list of all the selection boxes
       */
     targets = new ArrayList();
     target_boxes = new ArrayList();

      /* Fill out the main target display */   
      p.add(new TargetPanelLabels());   
      for (Iterator i=m.values().iterator();  i.hasNext();  ) {
         p.add( new TargetPanel( (java.util.List)i.next(), 
                                 targets, target_boxes) );
      }
      add(CreateLowerPane(), BorderLayout.SOUTH);

      try   { printer = new PrintStream( new FileOutputStream (bing) ); }
      catch ( IOException  e) {
         System.err.println("A error occurred while reading from the file \""
                            + bing +"\" (" + e +")");
      }
   }

   /* Builds the lower pane with comment field and ok/cancel buttons */
   Panel CreateLowerPane ( ) {
      Panel tt = new Panel();
      okButton = new Button("Ok");
      cancelButton = new Button("Cancel");
      comments = new TextField();

      Panel tmp= new Panel();
      //tmp.setLayout(new GridLayout(1,2));
      tmp.add(cancelButton);
      tmp.add(okButton);

      tt.setLayout(new GridLayout(2,1));
      tt.add(comments);
      tt.add(tmp);
      return (tt);
   }

   void WriteAndQuit() {
      for (int a=0; a<targets.size(); a++) {
         if (((Checkbox)target_boxes.get(a)).getState()) {
            ((Target)targets.get(a)).print(printer);
         }
      }
   }

   public void actionPerformed (ActionEvent actionEvt) {
      if (actionEvt.getSource() == cancelButton) {
         setVisible(false);
         this.dispose();
         return;
      }
      if (actionEvt.getSource() == okButton) {
         setVisible(false);
         this.dispose();
         WriteAndQuit();
         System.exit(0);
         return;
      }
      /* Check to see if it was one of the check boxes.
       * If so, print the comments */
      if (target_boxes.contains(actionEvt.getSource())) {
         int targInd = target_boxes.indexOf(actionEvt.getSource());
         comments.setText(((Target)targets.get(targInd)).cs_cmnts);
         return;
      }
   }
}


class TargetPanelLabels extends Panel {
   public TargetPanelLabels () {
      setLayout(new GridLayout(1, 5 ));
      add(new Label("Target"));
      add(new Label("Date of Maintance"));
      add(new Label());
      add(new Label());
      add(new Label());
      add(new Label());
   }
}


class TargetPanel extends Panel {
   private Panel target_list;
   public TargetPanel (java.util.List l, ArrayList trg, ArrayList check)  {
      super();
      int a;

      setLayout(new BorderLayout());
      target_list = new Panel();
      target_list.setLayout(new GridLayout(1, 5 ));

      CheckboxGroup cbg = new CheckboxGroup();

      /* Add the "None" flag for no targets */
      target_list.add(new Checkbox("None", cbg, true));

      /* Add up to 5 of the targets to the panel */
      for (a = 0; (a < l.size()) && (a < 5); a++) {
         Checkbox too = new Checkbox( ((Target)l.get(a)).cs_maint_date,
                                      cbg, false);
         target_list.add(too);

         /* Save Target and the check box repersenting the target */
         check.add(too);
         trg.add(l.get(a));
      }
         
      /* To even out spacing of targets, backfill with empty dates */
      for ( ; a < 5; a++)
         target_list.add( new Label());
     
      /* Create label for target */ 
      TextField lb = new TextField ( ((Target) l.get(0)).cs_dev_id);
      lb.setEditable(false);
      lb.setColumns(10);
      add(lb, BorderLayout.WEST);
      add(target_list, BorderLayout.CENTER);
   }
}
