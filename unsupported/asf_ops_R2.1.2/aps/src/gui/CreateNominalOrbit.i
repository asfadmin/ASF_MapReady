! UIMX ascii 2.9 key: 7860                                                      

*CreateNominalOrbit.class: form
*CreateNominalOrbit.classinc:
*CreateNominalOrbit.classspec:
*CreateNominalOrbit.classmembers:
*CreateNominalOrbit.classconstructor:
*CreateNominalOrbit.classdestructor:
*CreateNominalOrbit.gbldecl: #include <stdio.h>\
#ifdef COPYRIGHT\
Copyright (c)1996, California Institute of Technology.\
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.\
#endif\
 \
/*==============================================================================\
Filename:\
 \
Description:\
 \
External Functions Defined:\
 \
File Scope Functions:\
 \
External Variables Defined:\
 \
File Scope Variables:\
 \
Notes:\
 \
==============================================================================*/\
#pragma ident   "@(#)CreateNominalOrbit.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.CreateNominalOrbit.i"\
\
#include "cb_cnomorb.h"\
\
extern Widget cnomorb_form ;\

*CreateNominalOrbit.ispecdecl:
*CreateNominalOrbit.funcdecl: swidget create_CreateNominalOrbit(UxParent)\
swidget UxParent;
*CreateNominalOrbit.funcname: create_CreateNominalOrbit
*CreateNominalOrbit.funcdef: "swidget", "<create_CreateNominalOrbit>(%)"
*CreateNominalOrbit.argdecl: swidget UxParent;
*CreateNominalOrbit.arglist: UxParent
*CreateNominalOrbit.arglist.UxParent: "swidget", "%UxParent%"
*CreateNominalOrbit.icode:
*CreateNominalOrbit.fcode: XtAddCallback(XtParent(rtrn), XtNpopupCallback,\
	cb_show_orbit_relations, (XtPointer *) scrolledList2) ;\
 \
\
return(rtrn);\

*CreateNominalOrbit.auxdecl:
*CreateNominalOrbit.static: true
*CreateNominalOrbit.name: CreateNominalOrbit
*CreateNominalOrbit.parent: NO_PARENT
*CreateNominalOrbit.parentExpression: UxParent
*CreateNominalOrbit.defaultShell: topLevelShell
*CreateNominalOrbit.width: 667
*CreateNominalOrbit.height: 694
*CreateNominalOrbit.resizePolicy: "resize_any"
*CreateNominalOrbit.isCompound: "true"
*CreateNominalOrbit.compoundIcon: "form.xpm"
*CreateNominalOrbit.compoundName: "form_"
*CreateNominalOrbit.x: 419
*CreateNominalOrbit.y: 2
*CreateNominalOrbit.unitType: "pixels"
*CreateNominalOrbit.allowShellResize: "false"
*CreateNominalOrbit.noResize: "true"
*CreateNominalOrbit.dialogTitle: "APS:CREATE NOMINAL ORBIT"

*scrolledWindowList1.class: scrolledWindow
*scrolledWindowList1.static: true
*scrolledWindowList1.name: scrolledWindowList1
*scrolledWindowList1.parent: CreateNominalOrbit
*scrolledWindowList1.scrollingPolicy: "application_defined"
*scrolledWindowList1.visualPolicy: "variable"
*scrolledWindowList1.scrollBarDisplayPolicy: "static"
*scrolledWindowList1.shadowThickness: 0
*scrolledWindowList1.isCompound: "true"
*scrolledWindowList1.compoundIcon: "scrllist.xpm"
*scrolledWindowList1.compoundName: "scrolled_List"
*scrolledWindowList1.x: 45
*scrolledWindowList1.y: 115
*scrolledWindowList1.width: 600
*scrolledWindowList1.resizable: "true"

*scrolledList2.class: scrolledList
*scrolledList2.static: true
*scrolledList2.name: scrolledList2
*scrolledList2.parent: scrolledWindowList1
*scrolledList2.width: 620
*scrolledList2.scrollBarDisplayPolicy: "as_needed"
*scrolledList2.listSizePolicy: "resize_if_possible"
*scrolledList2.visibleItemCount: 6
*scrolledList2.automaticSelection: "true"
*scrolledList2.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"
*scrolledList2.browseSelectionCallback.source: public
*scrolledList2.browseSelectionCallback: cb_update_cnomorb_form
*scrolledList2.itemCount: 1
*scrolledList2.items: "E1        A"

*label1.class: label
*label1.static: true
*label1.name: label1
*label1.parent: CreateNominalOrbit
*label1.isCompound: "true"
*label1.compoundIcon: "label.xpm"
*label1.compoundName: "label_"
*label1.x: 72
*label1.y: 10
*label1.width: 540
*label1.height: 50
*label1.labelString: "CREATE  NOMINAL  STATE  VECTOR  AND  ORBIT"
*label1.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"

*label2.class: label
*label2.static: true
*label2.name: label2
*label2.parent: CreateNominalOrbit
*label2.isCompound: "true"
*label2.compoundIcon: "label.xpm"
*label2.compoundName: "label_"
*label2.x: 45
*label2.y: 95
*label2.width: 600
*label2.height: 20
*label2.labelString: "SATELLITE  PHASE      START TIME       #DAYS    -DAYS  REVS-    LONGITUDE"
*label2.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label2.alignment: "alignment_beginning"

*separator1.class: separator
*separator1.static: true
*separator1.name: separator1
*separator1.parent: CreateNominalOrbit
*separator1.width: 667
*separator1.height: 10
*separator1.isCompound: "true"
*separator1.compoundIcon: "sep.xpm"
*separator1.compoundName: "separator_"
*separator1.x: 0
*separator1.y: 215

*textField_sat.class: textField
*textField_sat.static: true
*textField_sat.name: textField_sat
*textField_sat.parent: CreateNominalOrbit
*textField_sat.isCompound: "true"
*textField_sat.compoundIcon: "textfield.xpm"
*textField_sat.compoundName: "text_Field"
*textField_sat.x: 43
*textField_sat.y: 227
*textField_sat.height: 32
*textField_sat.cursorPositionVisible: "false"
*textField_sat.editable: "false"
*textField_sat.sensitive: "false"
*textField_sat.text: ""
*textField_sat.columns: 9
*textField_sat.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_sat.traversalOn: "false"
*textField_sat.width: 91

*label5.class: label
*label5.static: true
*label5.name: label5
*label5.parent: CreateNominalOrbit
*label5.isCompound: "true"
*label5.compoundIcon: "label.xpm"
*label5.compoundName: "label_"
*label5.x: 70
*label5.y: 410
*label5.width: 80
*label5.height: 30
*label5.labelString: "LAST REV:"
*label5.alignment: "alignment_end"

*label7.class: label
*label7.static: true
*label7.name: label7
*label7.parent: CreateNominalOrbit
*label7.isCompound: "true"
*label7.compoundIcon: "label.xpm"
*label7.compoundName: "label_"
*label7.x: 60
*label7.y: 290
*label7.width: 90
*label7.height: 30
*label7.labelString: "PHASE LENGTH:"
*label7.alignment: "alignment_end"

*label8.class: label
*label8.static: true
*label8.name: label8
*label8.parent: CreateNominalOrbit
*label8.isCompound: "true"
*label8.compoundIcon: "label.xpm"
*label8.compoundName: "label_"
*label8.x: 54
*label8.y: 380
*label8.width: 96
*label8.height: 30
*label8.labelString: "#COMPLETE REVS:"
*label8.alignment: "alignment_end"

*label9.class: label
*label9.static: true
*label9.name: label9
*label9.parent: CreateNominalOrbit
*label9.isCompound: "true"
*label9.compoundIcon: "label.xpm"
*label9.compoundName: "label_"
*label9.x: 70
*label9.y: 320
*label9.width: 80
*label9.height: 30
*label9.labelString: "REPEAT CYCLE:"

*textField_phase_name.class: textField
*textField_phase_name.static: true
*textField_phase_name.name: textField_phase_name
*textField_phase_name.parent: CreateNominalOrbit
*textField_phase_name.isCompound: "true"
*textField_phase_name.compoundIcon: "textfield.xpm"
*textField_phase_name.compoundName: "text_Field"
*textField_phase_name.x: 135
*textField_phase_name.y: 227
*textField_phase_name.height: 32
*textField_phase_name.cursorPositionVisible: "false"
*textField_phase_name.editable: "false"
*textField_phase_name.sensitive: "false"
*textField_phase_name.columns: 2
*textField_phase_name.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_phase_name.traversalOn: "false"

*textField_cycle_days.class: textField
*textField_cycle_days.static: true
*textField_cycle_days.name: textField_cycle_days
*textField_cycle_days.parent: CreateNominalOrbit
*textField_cycle_days.isCompound: "true"
*textField_cycle_days.compoundIcon: "textfield.xpm"
*textField_cycle_days.compoundName: "text_Field"
*textField_cycle_days.x: 421
*textField_cycle_days.y: 227
*textField_cycle_days.height: 32
*textField_cycle_days.cursorPositionVisible: "false"
*textField_cycle_days.editable: "false"
*textField_cycle_days.sensitive: "false"
*textField_cycle_days.columns: 5
*textField_cycle_days.resizeWidth: "false"
*textField_cycle_days.maxLength: 5
*textField_cycle_days.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_cycle_days.traversalOn: "false"
*textField_cycle_days.width: 59

*textField_cycle_revs.class: textField
*textField_cycle_revs.static: true
*textField_cycle_revs.name: textField_cycle_revs
*textField_cycle_revs.parent: CreateNominalOrbit
*textField_cycle_revs.isCompound: "true"
*textField_cycle_revs.compoundIcon: "textfield.xpm"
*textField_cycle_revs.compoundName: "text_Field"
*textField_cycle_revs.x: 482
*textField_cycle_revs.y: 227
*textField_cycle_revs.height: 32
*textField_cycle_revs.cursorPositionVisible: "false"
*textField_cycle_revs.editable: "false"
*textField_cycle_revs.sensitive: "false"
*textField_cycle_revs.columns: 5
*textField_cycle_revs.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_cycle_revs.traversalOn: "false"
*textField_cycle_revs.width: 59

*label10.class: label
*label10.static: true
*label10.name: label10
*label10.parent: CreateNominalOrbit
*label10.isCompound: "true"
*label10.compoundIcon: "label.xpm"
*label10.compoundName: "label_"
*label10.x: 210
*label10.y: 320
*label10.width: 30
*label10.height: 30
*label10.labelString: "days"

*label11.class: label
*label11.static: true
*label11.name: label11
*label11.parent: CreateNominalOrbit
*label11.isCompound: "true"
*label11.compoundIcon: "label.xpm"
*label11.compoundName: "label_"
*label11.x: 210
*label11.y: 350
*label11.width: 30
*label11.height: 30
*label11.labelString: "revs"

*label12.class: label
*label12.static: true
*label12.name: label12
*label12.parent: CreateNominalOrbit
*label12.isCompound: "true"
*label12.compoundIcon: "label.xpm"
*label12.compoundName: "label_"
*label12.x: 350
*label12.y: 290
*label12.width: 100
*label12.height: 30
*label12.labelString: "SEMI-MAJOR AXIS:"
*label12.alignment: "alignment_end"

*label13.class: label
*label13.static: true
*label13.name: label13
*label13.parent: CreateNominalOrbit
*label13.isCompound: "true"
*label13.compoundIcon: "label.xpm"
*label13.compoundName: "label_"
*label13.x: 360
*label13.y: 320
*label13.width: 90
*label13.height: 30
*label13.labelString: "ECCENTRICITY:"
*label13.alignment: "alignment_end"

*label14.class: label
*label14.static: true
*label14.name: label14
*label14.parent: CreateNominalOrbit
*label14.isCompound: "true"
*label14.compoundIcon: "label.xpm"
*label14.compoundName: "label_"
*label14.x: 360
*label14.y: 350
*label14.width: 90
*label14.height: 30
*label14.alignment: "alignment_end"
*label14.labelString: "INCLINATION:"

*label15.class: label
*label15.static: true
*label15.name: label15
*label15.parent: CreateNominalOrbit
*label15.isCompound: "true"
*label15.compoundIcon: "label.xpm"
*label15.compoundName: "label_"
*label15.x: 330
*label15.y: 380
*label15.width: 120
*label15.height: 30
*label15.labelString: "SUBSAT LONGITUDE:"
*label15.alignment: "alignment_end"

*textField_phase_start.class: textField
*textField_phase_start.static: true
*textField_phase_start.name: textField_phase_start
*textField_phase_start.parent: CreateNominalOrbit
*textField_phase_start.isCompound: "true"
*textField_phase_start.compoundIcon: "textfield.xpm"
*textField_phase_start.compoundName: "text_Field"
*textField_phase_start.x: 172
*textField_phase_start.y: 227
*textField_phase_start.height: 32
*textField_phase_start.columns: 21
*textField_phase_start.cursorPositionVisible: "false"
*textField_phase_start.editable: "false"
*textField_phase_start.sensitive: "false"
*textField_phase_start.resizeWidth: "false"
*textField_phase_start.text: ""
*textField_phase_start.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_phase_start.traversalOn: "false"
*textField_phase_start.width: 190

*textField_phase_days.class: textField
*textField_phase_days.static: true
*textField_phase_days.name: textField_phase_days
*textField_phase_days.parent: CreateNominalOrbit
*textField_phase_days.isCompound: "true"
*textField_phase_days.compoundIcon: "textfield.xpm"
*textField_phase_days.compoundName: "text_Field"
*textField_phase_days.x: 362
*textField_phase_days.y: 227
*textField_phase_days.height: 32
*textField_phase_days.cursorPositionVisible: "false"
*textField_phase_days.editable: "false"
*textField_phase_days.sensitive: "false"
*textField_phase_days.columns: 5
*textField_phase_days.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_phase_days.traversalOn: "false"
*textField_phase_days.width: 59

*textField_phase_orbits.class: textField
*textField_phase_orbits.static: true
*textField_phase_orbits.name: textField_phase_orbits
*textField_phase_orbits.parent: CreateNominalOrbit
*textField_phase_orbits.isCompound: "true"
*textField_phase_orbits.compoundIcon: "textfield.xpm"
*textField_phase_orbits.compoundName: "text_Field"
*textField_phase_orbits.x: 150
*textField_phase_orbits.y: 380
*textField_phase_orbits.height: 30
*textField_phase_orbits.cursorPositionVisible: "false"
*textField_phase_orbits.editable: "false"
*textField_phase_orbits.sensitive: "false"
*textField_phase_orbits.columns: 5
*textField_phase_orbits.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_phase_orbits.traversalOn: "false"

*textField_last_rev.class: textField
*textField_last_rev.static: true
*textField_last_rev.name: textField_last_rev
*textField_last_rev.parent: CreateNominalOrbit
*textField_last_rev.isCompound: "true"
*textField_last_rev.compoundIcon: "textfield.xpm"
*textField_last_rev.compoundName: "text_Field"
*textField_last_rev.x: 150
*textField_last_rev.y: 410
*textField_last_rev.height: 30
*textField_last_rev.cursorPositionVisible: "false"
*textField_last_rev.editable: "false"
*textField_last_rev.sensitive: "false"
*textField_last_rev.columns: 5
*textField_last_rev.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_last_rev.traversalOn: "false"

*textField_orb_a.class: textField
*textField_orb_a.static: true
*textField_orb_a.name: textField_orb_a
*textField_orb_a.parent: CreateNominalOrbit
*textField_orb_a.isCompound: "true"
*textField_orb_a.compoundIcon: "textfield.xpm"
*textField_orb_a.compoundName: "text_Field"
*textField_orb_a.x: 450
*textField_orb_a.y: 290
*textField_orb_a.height: 30
*textField_orb_a.cursorPositionVisible: "false"
*textField_orb_a.editable: "false"
*textField_orb_a.sensitive: "false"
*textField_orb_a.columns: 10
*textField_orb_a.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_orb_a.resizeWidth: "true"
*textField_orb_a.shadowThickness: 2
*textField_orb_a.traversalOn: "false"
*textField_orb_a.width: 94

*textField_orb_e.class: textField
*textField_orb_e.static: true
*textField_orb_e.name: textField_orb_e
*textField_orb_e.parent: CreateNominalOrbit
*textField_orb_e.isCompound: "true"
*textField_orb_e.compoundIcon: "textfield.xpm"
*textField_orb_e.compoundName: "text_Field"
*textField_orb_e.x: 450
*textField_orb_e.y: 320
*textField_orb_e.height: 30
*textField_orb_e.cursorPositionVisible: "false"
*textField_orb_e.editable: "false"
*textField_orb_e.sensitive: "false"
*textField_orb_e.columns: 10
*textField_orb_e.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_orb_e.traversalOn: "false"

*textField_orb_i.class: textField
*textField_orb_i.static: true
*textField_orb_i.name: textField_orb_i
*textField_orb_i.parent: CreateNominalOrbit
*textField_orb_i.isCompound: "true"
*textField_orb_i.compoundIcon: "textfield.xpm"
*textField_orb_i.compoundName: "text_Field"
*textField_orb_i.x: 450
*textField_orb_i.y: 350
*textField_orb_i.height: 30
*textField_orb_i.cursorPositionVisible: "false"
*textField_orb_i.editable: "false"
*textField_orb_i.sensitive: "false"
*textField_orb_i.columns: 10
*textField_orb_i.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_orb_i.traversalOn: "false"

*textField_phase_lon.class: textField
*textField_phase_lon.static: true
*textField_phase_lon.name: textField_phase_lon
*textField_phase_lon.parent: CreateNominalOrbit
*textField_phase_lon.isCompound: "true"
*textField_phase_lon.compoundIcon: "textfield.xpm"
*textField_phase_lon.compoundName: "text_Field"
*textField_phase_lon.x: 543
*textField_phase_lon.y: 227
*textField_phase_lon.height: 32
*textField_phase_lon.cursorPositionVisible: "false"
*textField_phase_lon.editable: "false"
*textField_phase_lon.sensitive: "false"
*textField_phase_lon.columns: 10
*textField_phase_lon.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_phase_lon.traversalOn: "false"
*textField_phase_lon.width: 100

*label16.class: label
*label16.static: true
*label16.name: label16
*label16.parent: CreateNominalOrbit
*label16.isCompound: "true"
*label16.compoundIcon: "label.xpm"
*label16.compoundName: "label_"
*label16.x: 310
*label16.y: 410
*label16.width: 140
*label16.height: 30
*label16.labelString: "ARGUMENT OF PERIAPSIS:"

*textField_orb_arg_peri.class: textField
*textField_orb_arg_peri.static: true
*textField_orb_arg_peri.name: textField_orb_arg_peri
*textField_orb_arg_peri.parent: CreateNominalOrbit
*textField_orb_arg_peri.isCompound: "true"
*textField_orb_arg_peri.compoundIcon: "textfield.xpm"
*textField_orb_arg_peri.compoundName: "text_Field"
*textField_orb_arg_peri.x: 450
*textField_orb_arg_peri.y: 410
*textField_orb_arg_peri.height: 30
*textField_orb_arg_peri.cursorPositionVisible: "false"
*textField_orb_arg_peri.editable: "false"
*textField_orb_arg_peri.sensitive: "false"
*textField_orb_arg_peri.columns: 10
*textField_orb_arg_peri.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_orb_arg_peri.traversalOn: "false"

*label17.class: label
*label17.static: true
*label17.name: label17
*label17.parent: CreateNominalOrbit
*label17.isCompound: "true"
*label17.compoundIcon: "label.xpm"
*label17.compoundName: "label_"
*label17.x: 550
*label17.y: 290
*label17.width: 20
*label17.height: 30
*label17.labelString: "km"

*label18.class: label
*label18.static: true
*label18.name: label18
*label18.parent: CreateNominalOrbit
*label18.isCompound: "true"
*label18.compoundIcon: "label.xpm"
*label18.compoundName: "label_"
*label18.x: 550
*label18.y: 350
*label18.width: 30
*label18.height: 30
*label18.labelString: "deg"

*label19.class: label
*label19.static: true
*label19.name: label19
*label19.parent: CreateNominalOrbit
*label19.isCompound: "true"
*label19.compoundIcon: "label.xpm"
*label19.compoundName: "label_"
*label19.x: 550
*label19.y: 380
*label19.width: 30
*label19.height: 30
*label19.labelString: "deg"

*label20.class: label
*label20.static: true
*label20.name: label20
*label20.parent: CreateNominalOrbit
*label20.isCompound: "true"
*label20.compoundIcon: "label.xpm"
*label20.compoundName: "label_"
*label20.x: 550
*label20.y: 410
*label20.width: 30
*label20.height: 30
*label20.labelString: "deg"

*label21.class: label
*label21.static: true
*label21.name: label21
*label21.parent: CreateNominalOrbit
*label21.isCompound: "true"
*label21.compoundIcon: "label.xpm"
*label21.compoundName: "label_"
*label21.x: 210
*label21.y: 290
*label21.width: 30
*label21.height: 30
*label21.labelString: "days"

*pushButton_CreateVectorFile.class: pushButton
*pushButton_CreateVectorFile.static: true
*pushButton_CreateVectorFile.name: pushButton_CreateVectorFile
*pushButton_CreateVectorFile.parent: CreateNominalOrbit
*pushButton_CreateVectorFile.isCompound: "true"
*pushButton_CreateVectorFile.compoundIcon: "push.xpm"
*pushButton_CreateVectorFile.compoundName: "push_Button"
*pushButton_CreateVectorFile.x: 84
*pushButton_CreateVectorFile.y: 450
*pushButton_CreateVectorFile.width: 120
*pushButton_CreateVectorFile.height: 40
*pushButton_CreateVectorFile.labelString: "CREATE"
*pushButton_CreateVectorFile.activateCallback.source: public
*pushButton_CreateVectorFile.activateCallback: cb_do_cnom
*pushButton_CreateVectorFile.sensitive: "false"
*pushButton_CreateVectorFile.fontList: "rockwell-bold"

*pushButton_cnomorb_quit.class: pushButton
*pushButton_cnomorb_quit.static: true
*pushButton_cnomorb_quit.name: pushButton_cnomorb_quit
*pushButton_cnomorb_quit.parent: CreateNominalOrbit
*pushButton_cnomorb_quit.isCompound: "true"
*pushButton_cnomorb_quit.compoundIcon: "push.xpm"
*pushButton_cnomorb_quit.compoundName: "push_Button"
*pushButton_cnomorb_quit.x: 450
*pushButton_cnomorb_quit.y: 450
*pushButton_cnomorb_quit.width: 120
*pushButton_cnomorb_quit.height: 40
*pushButton_cnomorb_quit.labelString: "QUIT"
*pushButton_cnomorb_quit.activateCallback: {\
XtPopdown(XtParent(cnomorb_form)) ;\
}
*pushButton_cnomorb_quit.fontList: "rockwell-bold"

*textField_phase_days2.class: textField
*textField_phase_days2.static: true
*textField_phase_days2.name: textField_phase_days2
*textField_phase_days2.parent: CreateNominalOrbit
*textField_phase_days2.isCompound: "true"
*textField_phase_days2.compoundIcon: "textfield.xpm"
*textField_phase_days2.compoundName: "text_Field"
*textField_phase_days2.x: 150
*textField_phase_days2.y: 290
*textField_phase_days2.height: 30
*textField_phase_days2.cursorPositionVisible: "false"
*textField_phase_days2.editable: "false"
*textField_phase_days2.sensitive: "false"
*textField_phase_days2.columns: 5
*textField_phase_days2.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_phase_days2.traversalOn: "false"

*textField_cycle_days2.class: textField
*textField_cycle_days2.static: true
*textField_cycle_days2.name: textField_cycle_days2
*textField_cycle_days2.parent: CreateNominalOrbit
*textField_cycle_days2.isCompound: "true"
*textField_cycle_days2.compoundIcon: "textfield.xpm"
*textField_cycle_days2.compoundName: "text_Field"
*textField_cycle_days2.x: 150
*textField_cycle_days2.y: 320
*textField_cycle_days2.height: 30
*textField_cycle_days2.cursorPositionVisible: "false"
*textField_cycle_days2.editable: "false"
*textField_cycle_days2.sensitive: "false"
*textField_cycle_days2.columns: 5
*textField_cycle_days2.resizeWidth: "false"
*textField_cycle_days2.maxLength: 5
*textField_cycle_days2.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_cycle_days2.traversalOn: "false"

*textField_cycle_revs2.class: textField
*textField_cycle_revs2.static: true
*textField_cycle_revs2.name: textField_cycle_revs2
*textField_cycle_revs2.parent: CreateNominalOrbit
*textField_cycle_revs2.isCompound: "true"
*textField_cycle_revs2.compoundIcon: "textfield.xpm"
*textField_cycle_revs2.compoundName: "text_Field"
*textField_cycle_revs2.x: 150
*textField_cycle_revs2.y: 350
*textField_cycle_revs2.height: 30
*textField_cycle_revs2.cursorPositionVisible: "false"
*textField_cycle_revs2.editable: "false"
*textField_cycle_revs2.sensitive: "false"
*textField_cycle_revs2.columns: 5
*textField_cycle_revs2.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_cycle_revs2.traversalOn: "false"

*label23.class: label
*label23.static: true
*label23.name: label23
*label23.parent: CreateNominalOrbit
*label23.isCompound: "true"
*label23.compoundIcon: "label.xpm"
*label23.compoundName: "label_"
*label23.x: 45
*label23.y: 70
*label23.width: 600
*label23.height: 20
*label23.labelString: "                                                REPEAT CYCLE"
*label23.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label23.alignment: "alignment_beginning"

*textField_phase_lon2.class: textField
*textField_phase_lon2.static: true
*textField_phase_lon2.name: textField_phase_lon2
*textField_phase_lon2.parent: CreateNominalOrbit
*textField_phase_lon2.isCompound: "true"
*textField_phase_lon2.compoundIcon: "textfield.xpm"
*textField_phase_lon2.compoundName: "text_Field"
*textField_phase_lon2.x: 450
*textField_phase_lon2.y: 380
*textField_phase_lon2.height: 30
*textField_phase_lon2.cursorPositionVisible: "false"
*textField_phase_lon2.editable: "false"
*textField_phase_lon2.sensitive: "false"
*textField_phase_lon2.columns: 10
*textField_phase_lon2.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_phase_lon2.traversalOn: "false"

*label24.class: label
*label24.static: true
*label24.name: label24
*label24.parent: CreateNominalOrbit
*label24.isCompound: "true"
*label24.compoundIcon: "label.xpm"
*label24.compoundName: "label_"
*label24.x: 70
*label24.y: 270
*label24.width: 160
*label24.height: 20
*label24.labelString: "PHASE INFORMATION"
*label24.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"

*label26.class: label
*label26.static: true
*label26.name: label26
*label26.parent: CreateNominalOrbit
*label26.isCompound: "true"
*label26.compoundIcon: "label.xpm"
*label26.compoundName: "label_"
*label26.x: 260
*label26.y: 270
*label26.width: 355
*label26.height: 20
*label26.labelString: "MEAN ORBITAL ELEMENTS AT 1ST ASCENDING NODE"
*label26.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"

*separator2.class: separator
*separator2.static: true
*separator2.name: separator2
*separator2.parent: CreateNominalOrbit
*separator2.width: 667
*separator2.height: 10
*separator2.isCompound: "true"
*separator2.compoundIcon: "sep.xpm"
*separator2.compoundName: "separator_"
*separator2.x: 0
*separator2.y: 490

*label3.class: label
*label3.static: true
*label3.name: label3
*label3.parent: CreateNominalOrbit
*label3.isCompound: "true"
*label3.compoundIcon: "label.xpm"
*label3.compoundName: "label_"
*label3.x: 35
*label3.y: 500
*label3.width: 590
*label3.height: 20
*label3.labelString: "MESSAGES"
*label3.fontList: "-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1"

*scrolledWindowText3.class: scrolledWindow
*scrolledWindowText3.static: true
*scrolledWindowText3.name: scrolledWindowText3
*scrolledWindowText3.parent: CreateNominalOrbit
*scrolledWindowText3.scrollingPolicy: "application_defined"
*scrolledWindowText3.visualPolicy: "variable"
*scrolledWindowText3.scrollBarDisplayPolicy: "static"
*scrolledWindowText3.isCompound: "true"
*scrolledWindowText3.compoundIcon: "scrltext.xpm"
*scrolledWindowText3.compoundName: "scrolled_Text"
*scrolledWindowText3.x: 35
*scrolledWindowText3.y: 520
*scrolledWindowText3.width: 590
*scrolledWindowText3.height: 160

*scrolledText_cnomorb_status.class: scrolledText
*scrolledText_cnomorb_status.static: true
*scrolledText_cnomorb_status.name: scrolledText_cnomorb_status
*scrolledText_cnomorb_status.parent: scrolledWindowText3
*scrolledText_cnomorb_status.width: 571
*scrolledText_cnomorb_status.height: 150
*scrolledText_cnomorb_status.editMode: "multi_line_edit"
*scrolledText_cnomorb_status.editable: "false"

*pushButton_refresh1.class: pushButton
*pushButton_refresh1.static: true
*pushButton_refresh1.name: pushButton_refresh1
*pushButton_refresh1.parent: CreateNominalOrbit
*pushButton_refresh1.isCompound: "true"
*pushButton_refresh1.compoundIcon: "push.xpm"
*pushButton_refresh1.compoundName: "push_Button"
*pushButton_refresh1.x: 12
*pushButton_refresh1.y: 97
*pushButton_refresh1.width: 25
*pushButton_refresh1.height: 110
*pushButton_refresh1.labelString: "R\nE\nF\nR\nE\nS\nH"
*pushButton_refresh1.fontList: "rockwell-bold"
*pushButton_refresh1.activateCallback.source: public
*pushButton_refresh1.activateCallback: cb_show_orbit_relations
*pushButton_refresh1.activateCallbackClientData: (XtPointer) scrolledList2 

