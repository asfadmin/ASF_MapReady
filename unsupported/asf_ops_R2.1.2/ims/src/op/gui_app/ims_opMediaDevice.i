! UIMX ascii 2.9 key: 7420                                                      

*mediaDevice.class: form
*mediaDevice.classinc:
*mediaDevice.classspec:
*mediaDevice.classmembers:
*mediaDevice.classconstructor:
*mediaDevice.classdestructor:
*mediaDevice.gbldecl: #include <stdio.h>\
#include <ims_opCb.h>
*mediaDevice.ispecdecl:
*mediaDevice.funcdecl: swidget create_mediaDevice(swidget UxParent)
*mediaDevice.funcname: create_mediaDevice
*mediaDevice.funcdef: "swidget", "<create_mediaDevice>(%)"
*mediaDevice.argdecl: swidget UxParent;
*mediaDevice.arglist: UxParent
*mediaDevice.arglist.UxParent: "swidget", "%UxParent%"
*mediaDevice.icode:
*mediaDevice.fcode: return(rtrn);\

*mediaDevice.auxdecl:
*mediaDevice.static: true
*mediaDevice.name: mediaDevice
*mediaDevice.parent: NO_PARENT
*mediaDevice.parentExpression: UxParent
*mediaDevice.defaultShell: transientShell
*mediaDevice.height: 840
*mediaDevice.resizePolicy: "resize_none"
*mediaDevice.isCompound: "true"
*mediaDevice.compoundIcon: "form.xpm"
*mediaDevice.compoundName: "form_"
*mediaDevice.x: 134
*mediaDevice.y: 0
*mediaDevice.unitType: "pixels"
*mediaDevice.background: "#9ac0cd"
*mediaDevice.allowShellResize: "false"
*mediaDevice.width: 976

*mediaSW.class: scrolledWindow
*mediaSW.static: true
*mediaSW.name: mediaSW
*mediaSW.parent: mediaDevice
*mediaSW.scrollingPolicy: "automatic"
*mediaSW.width: 954
*mediaSW.height: 714
*mediaSW.isCompound: "true"
*mediaSW.compoundIcon: "scrlwnd.xpm"
*mediaSW.compoundName: "scrolled_Window"
*mediaSW.x: 12
*mediaSW.y: 80
*mediaSW.background: "#9ac0cd"
*mediaSW.scrollBarDisplayPolicy: "as_needed"
*mediaSW.visualPolicy: "constant"

*mediaRC.class: rowColumn
*mediaRC.static: true
*mediaRC.name: mediaRC
*mediaRC.parent: mediaSW
*mediaRC.width: 923
*mediaRC.height: 2000
*mediaRC.isCompound: "true"
*mediaRC.compoundIcon: "row.xpm"
*mediaRC.compoundName: "row_Column"
*mediaRC.y: 2
*mediaRC.packing: "pack_none"
*mediaRC.radioAlwaysOne: "false"
*mediaRC.numColumns: 8
*mediaRC.resizeWidth: "false"
*mediaRC.spacing: 0
*mediaRC.background: "#9ac0cd"
*mediaRC.entryVerticalAlignment: "alignment_center"
*mediaRC.resizeHeight: "false"

*ftpPB.class: pushButton
*ftpPB.static: true
*ftpPB.name: ftpPB
*ftpPB.parent: mediaRC
*ftpPB.isCompound: "true"
*ftpPB.compoundIcon: "push.xpm"
*ftpPB.compoundName: "push_Button"
*ftpPB.x: 4
*ftpPB.y: 6
*ftpPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*ftpPB.labelString: "Electronic FTP"
*ftpPB.shadowThickness: 3
*ftpPB.background: "LightSkyBlue3"
*ftpPB.marginLeft: 3
*ftpPB.marginRight: 0
*ftpPB.marginHeight: 4
*ftpPB.width: 174
*ftpPB.createCallbackClientData: (XtPointer) 0
*ftpPB.recomputeSize: "false"
*ftpPB.marginWidth: 0
*ftpPB.alignment: "alignment_beginning"

*ftpAvailLBL.class: label
*ftpAvailLBL.static: true
*ftpAvailLBL.name: ftpAvailLBL
*ftpAvailLBL.parent: mediaRC
*ftpAvailLBL.isCompound: "true"
*ftpAvailLBL.compoundIcon: "label.xpm"
*ftpAvailLBL.compoundName: "label_"
*ftpAvailLBL.x: 184
*ftpAvailLBL.y: 8
*ftpAvailLBL.background: "LightSteelBlue3"
*ftpAvailLBL.height: 26
*ftpAvailLBL.labelString: ""
*ftpAvailLBL.width: 103
*ftpAvailLBL.borderWidth: 2
*ftpAvailLBL.createCallbackClientData: (XtPointer) 0
*ftpAvailLBL.recomputeSize: "false"

*ftpAllocLBL.class: label
*ftpAllocLBL.static: true
*ftpAllocLBL.name: ftpAllocLBL
*ftpAllocLBL.parent: mediaRC
*ftpAllocLBL.isCompound: "true"
*ftpAllocLBL.compoundIcon: "label.xpm"
*ftpAllocLBL.compoundName: "label_"
*ftpAllocLBL.x: 288
*ftpAllocLBL.y: 8
*ftpAllocLBL.background: "LightSteelBlue3"
*ftpAllocLBL.height: 26
*ftpAllocLBL.labelString: ""
*ftpAllocLBL.width: 103
*ftpAllocLBL.borderWidth: 2
*ftpAllocLBL.createCallbackClientData: (XtPointer) 0
*ftpAllocLBL.recomputeSize: "false"

*ftpJobStartLBL.class: label
*ftpJobStartLBL.static: true
*ftpJobStartLBL.name: ftpJobStartLBL
*ftpJobStartLBL.parent: mediaRC
*ftpJobStartLBL.isCompound: "true"
*ftpJobStartLBL.compoundIcon: "label.xpm"
*ftpJobStartLBL.compoundName: "label_"
*ftpJobStartLBL.x: 392
*ftpJobStartLBL.y: 8
*ftpJobStartLBL.background: "LightSteelBlue3"
*ftpJobStartLBL.height: 26
*ftpJobStartLBL.labelString: ""
*ftpJobStartLBL.width: 103
*ftpJobStartLBL.borderWidth: 2
*ftpJobStartLBL.createCallbackClientData: (XtPointer) 0
*ftpJobStartLBL.recomputeSize: "false"

*ftpQualLBL.class: label
*ftpQualLBL.static: true
*ftpQualLBL.name: ftpQualLBL
*ftpQualLBL.parent: mediaRC
*ftpQualLBL.isCompound: "true"
*ftpQualLBL.compoundIcon: "label.xpm"
*ftpQualLBL.compoundName: "label_"
*ftpQualLBL.x: 496
*ftpQualLBL.y: 8
*ftpQualLBL.background: "LightSteelBlue3"
*ftpQualLBL.height: 26
*ftpQualLBL.labelString: ""
*ftpQualLBL.width: 103
*ftpQualLBL.borderWidth: 2
*ftpQualLBL.createCallbackClientData: (XtPointer) 0
*ftpQualLBL.recomputeSize: "false"

*ftpJobDoneLBL.class: label
*ftpJobDoneLBL.static: true
*ftpJobDoneLBL.name: ftpJobDoneLBL
*ftpJobDoneLBL.parent: mediaRC
*ftpJobDoneLBL.isCompound: "true"
*ftpJobDoneLBL.compoundIcon: "label.xpm"
*ftpJobDoneLBL.compoundName: "label_"
*ftpJobDoneLBL.x: 600
*ftpJobDoneLBL.y: 8
*ftpJobDoneLBL.background: "LightSteelBlue3"
*ftpJobDoneLBL.height: 26
*ftpJobDoneLBL.labelString: ""
*ftpJobDoneLBL.width: 103
*ftpJobDoneLBL.borderWidth: 2
*ftpJobDoneLBL.createCallbackClientData: (XtPointer) 0
*ftpJobDoneLBL.recomputeSize: "false"

*ftpJobFailLBL.class: label
*ftpJobFailLBL.static: true
*ftpJobFailLBL.name: ftpJobFailLBL
*ftpJobFailLBL.parent: mediaRC
*ftpJobFailLBL.isCompound: "true"
*ftpJobFailLBL.compoundIcon: "label.xpm"
*ftpJobFailLBL.compoundName: "label_"
*ftpJobFailLBL.x: 704
*ftpJobFailLBL.y: 8
*ftpJobFailLBL.background: "LightSteelBlue3"
*ftpJobFailLBL.height: 26
*ftpJobFailLBL.labelString: ""
*ftpJobFailLBL.width: 103
*ftpJobFailLBL.borderWidth: 2
*ftpJobFailLBL.createCallbackClientData: (XtPointer) 0
*ftpJobFailLBL.recomputeSize: "false"

*ftpOfflineLBL.class: label
*ftpOfflineLBL.static: true
*ftpOfflineLBL.name: ftpOfflineLBL
*ftpOfflineLBL.parent: mediaRC
*ftpOfflineLBL.isCompound: "true"
*ftpOfflineLBL.compoundIcon: "label.xpm"
*ftpOfflineLBL.compoundName: "label_"
*ftpOfflineLBL.x: 808
*ftpOfflineLBL.y: 8
*ftpOfflineLBL.background: "LightSteelBlue3"
*ftpOfflineLBL.height: 26
*ftpOfflineLBL.labelString: ""
*ftpOfflineLBL.width: 103
*ftpOfflineLBL.borderWidth: 2
*ftpOfflineLBL.createCallbackClientData: (XtPointer) 0
*ftpOfflineLBL.recomputeSize: "false"

*ftpSW.class: scrolledWindow
*ftpSW.static: true
*ftpSW.name: ftpSW
*ftpSW.parent: mediaRC
*ftpSW.scrollingPolicy: "application_defined"
*ftpSW.visualPolicy: "variable"
*ftpSW.scrollBarDisplayPolicy: "static"
*ftpSW.isCompound: "true"
*ftpSW.compoundIcon: "scrltext.xpm"
*ftpSW.compoundName: "scrolled_Text"
*ftpSW.x: 6
*ftpSW.y: 40
*ftpSW.width: 890
*ftpSW.background: "LightSkyBlue3"
*ftpSW.height: 100

*ftpST.class: scrolledText
*ftpST.static: true
*ftpST.name: ftpST
*ftpST.parent: ftpSW
*ftpST.width: 892
*ftpST.height: 100
*ftpST.background: "LightSkyBlue3"
*ftpST.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*ftpST.text: ""
*ftpST.scrollHorizontal: "false"
*ftpST.editable: "false"
*ftpST.editMode: "multi_line_edit"
*ftpST.cursorPositionVisible: "false"
*ftpST.wordWrap: "true"
*ftpST.valueWcs: ""

*label189.class: label
*label189.static: true
*label189.name: label189
*label189.parent: mediaDevice
*label189.isCompound: "true"
*label189.compoundIcon: "label.xpm"
*label189.compoundName: "label_"
*label189.x: 360
*label189.y: 8
*label189.background: "#9ac0cd"
*label189.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label189.labelString: "Media  Device  Status  Screen"
*label189.height: 28
*label189.width: 284

*label190.class: label
*label190.static: true
*label190.name: label190
*label190.parent: mediaDevice
*label190.isCompound: "true"
*label190.compoundIcon: "label.xpm"
*label190.compoundName: "label_"
*label190.x: 196
*label190.y: 48
*label190.background: "#50a0a0"
*label190.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label190.labelString: "Available"
*label190.borderWidth: 2
*label190.height: 25
*label190.width: 104

*label191.class: label
*label191.static: true
*label191.name: label191
*label191.parent: mediaDevice
*label191.isCompound: "true"
*label191.compoundIcon: "label.xpm"
*label191.compoundName: "label_"
*label191.x: 300
*label191.y: 48
*label191.background: "#50a0a0"
*label191.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label191.labelString: "Allocated"
*label191.borderWidth: 2
*label191.height: 25
*label191.width: 107

*label192.class: label
*label192.static: true
*label192.name: label192
*label192.parent: mediaDevice
*label192.isCompound: "true"
*label192.compoundIcon: "label.xpm"
*label192.compoundName: "label_"
*label192.x: 408
*label192.y: 48
*label192.background: "#50a0a0"
*label192.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label192.labelString: "Job Started"
*label192.borderWidth: 2
*label192.height: 25
*label192.width: 103

*label193.class: label
*label193.static: true
*label193.name: label193
*label193.parent: mediaDevice
*label193.isCompound: "true"
*label193.compoundIcon: "label.xpm"
*label193.compoundName: "label_"
*label193.x: 512
*label193.y: 48
*label193.background: "#50a0a0"
*label193.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label193.labelString: "Qual Check"
*label193.borderWidth: 2
*label193.height: 25
*label193.width: 103

*label194.class: label
*label194.static: true
*label194.name: label194
*label194.parent: mediaDevice
*label194.isCompound: "true"
*label194.compoundIcon: "label.xpm"
*label194.compoundName: "label_"
*label194.x: 616
*label194.y: 48
*label194.background: "#50a0a0"
*label194.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label194.labelString: "Job Done"
*label194.borderWidth: 2
*label194.height: 25
*label194.width: 103

*label195.class: label
*label195.static: true
*label195.name: label195
*label195.parent: mediaDevice
*label195.isCompound: "true"
*label195.compoundIcon: "label.xpm"
*label195.compoundName: "label_"
*label195.x: 720
*label195.y: 48
*label195.background: "#50a0a0"
*label195.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label195.labelString: "Job Failed"
*label195.borderWidth: 2
*label195.height: 25
*label195.width: 103

*label196.class: label
*label196.static: true
*label196.name: label196
*label196.parent: mediaDevice
*label196.isCompound: "true"
*label196.compoundIcon: "label.xpm"
*label196.compoundName: "label_"
*label196.x: 824
*label196.y: 48
*label196.background: "#50a0a0"
*label196.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label196.labelString: "Off-Line"
*label196.borderWidth: 2
*label196.height: 25
*label196.width: 100

*closePB.class: pushButton
*closePB.static: true
*closePB.name: closePB
*closePB.parent: mediaDevice
*closePB.isCompound: "true"
*closePB.compoundIcon: "push.xpm"
*closePB.compoundName: "push_Button"
*closePB.x: 408
*closePB.y: 796
*closePB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*closePB.labelString: "CLOSE"
*closePB.shadowThickness: 4
*closePB.background: "cadetBlue"
*closePB.marginLeft: 2
*closePB.marginRight: 2
*closePB.marginHeight: 4
*closePB.width: 156
*closePB.height: 36
*closePB.activateCallback.source: public
*closePB.activateCallback: mediaDevice_closeCb

