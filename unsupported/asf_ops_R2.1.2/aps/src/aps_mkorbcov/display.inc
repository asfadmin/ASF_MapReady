*.
*   Display.inc - display variables
*
*   Commons
*	eos$display
*
*   History
*	 3/20/91 Added DisplayName()
*	 8/ 2/89
*
*   Last modified
*	3/20/91 13:45
*..

c Display devices

	integer RAM9460,VT100,VT220,VT240,JUP7,JUPJ
	integer TEK4010,TEK4010SEL,TEK4014,TEK4014SEL
	integer TEK4027,TEK4105MAC,TEK4208,PRINTX,POSTSCRIPT,NONE,DEVICES
	integer disp				! selected display device

	parameter (RAM9460=1,VT100=2,VT220=3,VT240=4,JUP7=5,JUPJ=6)
	parameter (TEK4010=7,TEK4010SEL=8,TEK4014=9,TEK4014SEL=10)
	parameter (TEK4027=11,TEK4105MAC=12,TEK4208=13,PRINTX=14)
	parameter (POSTSCRIPT=15,NONE=16)
	parameter (DEVICES=16)

	character*30 DisplayName(DEVICES)
	data DisplayName(RAM9460)	/'Ramtek 9460'/
	data DisplayName(VT100)		/'VT100 (Selanar GR100)'/
	data DisplayName(VT220)		/'VT220 (Selanar SG220)'/
	data DisplayName(VT240)		/'VT240 (ReGIS)'/
	data DisplayName(JUP7)		/'Jupiter7'/
	data DisplayName(JUPJ)		/'Jupiter J-Station'/
	data DisplayName(TEK4010)	/'Tek 4010'/
	data DisplayName(TEK4010SEL)	/'Tek 4010 (Selanar GR100)'/
	data DisplayName(TEK4014)	/'Tek 4014'/
	data DisplayName(TEK4014SEL)	/'Tek 4014 (Selanar Hirez)'/
	data DisplayName(TEK4027)	/'Tek 4027'/
	data DisplayName(TEK4105MAC)	/'Tek 4105 (Mac)'/
	data DisplayName(TEK4208)	/'Tek 4208'/
	data DisplayName(PRINTX)	/'Printronix line printer'/
	data DisplayName(POSTSCRIPT)	/'PostScript'/
	data DisplayName(NONE)		/'None'/

* Screen sizes

	integer RAM9460X,RAM9460Y,VT100X,VT100Y
	integer VT220X,VT220Y,VT240X,VT240Y
	integer JUP7X,JUP7Y,JUPJX,JUPJY
	integer TEK4010X,TEK4010Y,TEK4010SELX,TEK4010SELY
	integer TEK4014X,TEK4014Y,TEK4014SELX,TEK4014SELY
	integer TEK4027X,TEK4027Y
	integer TEK4105MACX,TEK4105MACY,TEK4208X,TEK4208Y
	integer PRINTXX,PRINTXY,POSTSCRIPTX,POSTSCRIPTY

	parameter(RAM9460X=1279,RAM9460Y=1023,VT100X=1224,VT100Y=239)
	parameter(VT220X=4095,VT220Y=3119,VT240X=799,VT240Y=479)
	parameter(JUP7X=639,JUP7Y=482,JUPJX=1279,JUPJY=1023)
	parameter(TEK4010X=4095,TEK4010Y=3119)
	parameter(TEK4010SELX=4094,TEK4010SELY=3119)
	parameter(TEK4014X=4095,TEK4014Y=3119)
	parameter(TEK4014SELX=4095,TEK4014SELY=3119)
	parameter(TEK4027X=639,TEK4027Y=308)
	parameter(TEK4105MACX=4095,TEK4105MACY=3130)
	parameter(TEK4208X=4095,TEK4208Y=3130,PRINTXX=779,PRINTXY=779)
	parameter(POSTSCRIPTX=10000,POSTSCRIPTY=7500)

c Display attributes

	integer FormLen(DEVICES)
	data FormLen(RAM9460)/24/, FormLen(VT100)/24/
	data FormLen(VT220)/24/,   FormLen(VT240)/24/
	data FormLen(JUP7)/24/,    FormLen(JUPJ)/24/
	data FormLen(TEK4010)/38/, FormLen(TEK4010SEL)/24/
	data FormLen(TEK4014)/38/, FormLen(TEK4014SEL)/24/
	data FormLen(TEK4027)/12/
	data FormLen(TEK4105MAC)/24/, FormLen(TEK4208)/24/
	data FormLen(PRINTX)/24/,  FormLen(POSTSCRIPT)/24/
	data FormLen(NONE)/24/

	logical TwoMonitors(DEVICES)
	data TwoMonitors(RAM9460)/.true./,  TwoMonitors(VT100)/.false./
	data TwoMonitors(VT220)/.false./,   TwoMonitors(VT240)/.false./
	data TwoMonitors(JUP7)/.true./,     TwoMonitors(JUPJ)/.true./
	data TwoMonitors(TEK4010)/.false./, TwoMonitors(TEK4010SEL)/.false./
	data TwoMonitors(TEK4014)/.false./, TwoMonitors(TEK4014SEL)/.false./
	data TwoMonitors(TEK4027)/.true./
	data TwoMonitors(TEK4105MAC)/.true./,  TwoMonitors(TEK4208)/.true./
	data TwoMonitors(PRINTX)/.true./,   TwoMonitors(POSTSCRIPT)/.true./
	data TwoMonitors(NONE)/.true./

	logical GraphicText(DEVICES)
	data GraphicText(RAM9460)/.false./, GraphicText(VT100)/.false./
	data GraphicText(VT220)/.false./,   GraphicText(VT240)/.true./
	data GraphicText(JUP7)/.false./,    GraphicText(JUPJ)/.false./
	data GraphicText(TEK4010)/.true./,  GraphicText(TEK4010SEL)/.false./
	data GraphicText(TEK4014)/.true./,  GraphicText(TEK4014SEL)/.false./
	data GraphicText(TEK4027)/.false./
	data GraphicText(TEK4105MAC)/.false./, GraphicText(TEK4208)/.false./
	data GraphicText(PRINTX)/.false./,  GraphicText(POSTSCRIPT)/.false./
	data GraphicText(NONE)/.false./

c Scaling

	integer screenx(2),screeny(2)		! screen boundary coordinates
	integer swidex,swidey			! screen width, height
	double precision xPageMult,yPageMult		! scale factors (sizes image)
	double precision devscalex,devscaley		! for x,y pixel density diffs
	double precision zoom				! zoom factor

c Line Styles

	integer StyleTable(0:255,4)		! line style lookup table
	double precision StyleTableR(0:255,2)

**
*   Escape sequences
*
*	So that I don't have to remember how many characters are in a
*	given escape sequence, I use character strings instead of character
*	arrays.  The pain then is in the data statements because escape
*	characters (you know, like ESC) must be given in their ASCII decimal
*	equivalent, but mixing numbers and characters in the data statement
*	is not allowed:
*
*	character esc1*2
*	data esc1 /27,'1'/			! no good!
*	data esc1 /27,49/			! no good!
*	data esc1(1:1),esc1(2:) /27,'1'/	! ok
*
*	This way you only have do it once and can then forget all of your
*	suffering.
**

c VT100 Escape Sequences

	character*2 esc1,esc2,escb,escc,escsq,escdq
	character*2 esck,escm,esca,escff,esco
	character*7 eschcpy
	character ff

	data esc1(1:1),esc1(2:) /27,'1'/
	data esc2(1:1),esc2(2:) /27,'2'/
	data escb(1:1),escb(2:) /27,'B'/
	data escc(1:1),escc(2:) /27,'C'/
	data escsq(1:1),escsq(2:) /27,39/
	data escdq(1:1),escdq(2:) /27,'"'/
	data esck(1:1),esck(2:) /27,'K'/
	data escm(1:1),escm(2:) /27,'-'/
	data esca(1:1),esca(2:) /27,'*'/
	data escff(1:1),escff(2:) /27,12/
	data esco(1:1),esco(2:) /27,'O'/
	data eschcpy(1:1),eschcpy(2:) /27,';1W2HD'/
	data ff /12/

c VT220 Escape Sequences

	character*4 esc4i,esc5i
	character*2 escetb,escast
	character gs

	data esc4i(1:1),esc4i(2:) /27,'[4i'/
	data esc5i(1:1),esc5i(2:) /27,'[5i'/
	data escetb(1:1),escetb(2:) /27,23/
	data escast(1:1),escast(2:) /27,'*'/
	data gs /29/

c VT240 Escape Sequences

	character regison*3,regisoff*2
	data regison(1:1),regison(2:) /27,'Pp'/
	data regisoff(1:1),regisoff(2:) /27,'\'/

c VT640 Escape Sequences

	character us
	data us /31/

c Printronix Virtual Screen

	logical*1 PxVirtScreen(0:779,0:779)

c PostScript(R)

	integer PSPageNum

c Unit Numbers

	integer UnitVT,UnitPx,UnitPS,UnitNone
	data UnitVT/25/,UnitPx/27/,UnitPS/29/,UnitNone/31/
	integer UnitAdd,UnitStn
	data UnitAdd/33/,UnitStn/34/

c Commons

	common /eos$display/ xPageMult,yPageMult,DevScalex,DevScaley,zoom,
     .				StyleTableR,
     .				disp,screenx,screeny,swidex,swidey,
     .				StyleTable,PxVirtScreen
	common /eos$postscript/ PSPageNum
