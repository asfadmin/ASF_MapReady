*.
*   EosSimul.inc - simul common for EosMenu
*
*	03/21/89 Original
*
*	08/02/89 15:46
*..
	logical ShowSimul,SimulOk
	double precision tBias
	integer MAXSIMUL
	parameter (MAXSIMUL=2)
	integer simplat(MAXSIMUL),siminst(MAXSIMUL)
	integer ColSimul

	common /eos$eossimul/ ShowSimul,SimulOk,tBias,simplat,siminst,
     .				ColSimul
