static char sccsid_serverSocketXport_h[] = "@(#)serverSocketXport.h	4.3 95/03/24 18:50:53";

#ifndef _serverSocketXport_h__
#define _serverSocketXport_h__
/*************************************
* serverSocketXport.h
*
*************************************/
ODL ReadMsgFromClient(int sockfd, int *errStatusPtr);
int WriteMsgToClient(int sockfd, ODL ODLmsg);


#endif			/* !_serverSocketXport_h_ */
