#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

#ifndef LIST_HANDLING_H
#define LIST_HANDLING_H
 
/*==============================================================================
Filename:    list_handling.h
Description: defines the node structures for the double linked list used to
               keep track of GRS points
Creator:     Brian J Griglak
Notes:      
==============================================================================*/
#pragma ident	"@(#)list_handling.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.list_handling.h"

struct rownode {
    int value;
    struct rownode *next;
};

struct pathnode {
    int  value;
    struct rownode *head;
    struct pathnode *next;
};

typedef struct rownode    ROW;
typedef struct pathnode   PATH;

void list_next(PATH **path, ROW **row) ;
void free_list(PATH *head) ;
void print_list(PATH *head) ;
void insert_node(PATH *path, ROW *row, int pathval, int rowval) ;
void initialize(PATH *list) ;
int delete_node(PATH *head, int pathval, int rowval) ;
int count_list(PATH *head) ;

#endif  /* LIST_HANDLING_H */
