#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       list_handling.c
 
Description:  Contains structure definitions for linked list structure to
                handle the collection of GRS points, and functions to
                manipulate the structure.
              You have a linked list of pathnodes, and each pathnode
                also points to the head of a linked list of rownodes.
              Hence you pass down the path list and across the corresponding
                row list to see what path,row pairs are present
 
External Functions Defined:
list_next()
print_list()
insert_node()
initialize()
 
Notes:
        This file written with a 4-character tab setting. 
==============================================================================*/
#pragma ident	"@(#)list_handling.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.list_handling.c"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "list_handling.h"


/*==============================================================================
Function:       free_list()

Description:    free the memory allocated in the entire grslist.  

Creator:        Lawrence Stevens

Creation Date:  Thu Sep 18 20:55:36 PDT 1997

==============================================================================*/
void free_list( PATH *grs_list ) 
{
    PATH  *path, *next_path_save ;
    ROW   *row, *next_row_save ;
 
    /* for each path in the GRS list:   */
    path = grs_list ;
    while( path != NULL )
    {
        /* 
        -- save pointer to next path 
        -- before free()  
        */
        next_path_save = path->next ;


        /* for each row in the path list:   */
        row = path->head ;
        while( row != NULL )
        {
            /* 
            -- save pointer to next row 
            -- before free()  
            */
            next_row_save = row->next ;

            /* FREE  */
            free( row ) ;

            /* 
            -- use saved next row  
            -- to get to next:  
            */
            row = next_row_save ; 
            /*next row */

        } /* while loop for each row.  */


        /* 
        -- all of the rows were freed.  
        -- now free this path.  
        */
        /* FREE  */
        free( path ) ;

        /* 
        -- use saved next path to get to next:  
        */
        path = next_path_save ; 
        /*next  */

    }  /* while loop for each path.  */

    return;
}



/*==============================================================================
Function:      list_next()

Description:   Given pointers to a point in the doubly sorted list of paths
                 and rows, this function advances to the next point in the
                 list, and returns the proper pointers.  It also increments
                 the count to tell you what point number you are at, provided
                 you start with count = -1 at the dummy pathnode head structure

Parameters:
    Input Parameters:
    path         ** struct pathnode    double pointer to the current pathnode
                                         (need double so you can return updated
                                         pointer adress)

    row          ** struct rownode     double pointer to the current rownode

    Output Parameters:
    path         ** struct pathnode    double pointer to the new pathnode
                                         (possibly the same node)

    row          ** struct rownode     double pointer to the new rownode

Returns:       void

Creator:       Brian J Griglak

Creation Date: Wed Jul  9 19:26:05 PDT 1997
==============================================================================*/

void list_next(PATH **path, ROW **row)
{

    if ((*row) == NULL)
    {
        *path = (*path)->next;
        if (*path != NULL)
            *row = (*path)->head;
    }
    else if (((*path) -> next == NULL) && ((*row) -> next == NULL))
    {
        *path = NULL;
        *row = NULL;
    }
    else if ((*row) -> next == NULL)
    {
        *path = (*path) -> next;
        *row = (*path) -> head;
    }
    else 
    {
        *row = (*row) -> next;
    }

    return;
}


/*==============================================================================
Function:      print_list()

Description:   Given pointer to the head of the doubly sorted list of paths
                 and rows, this function prints out the collection of pairs
                 in the list, sorted first by path, then by row.  Pairs with
                 the same path are output to the same line.  A carriage return
                 is added when a new path number comes along to improve
                 readability of the list.

Parameters:
    Input Parameters:
    head         * struct pathnode     pointer to the head node in the pathnode
                                         list

    Output Parameters:
    none, but there is output to stdout 

Returns:       void

Creator:       Brian J Griglak

Creation Date: Thu Jul 10 10:37:33 PDT 1997
==============================================================================*/

void print_list(PATH *head)
{
    PATH *path;
    ROW *row;

    if (head -> next == NULL)
    {
      (void) printf("No nodes in list yet.\n\n");
      return;
    }
    else
    {
        path = head -> next;
        row = path -> head;
    }

    while (path != NULL)
    {
        if (path -> head == NULL)
            list_next(&path, &row);
        else if (row -> next == NULL)
    {
            (void) printf(" (%d , %d)\n", path -> value, row -> value);
            list_next(&path, &row);
    }
        else
        {
            (void) printf(" (%d , %d),",path -> value, row -> value);
            list_next(&path, &row);
    }
    }

    (void) printf("\ndone.\n");
}


/*==============================================================================
Function:      insert_node()

Description:   Given pointers to the head of the doubly sorted list of paths
                 and rows, this function travels down through the list and
                 inserts the new path,row pair in the correct location.  If
                 such a point already exists, there is no need for insertion,
                 and the routine returns.
               From an external usage point, it is easiest to pass the dummy
                 header of the pathnode list, and NULL for the rownode pointer,
                 the routine will handle it from there.

Parameters:
    Input Parameters:
    path         * struct pathnode     pointer to the current pathnode

    row          * struct rownode      pointer to the current rownode

    pathval      int                   value of new path point

    rowval       int                   value of new row point

    Output Parameters:
    none, but the list is updated

Returns:       void

Creator:       Brian J Griglak

Creation Date: Thu Jul 10 10:45:50 PDT 1997
==============================================================================*/

void insert_node(PATH *path, ROW *row, int pathval, int rowval)
{
    PATH *newpath;
    ROW *newrow;

    if ((pathval == 0) && (rowval == 0))    /*  flag from latlon2grs that */
        return;                             /*  point is out of GRS range */

    if (path == NULL)
    {
        newrow = malloc(sizeof(ROW));
        path -> value = pathval;
        path -> next = NULL;
        newrow -> value = rowval;    
        newrow -> next = NULL;
        path -> head = newrow;
    }
    else if (path -> value == pathval)
    {
        if (row -> value == rowval)
            return;     /* OK.  redundant.  */
        else if (row -> value > rowval)
        {
            /* insert into this list of rows.  */
            newrow = malloc(sizeof(ROW));
            newrow -> value = row -> value;
            newrow -> next = row -> next;
            row -> value = rowval;
            row -> next = newrow;
        }
        else
        {
            if (row->next == NULL)
            {
                /* add onto the end of this list of rows.  */
                newrow = malloc(sizeof(ROW));
                newrow -> value = rowval;
                newrow -> next = NULL;
                row -> next = newrow;
            }
            else   /* not yet found insert, recursive call:  */
                insert_node(path, row -> next, pathval, rowval);
        }
    }
    else if ( path->value > pathval)
    {
        /* insert a new path in to path list:  */
        newpath = malloc(sizeof(PATH));
        newrow = malloc(sizeof(ROW));

        /* new path gets existing path values:  */
        newpath->value = path -> value;
        newpath->head = path -> head;
        newpath->next = path -> next;

        /* old path gets the new path values:  */
        path->value = pathval;
        path->next = newpath;
        path->head = newrow;

        /* new row entry:   */
        newrow->value = rowval;
        newrow->next = NULL;
    }
    else
    {
        if (path -> next == NULL)
        {
            /* add new path onto the end of path list:  */
            newpath = malloc(sizeof(PATH));
            newrow = malloc(sizeof(ROW));
            newpath -> value = pathval;
            newpath -> head = newrow;
            newpath -> next = NULL;
            path -> next = newpath;
            newrow -> value = rowval;
            newrow -> next = NULL;
        }
        else
            insert_node(path -> next, path -> next -> head, pathval, rowval);
    }
} 


/*==============================================================================
Function:      initialize()

Description:   Given pointer to the dummy head of the doubly sorted list of
                 paths and rows, this function initializes the internal data.
               Value is set to 0 so that this node remains the head node 
               Head is set to NULL, and never changes, in this case it is
                 like excess baggage
               Next is set to NULL, since there are no other nodes yet, and
                 the other routines use the NULL flag to indicate that

Parameters:
    Input Parameters:
    list         * struct pathnode     pointer to the dummy pathnode header

    Output Parameters:
    none

Returns:       void

Creator:       Brian J Griglak

Creation Date: Thu Jul 10 10:50:58 PDT 1997
==============================================================================*/

void initialize(PATH *list)
{
    list -> value = 0;
    list -> next = NULL;
    list -> head = NULL;
}


/*==============================================================================
Function:      delete_node()

Description:   Given pointer to the head of the list structure, and the path
                 and row values that are desired to be deleted, this will
                 go through the list and delete the node if it exists. If
                 the node does not exist, it returns 0, if the node did
                 exist, it returns 1, telling how many nodes were deleted.

Parameters:
    Input Parameters:
    head         * struct pathnode     pointer to the head of the list

    pat, row     int                   the path and row values that you want
                                         to delete

    Output Parameters:
    none

Returns:       int

Creator:       Brian J Griglak

Creation Date: Tue Jul 22 14:18:57 PDT 1997
==============================================================================*/

int delete_node(PATH *head, int pathval, int rowval)
{
    PATH *path, *prepath;
    ROW  *row, *prerow;

    /*
    --- Check to see if list is empty.  If it is, return 0, since there's
    --- nothing to delete.
    */

    if (head -> next == NULL)
        return 0;

    /*
    --- Now traverse down the path list until you either get to the path
    --- value, or pass by where it would be if it existed.
    */

    path = head -> next;
    prepath = head;

    while (path -> value < pathval)
    {
        if (path -> next == NULL)
            return 0;
        prepath = path;
        path = prepath -> next;
    }

    if (path -> value > pathval)
        return 0;

    /*
    --- Do same thing for row list linked to the path head.
    */

    row = path -> head;
    prerow = NULL;

    while (row -> value < rowval)
    {
        if (row -> next == NULL)
            return 0;
        prerow = row;
        row = prerow -> next;
    }

    if (row -> value > rowval)
        return 0;

    /* 
    -- at this point, the desired path and row have been found:
    --     path->value  == pathval
    --     row->value == rowval
    */

    /*
    --- See if the head row is being deleted.  If it is, you need to update
    --- either the path head pointer, or the path next pointer if this is
    --- the only node on this path branch.  Free up the memory used by
    --- the deleted node too, so you don't use up too much memory on a
    --- number of large lists being processed.
    */

    if (path->head == row)
    {
        /* the head row is being deleted.  */
        if (row->next == NULL)
        {
            /* the end of the row list is being deleted.  */

            /* 
            -- this is the last of the path.  
            -- free BOTH the row entry and the 
            -- path, too.  
            */
            free(row);
            prepath -> next = path -> next;
            free(path);
            return 1;
        }
        else
        {
            path -> head = row -> next;
            free(row);
            return 1;
        }
    }
    else
    {
        prerow -> next = row -> next;
        free(row);
        return 1;
    }
}


/*==============================================================================
Function:      count_list()

Description:   Given the pointer to the head of the list structure, this goes
                 through the list and returns a count of how many nodes are
                 in it.

Parameters:
    Input Parameters:
    head         * struct pathnode     pointer to the pathnode header

    Output Parameters:
    none

Returns:       int

Creator:       Brian J Griglak

Creation Date: Tue Jul 22 14:39:45 PDT 1997
==============================================================================*/

int count_list(PATH *head)
{
    PATH *p;
    ROW *r;
    int count = -1;

    p = head;
    r = NULL;

    while (p != NULL)
    {
        list_next(&p, &r);
        count++;
    }

    return count;
}
