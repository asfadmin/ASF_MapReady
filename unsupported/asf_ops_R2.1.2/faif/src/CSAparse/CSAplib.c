/*==============================================================================
Filename:       CSAplib.c

Description:    Contains the functions to create and manipulate the tree 
                structure for CSA files

External Functions:
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:
1.  May '96 - R. Hoffman
    Set root to NULL before calling yyparse() in create_tree() [for PR 859]
    (and corrected "Filename: " value in comment above)
==============================================================================*/

static char SccsFile[] = "CSAplib.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 May 1996";
static char SccsLastChanger[] = "@(#)CSAplib.c	1.2";
static char SccsState[] = "1.2";

#include<stdio.h>
#include "CSAparse.h"

extern NODEPTR root;
extern BLOCKPTR head_block;
extern FILE *yyin;
extern FILE *yyout;

/*==============================================================================
Function:       yywrap
Description:    required by yacc, and taken from the yacc library
Parameters:
Returns:        the integer value 1
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
int yywrap()
{
  return(1);
}




/*==============================================================================
Function:       yyerror
Description:    required by yacc, and taken from the yacc library
Parameters:     char *s
Returns:        prints s to stderr
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
yyerror(s)
char *s;
{
  (void) fprintf(stderr, "%s\n", s);
}




/*==============================================================================
Function:       create_tree
Description:    parses a CSA file and returns a tree with its contents
Parameters:     char *filename - the name of the file to be parsed
Returns:        NODEPTR - a pointer to the root of the tree
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
NODEPTR create_tree(char *filename)
{
  BLOCKPTR temp;

  if (strcmp(filename, "stdin") != 0)
    {
      if (!(yyin = fopen(filename, "r")))
	{
	  syslog(LOG_DEBUG, 
	     "create_tree: Input file %s could not be opened\n", filename);
	  return(NULL);
	}
    }

  root = NULL;  /* for PR 859 */

  yyparse();
  
  /* free up memory taken by BLOCKPTR's */

  while (head_block)
    {
      temp = head_block->next;
      free(head_block);
      head_block = temp;
    }

  fclose(yyin);
  
  return(root);
}




/*==============================================================================
Function:       print_tree()
Description:    prints the tree data structure
Parameters:     NODEPTR tree_root - a pointer to the root of the tree structure
                char *filename    - name of the file to be read
Returns:
Creator:        Philip Yurchuk
Creation Date:  02/15/1995
Notes:
==============================================================================*/
int print_tree(NODEPTR tree_root, char *filename)
{
  NODEPTR temp;           /* temporary pointer to a node */
  ARRAYPTR atemp;         /* */
  BLOCKPTR btemp = NULL;
  BLOCKPTR btemp2;
  int indent = 0;
  int begin_flag = 0;
  int i;

  if (strcmp(filename, "stdout") != 0)

    if (!(yyout = fopen(filename, "w")))
      {
	syslog(LOG_DEBUG, 
	   "print_tree: Output file %s could not be opened\n", filename);
	return -1;
      }

  if (tree_root == NULL)
    {
      syslog(LOG_DEBUG, 
	 "print_tree: Tree is NULL - nothing will be printed.\n");
      return -1;
    }

  temp = tree_root;
  while (temp)
    {
      switch (temp->data_type) 
	{

	case HDR_ASSIGN:
	  fprintf(yyout, ";###%s: %s\n", temp->keyword, temp->value);
	  break;

	case ASSIGN:

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  else
	    begin_flag = 0;

	  fprintf(yyout, "%s = ", temp->keyword);

	  if (temp->value) 
	    fprintf(yyout, "%s\n", temp->value);

	  else
	    fprintf(yyout, "\n");

	  break;

	case ARRAY_:

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, "array %s = %s\n", temp->keyword, temp->value);
	  atemp = temp->Array;

	  while (atemp)
	    {

	      if (begin_flag == 0)

		for (i = 0; i < indent; i++)
		  fprintf(yyout, "\t");

	      fprintf(yyout, "\t%s\n", atemp->value);
	      atemp = atemp->next;
	    }

	  if (begin_flag == 0)

		for (i = 0; i < indent; i++)
		  fprintf(yyout, "\t");

	  fprintf(yyout, "end\n");

	  break;

	case BLOCK:

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, "begin ");
	  indent++;
	  begin_flag = 1;

	  break;

	case VECTOR:

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, "%s\n", temp->vector[0]);

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, 
	     "%s\t%s\t%s\n", 
	     temp->vector[1], temp->vector[2], temp->vector[3]);

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, 
	     "%s\t%s\t%s\n", 
	     temp->vector[4], temp->vector[5], temp->vector[6]);

	  break;

	case COMMENT_:

	  fprintf(yyout, "%s\n", temp->keyword);

	} /* end 'switch (temp->data_type)' */

      if (temp->block)
	{
	  btemp2 = (BLOCKPTR) util_do_malloc(sizeof(struct block_ptr));
	  btemp2->block = temp;
	  temp = temp->block;
	  btemp2->next = NULL;
	  btemp2->prev = btemp;

	  if (btemp)
	    btemp->next = btemp2;

	  btemp = btemp2;
	}

      else if (temp->next)
	temp = temp->next;

      else
	{
	  if (btemp)
	    {
	      while (!btemp->block->next)
		{
		  if (btemp->prev)
		    {
		      btemp2 = btemp;
		      btemp = btemp->prev;
		      free(btemp2);
		      btemp->next = NULL;
		      indent--;

		      if (begin_flag == 0)

			for (i = 0; i < indent; i++)
			  fprintf(yyout, "\t");

		      fprintf(yyout, "end\n");
		    }
		  else
		    {
		      free(btemp);
		      indent--;

		      if (begin_flag == 0)

			for (i = 0; i < indent; i++)
			  fprintf(yyout, "\t");

		      fprintf(yyout, "end\n");

		      return 0;

		    }
		}

	      temp = btemp->block->next;
	      indent--;

	      if (begin_flag == 0)

		for (i = 0; i < indent; i++)
		  fprintf(yyout, "\t");

              fprintf(yyout, "end\n");
	      btemp2 = btemp;
	      btemp = btemp->prev;
	      free(btemp2);

	      if (btemp)
		btemp->next = NULL;
	    } /* end 'if (btemp)' */

	  else
	    break;

	} /* end 'else' */
    } /* end 'while (temp)' */

  fclose(yyout);
  
} /* end 'print_tree' */
 







/*==============================================================================
Function:       print_tree_2()
Description:    prints the tree data structure
Parameters:     NODEPTR tree_root - a pointer to the root of the tree structure
                char *filename    - name of the file to be read
Returns:
Creator:        Philip Yurchuk
Creation Date:  02/15/1995
Notes:          Modified 970220, per valuable suggestions from
                Phil Yurchuk. The routine now adds text strings on
                some of the *end* statements in the output file.
==============================================================================*/
int print_tree_2(NODEPTR tree_root, char *filename)
{
  NODEPTR temp;           /* temporary pointer to a node */
  ARRAYPTR atemp;         /* */
  BLOCKPTR btemp = NULL;
  BLOCKPTR btemp2;
  int indent = 0;
  int begin_flag = 0;
  int i;

  char array_keyword[80];
  char block_keyword[2][80];
  char block_value[2][80];

  if (strcmp(filename, "stdout") != 0)

    if (!(yyout = fopen(filename, "w")))
      {
	syslog(LOG_DEBUG, 
	   "print_tree: Output file %s could not be opened\n", filename);
	return -1;
      }

  if (tree_root == NULL)
    {
      syslog(LOG_DEBUG, 
	 "print_tree: Tree is NULL - nothing will be printed.\n");
      return -1;
    }

  temp = tree_root;
  while (temp)
    {
      switch (temp->data_type) 
	{

	case HDR_ASSIGN:
	  fprintf(yyout, ";###%s: %s\n", temp->keyword, temp->value);
	  break;

	case ASSIGN:

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  else
	  {
	    begin_flag = 0;
            strcpy(block_keyword[indent], temp->keyword);
            strcpy(block_value[indent]  , temp->value  );
	  }

	  fprintf(yyout, "%s = ", temp->keyword);

	  if (temp->value) 
	    fprintf(yyout, "%s\n", temp->value);

	  else
	    fprintf(yyout, "\n");

	  break;

	case ARRAY_:

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, "array %s = %s\n", temp->keyword, temp->value);
	  atemp = temp->Array;
          strcpy(array_keyword, temp->keyword);

	  while (atemp)
	    {

	      if (begin_flag == 0)

		for (i = 0; i < indent; i++)
		  fprintf(yyout, "\t");

	      fprintf(yyout, "\t%s\n", atemp->value);
	      atemp = atemp->next;
	    }

	  if (begin_flag == 0)

		for (i = 0; i < indent; i++)
		  fprintf(yyout, "\t");

	  fprintf(yyout, "end %s\n", array_keyword);

	  break;

	case BLOCK:

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, "begin ");
	  indent++;
	  begin_flag = 1;

	  break;

	case VECTOR:

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, "%s\n", temp->vector[0]);

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, 
	     "%s\t%s\t%s\n", 
	     temp->vector[1], temp->vector[2], temp->vector[3]);

	  if (begin_flag == 0)

	    for (i = 0; i < indent; i++)
	      fprintf(yyout, "\t");

	  fprintf(yyout, 
	     "%s\t%s\t%s\n", 
	     temp->vector[4], temp->vector[5], temp->vector[6]);

	  break;

	case COMMENT_:

	  fprintf(yyout, "%s\n", temp->keyword);

	} /* end 'switch (temp->data_type)' */

      if (temp->block)
	{
	  btemp2 = (BLOCKPTR) util_do_malloc(sizeof(struct block_ptr));
	  btemp2->block = temp;
	  temp = temp->block;
	  btemp2->next = NULL;
	  btemp2->prev = btemp;

	  if (btemp)
	    btemp->next = btemp2;

	  btemp = btemp2;
	}

      else if (temp->next)
	temp = temp->next;

      else
	{
	  if (btemp)
	    {
	      while (!btemp->block->next)
		{
		  if (btemp->prev)
		    {
		      btemp2 = btemp;
		      btemp = btemp->prev;
		      free(btemp2);
		      btemp->next = NULL;
		      indent--;

		      if (begin_flag == 0)

			for (i = 0; i < indent; i++)
			  fprintf(yyout, "\t");

/*		      fprintf(yyout, "end %s = %s\n", block_keyword[indent+1],
                                                      block_value[indent+1]); */
		      fprintf(yyout, "end %s\n", block_keyword[indent+1]);

		    }
		  else
		    {
		      free(btemp);
		      indent--;

		      if (begin_flag == 0)

			for (i = 0; i < indent; i++)
			  fprintf(yyout, "\t");

/*		      fprintf(yyout, "end %s = %s\n", block_keyword[indent+1],
                                                      block_value[indent+1]); */
		      fprintf(yyout, "end %s\n", block_keyword[indent+1]);

		      return 0;

		    }
		}

	      temp = btemp->block->next;
	      indent--;

	      if (begin_flag == 0)

		for (i = 0; i < indent; i++)
		  fprintf(yyout, "\t");

/*              fprintf(yyout, "end %s = %s\n", block_keyword[indent+1],
                                              block_value[indent+1]); */
              fprintf(yyout, "end %s\n", block_keyword[indent+1]);

	      btemp2 = btemp;
	      btemp = btemp->prev;
	      free(btemp2);

	      if (btemp)
		btemp->next = NULL;
	    } /* end 'if (btemp)' */

	  else
	    break;

	} /* end 'else' */
    } /* end 'while (temp)' */

  fclose(yyout);
  
} /* end 'print_tree_2' */
 



/*==============================================================================
Function:       find_ag
Description:    finds an aggregation and returns a pointer to it
Parameters:     char *keyword - character string containing the keyword to 
                                search for
		NODEPTR tree_root - a pointer to the root of the tree structure
Returns:        NODEPTR - pointer to the node found
                NULL    - if the node was not found
Creator:        Philip Yurchuk
Creation Date:  02/16/1995
Notes:
==============================================================================*/
NODEPTR find_ag(char *keyword, NODEPTR tree_root)
{
  NODEPTR temp;
  BLOCKPTR btemp = NULL;
  BLOCKPTR btemp2;

  if (tree_root == NULL)
    {
      syslog(LOG_DEBUG, 
         "find_ag: The tree is NULL, so no aggregation could be found in it\n");
 
     return (NULL);

    }
  
  if (keyword == NULL)
    {
      syslog(LOG_DEBUG, 
	 "find_ag: Cannot pass a NULL keyword to function find_ag\n");

      return (NULL);

    }

  temp = tree_root;

  while (temp)
    {
      if (temp->block)
	{
	  if (temp->block->keyword)
	    if (strcmp(keyword, temp->block->keyword) == 0)
	      return(temp);

	  btemp2 = (BLOCKPTR) util_do_malloc(sizeof(struct block_ptr));
	  btemp2->block = temp;
	  temp = temp->block;
	  btemp2->next = NULL;
	  btemp2->prev = btemp;
	  if (btemp)
	    btemp->next = btemp2;
	  btemp = btemp2;
	}

      else if (temp->next)
	temp = temp->next;

      else
	{
	  if (btemp)
	    {
	      while (!btemp->block->next)
		{
		  if (btemp->prev)
		    {
		      btemp2 = btemp;
		      btemp = btemp->prev;
		      free(btemp2);
		      btemp->next = NULL;
		    }
		  else
		    return(NULL);
		}
	      temp = btemp->block->next;
	      btemp2 = btemp;
	      btemp = btemp->prev;
	      free(btemp2);
	      if (btemp)
		btemp->next = NULL;
	    }
	  else
	    break;
	}
    } /* end 'while (temp)' */

  return(NULL);
}




/*==============================================================================
Function:       next_ag
Description:    given a pointer to the current aggregation, returns a pointer to
                the next
Parameters:     NODEPTR ag - pointer to the current aggregation
                NODEPTR tree_root - a pointer to the root of the tree structure
Returns:        NODEPTR - pointer to the next aggregation
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
NODEPTR next_ag (NODEPTR ag, NODEPTR tree_root)
{
  NODEPTR temp;
  struct block_ptr *btemp = NULL;
  struct block_ptr *btemp2;
  int next_flag = 0;

  if (tree_root == NULL)
    {
      syslog(LOG_DEBUG, 
         "next_ag: The tree is NULL, so no aggregation could be found in it\n");
      return (NULL);
    }
  
  if (ag == NULL)
    {
      syslog(LOG_DEBUG, 
	 "next_ag: Cannot pass a NULL aggregation (ag) to function next_ag\n");
      return (NULL);
    }

  temp = tree_root;

  while (temp)
    {

      if (temp->block)
	{

	  if (next_flag == 1)
	    {

	      while (btemp)
		{
		  btemp2 = btemp->prev;
		  free(btemp);
		  btemp = btemp2;
		}

	      return(temp);
	    }

	  /* if you've found the current aggregation, set the next_flag */

	  if (temp->block == ag->block)  
	    next_flag = 1;

	  btemp2 = (BLOCKPTR) util_do_malloc(sizeof(struct block_ptr));
	  btemp2->block = temp;
	  temp = temp->block;
	  btemp2->next = NULL;
	  btemp2->prev = btemp;

	  if (btemp)
	    btemp->next = btemp2;

	  btemp = btemp2;
	}

      else if (temp->next)
	temp = temp->next;

      else
	{

	  if (btemp)
	    {

	      while (!btemp->block->next)
		{

		  if (btemp->prev)
		    {
		      btemp2 = btemp;
		      btemp = btemp->prev;
		      free(btemp2);
		      btemp->next = NULL;
		    }

		  else
		    return(NULL);
		}

	      temp = btemp->block->next;
	      btemp2 = btemp;
	      btemp = btemp->prev;
	      free(btemp2);
	      if (btemp)
		btemp->next = NULL;
	    }

	  else
	    break;
	}
    }

  while (btemp)
    {
      btemp2 = btemp->prev;
      free(btemp);
      btemp = btemp2;
    }

  return(NULL);
}




/*==============================================================================
Function:       find_keyword
Description:    given a character string containing a keyword, will find the 
                node containing it and return a pointer to it
Parameters:     char *keyword - char string containing the keyword to search for
                NODEPTR tree_root - a pointer to the root of the tree structure
Returns:        NODEPTR - pointer to the node containing keyword
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
NODEPTR find_keyword(char *keyword, NODEPTR tree_root)
{
  NODEPTR temp;
  BLOCKPTR btemp = NULL;
  BLOCKPTR btemp2;

  if (tree_root == NULL)
    {
      syslog(LOG_DEBUG, 
	 "find_keyword: The tree is NULL, so no keyword could be found in it.\n");
      return (NULL);
    }
  
  if (keyword == NULL)
    {
      syslog(LOG_DEBUG,
	 "find_keyword: Cannot pass a NULL keyword to function find_keyword.\n");
      return (NULL);
    }

  temp = tree_root;
  while (temp)
    {
      if (temp->keyword)
	if (strcmp(keyword, temp->keyword) == 0)
	  return(temp);

      if (temp->block)
	{
	  btemp2 = (BLOCKPTR) util_do_malloc(sizeof(struct block_ptr));
	  btemp2->block = temp;
	  temp = temp->block;
	  btemp2->next = NULL;
	  btemp2->prev = btemp;
	  if (btemp)
	    btemp->next = btemp2;
	  btemp = btemp2;
	}

      else if (temp->next)
	temp = temp->next;

      else
	{
	  if (btemp)
	    {
	      while (!btemp->block->next)
		{
		  if (btemp->prev)
		    {
		      btemp2 = btemp;
		      btemp = btemp->prev;
		      free(btemp2);
		      btemp->next = NULL;
		    }
		  else
		    return(NULL);
		}
	      temp = btemp->block->next;
	      btemp2 = btemp;
	      btemp = btemp->prev;
	      free(btemp2);
	      if (btemp)
		btemp->next = NULL;
	    }
	  else
	    break;
	}
    }
  return(NULL);
}




/*==============================================================================
Function:       get_keyword_value
Description:    given a node, returns the value field
Parameters:     NODEPTR Node - a pointer to the node containing the desired 
		               value
Returns:        char * - character string containing the node's value
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
char *get_keyword_value(NODEPTR Node)
{
  return(Node->value);
}




/*==============================================================================
Function:       change_keyword_value
Description:    changes the value field to the given character string
Parameters:     NODEPTR Node - a pointer to the node whose value will be changed
                char * - character string containing the node's new value
Returns:        
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
void change_keyword_value(NODEPTR Node, char *value)
{

  if (Node->value)
    free(value);
  
  Node->value = value;
}




/*==============================================================================
Function:       find_keyword_value
Description:    given a keyword, returns it's value
Parameters:     char *keyword - character string containing the keyword
                NODEPTR tree_root - a pointer to the root of the tree structure
Returns:        char * - character string containing the keyword's value
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
char *find_keyword_value(char *keyword, NODEPTR tree_root)
{
  NODEPTR temp;
  BLOCKPTR btemp = NULL;
  BLOCKPTR btemp2;

  if (!tree_root)
    {
      syslog(LOG_DEBUG, 
      "find_keyword_value: The tree is NULL, so no keyword could be found in it\n");
      return (NULL);
    }
  
  if (!keyword)
    {
      syslog(LOG_DEBUG, 
      "find_keyword_value: Cannot pass a NULL keyword to function find_keyword_value\n");
      return (NULL);
    }

  temp = tree_root;
  while (temp)
    {
      if (temp->keyword)
	if (strcmp(keyword, temp->keyword) == 0)
	  return(temp->value);

      if (temp->block)
	{
	  btemp2 = (BLOCKPTR) util_do_malloc(sizeof(struct block_ptr));
	  btemp2->block = temp;
	  temp = temp->block;
	  btemp2->next = NULL;
	  btemp2->prev = btemp;
	  if (btemp)
	    btemp->next = btemp2;
	  btemp = btemp2;
	}

      else if (temp->next)
	temp = temp->next;

      else
	{
	  if (btemp)
	    {
	      while (!btemp->block->next)
		{
		  if (btemp->prev)
		    {
		      btemp2 = btemp;
		      btemp = btemp->prev;
		      free(btemp2);
		      btemp->next = NULL;
		    }
		  else
		    return(NULL);
		}
	      temp = btemp->block->next;
	      btemp2 = btemp;
	      btemp = btemp->prev;
	      free(btemp2);
	      if (btemp)
		btemp->next = NULL;
	    }
	  else
	    break;
	}
    }
  return(NULL);
}




/*==============================================================================
Function:       create_node
Description:    given all the parameters for a node, creates a new node and 
                returns a pointer to it
Parameters:     char *keyword   - character string containing the keyword
                char *value     - character string containing the value of 
		                  keyword
                NODEPTR block   - pointer to an aggregation
                ARRAYPTR Array_ - pointer to a linked list containing the array
		                  structure
		char *vector[7] - pointer to an array of character strings 
		                  containing the state vector information
		int data_type   - the type of data the node contains
Returns:        NODEPTR - a pointer to the newly created node
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
NODEPTR create_node(char *keyword, char *value, NODEPTR block, ARRAYPTR Array, char *vector[7], int data_type)
{
  NODEPTR temp;

  temp = (NODEPTR) mknode();
  
  if (keyword)
    {
      temp->keyword = 
	 (char *) util_do_malloc((sizeof(char))*(strlen(keyword)+1));
      strcpy(temp->keyword, keyword);
    }
  else
  temp->keyword = keyword;

  if (value)
    {
      temp->value = (char *)util_do_malloc((sizeof(char))*(strlen(value)+1));
      strcpy(temp->value, value);
    }
  else
    temp->value = value;

  temp->block = block;

  if (block)
    block->prev = temp;

  temp->Array = Array;

  if (vector)
    {
      temp->vector[0] = vector[0];
      temp->vector[1] = vector[1];
      temp->vector[2] = vector[2];
      temp->vector[3] = vector[3];
      temp->vector[4] = vector[4];
      temp->vector[5] = vector[5];
      temp->vector[6] = vector[6];
    }

  temp->data_type = data_type;
  
  temp->next = temp->prev = NULL;

  return(temp);
}




/*==============================================================================
Function:       insert_node
Description:    inserts a node into the tree
Parameters:     NODEPTR Node      - the node to be inserted
                NODEPTR current   - a pointer to the node which will come 
		                    before or after Node
                NODEPTR tree_root - a pointer to the root of the tree structure
		int placement     - defines whether Node will come before or 
		                    after 'current'
Returns:
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
int insert_node(NODEPTR Node, NODEPTR current, NODEPTR tree_root, int placement)
{
  if (!Node || !current || !tree_root)
    {
      syslog(LOG_DEBUG, 
	 "insert_node: Cannot pass a NULL pointer to insert_node.\n");
      return -1;
    }

  if (placement == BEFORE)
    {
      if (current->prev)
	current->prev->next = Node;
      
      Node->prev = current->prev;
      Node->next = current;
      current->prev = Node;
      
      if (tree_root == current)  /* if we're inserting something before the */
	tree_root = Node;        /* root of the tree, make it the new root  */
    }
  
  else if (placement == AFTER)
    {
      if (current->next)
	current->next->prev = Node;
      
      Node->next = current->next;
      Node->prev = current;
      current->next = Node;
    }

  else
    {
      syslog(LOG_DEBUG, "insert_node: Invalid placement passed to insert_node.\n");
      return -1;
    }
  return 0;
}




/*==============================================================================
Function:       delete_node
Description:    deletes a node from the tree structure
Parameters:     NODEPTR Node      - a pointer to the node to be deleted
                NODEPTR tree_root - a pointer to the tree data structure
Returns:
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
int delete_node(NODEPTR Node, NODEPTR tree_root)
{
  int i;
  ARRAYPTR atemp;
  ARRAYPTR atemp2;

  if (!Node || !tree_root)
    {
      syslog(LOG_DEBUG, 
	 "delete_node: Cannot pass a NULL pointer to delete_node.\n");
      return -1;
    }

  if (Node->keyword)
    free(Node->keyword);
  if (Node->value)
    free(Node->value);
  if (Node->vector[0])
    for (i = 0; i < 7; i++)
      free(Node->vector[i]);

  if (Node->Array)
    {
      atemp = Node->Array;
      while (atemp)
	{
	  atemp2 = atemp->next;
	  free(atemp);
	  atemp = atemp2;
	}
    }

  if (tree_root == Node)
    tree_root = Node->next;

  else if (Node->prev)
    {

      if ((Node->prev->block == Node) && Node->next)
	Node->prev->block = Node->next;

      else if ((Node->prev->block == Node) && !Node->next)
	delete_node(Node->prev, tree_root);

      else 
	Node->prev->next = Node->next;
    }

  else if (Node->next)
    Node->next->prev = Node->prev;

  free(Node);

  return 0;
}




/*==============================================================================
Function:       insert_ag
Description:    inserts an aggregation into the tree structure
Parameters:     NODEPTR Node      - a pointer to the node to be inserted
                NODEPTR current   - a pointer to the node that will come before
		                    the aggregation
                NODEPTR tree_root - a pointer to the root of the tree
Returns:
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
int insert_ag (NODEPTR Node, NODEPTR current, NODEPTR tree_root)
{
  NODEPTR temp;

  if (!Node || !current || !tree_root)
    {
      syslog(LOG_DEBUG,
	 "insert_ag: Cannot pass a NULL pointer to insert_ag.\n");
      return -1;
    }

  temp = create_node(NULL, NULL, Node, NULL, NULL, BLOCK);

  insert_node(temp, current, tree_root, AFTER);
  
  return 0;
}




/*==============================================================================
Function:       append_statement
Description:    appends a statement to the end of the tree structure
Parameters:     NODEPTR Node      - a pointer to the node to be inserted
                NODEPTR tree_root - a pointer to the root of the tree structure
Returns:
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
int append_statement(NODEPTR Node, NODEPTR tree_root)
{
  NODEPTR temp;

  if (!Node || !tree_root)
    {
      syslog(LOG_DEBUG, 
	 "append_statement: Cannot pass a NULL pointer to append_statement.\n");
      return -1;
    }

  temp = tree_root;
  
  while (temp->next)
    temp = temp->next;
  
  if (temp == Node)
    {
      syslog(LOG_DEBUG, "append_statement: Cannot append the same node twice");
      return -1;
    }

  temp->next  = Node;
  Node->prev = temp;

  return 0;
}




/*==============================================================================
Function:       append_substatement
Description:    appends a statement to the end of an aggregation
Parameters:     NODEPTR Node        - a pointer to the node to be inserted
                NODEPTR aggregation - a pointer to the aggregation
Returns:
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
int append_substatement(NODEPTR Node, NODEPTR aggregation)
{
  NODEPTR temp;

  if (!Node || !aggregation)
    {
      syslog(LOG_DEBUG, 
	 "append_substatement: Cannot pass a NULL pointer to append_substatement.\n");
      return -1;
    }

  temp = aggregation->block;
  
  if (!temp)
    {
      Node->prev = aggregation;
      aggregation->block = Node;
      return 0;
    }

  else
    {
      while (temp->next)
	temp = temp->next;

      if (temp == Node)
	{
	  syslog(LOG_DEBUG,
	     "append_substatement: Cannot append the same node twice");
	  return -1;
	}
      
      temp->next  = Node;
      Node->prev = temp;
    }
  
  return 0;
}




/*==============================================================================
Function:       create_header
Description:    creates a tree of header assignments given their values
Parameters:     char *filename
                char *spacecraft_identifier
		char *file_creation_time
		char *flie_source
		char *file_dest
		char *file_type
Returns:        NODEPTR - a pointer to the first node (filename)
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
NODEPTR create_header(char *filename, char *spacecraft_identifier, char *file_creation_time, char *file_source, char *file_dest, char *file_type)

{
  NODEPTR top;
  NODEPTR temp;
  char *string;

  top = create_node(";*******************************************************", 
     NULL, NULL, NULL, NULL, COMMENT_);

  temp = create_node(";FILE HEADER", NULL, NULL, NULL, NULL, COMMENT_);

  append_statement(temp, top);

  temp = create_node(";*******************************************************", NULL, NULL, NULL, NULL, COMMENT_);
  
  append_statement(temp, top);

  temp = create_node("FILENAME", filename, NULL, NULL, NULL, HDR_ASSIGN);
  append_statement(temp, top);

  temp = create_node("SPACECRAFT_IDENTIFIER", spacecraft_identifier, NULL, NULL, NULL, HDR_ASSIGN);

  append_statement(temp, top);

  temp = create_node("FILE_CREATION_TIME", file_creation_time, NULL, NULL, NULL, HDR_ASSIGN);
  
  append_statement(temp, top);

  temp = create_node("FILE_SOURCE", file_source, NULL, NULL, NULL, HDR_ASSIGN);
  append_statement(temp, top);

  temp = create_node("FILE_DEST", file_dest, NULL, NULL, NULL, HDR_ASSIGN);
  append_statement(temp, top);

  temp = create_node("FILE_TYPE", file_type, NULL, NULL, NULL, HDR_ASSIGN);
  append_statement(temp, top);

  return(top);
}




/*==============================================================================
Function:       create_keyword_value
Description:    returns a node with keyword and value inserted
Parameters:     char *keyword
                char *value
Returns:        NODEPTR - a pointer to the newly created node
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
NODEPTR create_keyword_value(char *keyword, char *value)
{
  return (create_node(keyword, value, NULL, NULL, NULL, ASSIGN));
}




/*==============================================================================
Function:       create_comment
Description:    returns a node with a comment inserted
Parameters:     char *keyword - the comment string
Returns:        NODEPTR - a pointer to the newly created node
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
NODEPTR create_comment(char *keyword)
{
  return (create_node(keyword, NULL, NULL, NULL, NULL, COMMENT_));
}




/*==============================================================================
Function:       create_ag
Description:    creates an aggregation node
Parameters:     NODEPTR top - the node that will become the first node in the 
                              aggregation
Returns:        NODEPTR - a pointer to the newly created node
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
NODEPTR create_ag(NODEPTR top)
{

  if (!top)
    {
      syslog(LOG_DEBUG, "create_ag: Cannot pass a NULL pointer to create_ag.\n");
      return(NULL);
    }

  return (create_node(NULL, NULL, top, NULL, NULL, BLOCK));
}




/*==============================================================================
Function:       create_array
Description:    creates an array node and returns it
Parameters:     char *keyword - the name of the array
Returns:        char *value   - the number of columns in the array
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
NODEPTR create_array(char *keyword, char *value)
{
  if (!keyword || !value)
    {
      syslog(LOG_DEBUG,
	 "create_array: Cannot pass a NULL pointer to create_array.\n");
      return(NULL);
    }
  
  return(create_node(keyword, value, NULL, NULL, NULL, ARRAY_));
}




/*=============================================================================
Function:       append_element
Description:    appends an element to the end of an array
Parameters:     char *element     - the element to be added
                NODEPTR arraynode - the array that receives the element
Returns:
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
int append_element(char *element, NODEPTR array_stmt)
{
  ARRAYPTR temp;
  ARRAYPTR temp2;
  
  if (!element || !array_stmt)
    {
      syslog(LOG_DEBUG,
	 "append_element: Cannot pass a NULL pointer to append_element.\n");
      return -1;
    }

  temp = (ARRAYPTR) util_do_malloc(sizeof(struct array_node));
  temp->value = element;
  temp->next = NULL;

  if (!array_stmt->Array)
    {
      array_stmt->Array = temp;
      return 0;
    }
  else
    {
      temp2 = array_stmt->Array;
      while (temp2->next)
	temp2 = temp2->next;
      temp2->next = temp;
    }

  return 0;
}




/*==============================================================================
Function:       delete_tree
Description:    deletes a tree structure and frees up the memory
Parameters:     NODEPTR tree_root - a pointer to the root of the tree structure
Returns:
Creator:        Philip Yurchuk
Creation Date:  03/03/1995
Notes:
==============================================================================*/
void delete_tree(NODEPTR tree_root)
{
 NODEPTR curr;
 NODEPTR temp;
 
 curr = tree_root;

 while (curr->next)
   curr = curr->next;

 while (curr)
   {

/*  if the current node is not a block statement, delete the node and move to 
 *  the previous node
 */

     if (!curr->block)
       {
	 if (curr->prev)
	   {
	     if (curr->prev->block == curr)
	       temp = curr->prev->prev;
	     else
	       temp = curr->prev;
	   }
	 else
	   temp = curr->prev;

	 delete_node(curr, tree_root);
	 curr = temp;
       }

/*  if the current node is a block statement, go to the end of the 
 *  block (aggregation)
 */ 
     else
       {
	 curr = curr->block;
	 while (curr->next)
	   curr = curr->next;
       }
   }
}


/* End of File */
