/*==============================================================================
Filename:       CSAparse.h
Description:    Contains definitions for the tree structure for CSA files
Creator:        Phil Yurchuk (phil@ditto.jpl.nasa.gov)
Notes:

SCCS Info:
   %W%
==============================================================================*/

#ifndef _CSAPARSE_
#define _CSAPARSE_

#include <stdio.h>
#include <syslog.h>

#define HDR_ASSIGN 1
#define ASSIGN     2
#define BLOCK      3
#define ARRAY_     4
#define VECTOR     5
#define COMMENT_   6

struct array_node
{
  char *value;
  struct array_node *next;
};

struct node 
{
  char *keyword;
  char *value;
  struct node *block;
  struct array_node *Array;
  char *vector[7];
  int data_type;
  struct node *prev;
  struct node *next;
};

struct block_ptr
{
  struct node *block;
  struct block_ptr *next;
  struct block_ptr *prev;
};

typedef struct node *NODEPTR;
typedef struct node *CSA_STMT;
typedef struct block_ptr *BLOCKPTR;
typedef struct array_node *ARRAYPTR;

#define BEFORE -1
#define AFTER   1

#endif /* _CSAPARSE_ */

/* End of File */
