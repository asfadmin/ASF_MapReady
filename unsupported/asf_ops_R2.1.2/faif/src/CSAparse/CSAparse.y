/*==============================================================================
Filename: CSAparse.y
 
Description: yacc code to recognize CSA files
 
External Functions:
 
Static Functions:
 
External Variables Defined:
 
File Scope Static Variables:
 
Notes:
1.  May '96 - R. Hoffman 
    Added "|| (root == NULL)" to if condition in tree() [for PR 859]
    (and corrected "Filename: " value in comment above)
==============================================================================*/


%token  Begin END ARRAY VALUE KEYWORD DATE COMMENT
%{
#include "CSAparse.h"
char *val;                       /* string a value is stored in           */
char *big;                       /* string a text string is stored in     */
int array_flag = 0;              /* flag that an array is being processed */
int block_flag = 0;              /* flag that a block is being processed  */
struct node *root = NULL;        /* pointer to the root of the tree       */
struct node *curr = NULL;        /* pointer to the current tree node      */

struct block_ptr *curr_block = NULL;  /* pointer to current block node     */
struct block_ptr *head_block = NULL;  /* pointer to list of nested blocks  */
struct block_ptr *last_block = NULL;  /* pointer to last block in list     */
struct array_node *array_list = NULL; /* pointer to a list of values       */
struct array_node *curr_array = NULL; /* pointer to the current array node */
int count = 0;
extern yyout;
%}

%%

start : multassign start | block start | array start | vector start | comment start | ;

Value : VALUE 
{
  /* dynamically allocate an array to store the value string */

  val = big = (char *) calloc((strlen(yylval) + 1), sizeof(char));
  strcpy(val, yylval);
  $$ = (int) val;
}
;

Date : DATE 
{
  /* dynamically allocate an array to store the date string */

  val = (char *) calloc((strlen(yylval) + 1), sizeof(char));
  strcpy(val, yylval);
  $$ = (int) val;
}
;

Keyword : KEYWORD 
{
  /* dynamically allocate an array to store the keyword string */

  val = (char *) calloc((strlen(yylval) + 1), sizeof(char));
  strcpy(val, yylval);
  $$ = (int) val;
}
;

comment : COMMENT 
{
  /* dynamically allocate an array to store the comment string */

  val = (char *) calloc((strlen(yylval) + 1), sizeof(char));
  strcpy(val, yylval);
  tree(COMMENT_, val, NULL);
}
;

value : Value 
{
  struct array_node *temp;

  /* if we're processing an array, put all the values into a linked list */

  if (array_flag == 1)  
    {
      temp = (ARRAYPTR) util_do_malloc(sizeof (struct array_node));
      temp->value = (char *) $1;
      temp->next = NULL;

      if (array_list == NULL)           /* if this is a new array */
	array_list = curr_array = temp;

      else                            /* if we're adding to the current array */
	{
	  curr_array->next = temp;
	  curr_array = temp;
	}
    }
}
| Date 
{
  struct array_node *temp;

  /* if we're processing an array, put all the dates into a linked list    */
  /* NOTE: dates and values can go into the same list (a date is a value)  */

  if (array_flag == 1)
    {
      temp = (ARRAYPTR) util_do_malloc(sizeof (struct array_node));
      temp->value = (char *) $1;
      temp->next = NULL;

      if (array_list == NULL)            /* if this is a new array */
	array_list = curr_array = temp;

      else                           /* if we're adding to the current array */
	{
	  curr_array->next = temp;
	  curr_array = temp;
	}
    } 
}
;

multvalue : value multvalue 
{
  char *temp;
  char *temp2;
  temp2 = (char *) $2;
  count++;
  
  /*  if we're not processing an array, concatenate all the values into one
   *  string 
   */

  if (array_flag == 0)
    {
      temp = (char *) calloc((strlen($2)+1), sizeof(char));

      /* You must prepend the new value to the existing string */

      strcpy(temp, big);
      free(big);
      big = (char *)util_do_malloc((strlen($1) + strlen(temp) + 1)*sizeof(char));
      strcpy(big, $1);
      strcat(big, " ");
      strcat(big, temp);
      free(temp);
      free((char *) $1);
      $$ = (int) big;
    }
  
}
| value 
{
}
;

assign : Keyword '=' multvalue 
{

  if (array_flag == 1)     /* insert an array into the tree structure */
    tree(ARRAY_, (char *) $1, (char *) $3);
  else                     /* insert an assignment into the tree structure */
    tree(ASSIGN, (char *) $1, (char *)$3);
}
| Keyword ':' multvalue 
{
  /* insert a header assignment into the tree structure */

  tree(HDR_ASSIGN, (char *) $1, (char *) $3);

}
| Keyword '=' 
{
  /*  insert an empty assignment into the tree structure
   *  (empty meaning keyword with missing value)
   */

  if (array_flag == 1) 
    tree(ARRAY_, (char *) $1, NULL);
  else
    tree(ASSIGN, (char *) $1, NULL);
}
;

multassign : assign multassign | assign ;

$ACT1 : 
{
  array_flag = 1;  /* set the flag to signal we're processing an array */
}
;

$ACT0 : 
{
  
  array_flag = 0;  /* reset the flag to zero */
}
;

array : ARRAY $ACT1 assign END $ACT0 | ARRAY $ACT1 assign END value $ACT0 ;

datastruct : multassign datastruct | array datastruct | block datastruct | ;

$ACT3 : 
{

  /* This is called when a "begin" is found */

  struct node *temp;         /* temporary tree node pointer */
  struct block_ptr *btemp;   /* temporary block node pointer */

  temp = mknode();
  temp->data_type = 3; 
  btemp = (BLOCKPTR) util_do_malloc(sizeof(struct block_ptr));
  curr->next = temp;
  temp->prev = curr;
  btemp->block = curr = temp;
  
  if (curr_block == NULL)
    {
      btemp->next = btemp->prev = NULL;
      last_block = curr_block = btemp;
      if (!head_block)
	head_block = btemp;
    }
  else
    {
      btemp->next = NULL;
      btemp->prev = curr_block;
      curr_block = last_block = btemp;
    }
  
  block_flag = 1;
}
;

$ACT4 : 
{
   curr = curr_block->block; 
   curr_block = curr_block->prev; 
}
;

block : Begin $ACT3 datastruct END $ACT4 value 
{
  char *temp;
  temp = (char *) $6;

  if ($6)
    free((char *) $6);
} 
| Begin $ACT3 datastruct END $ACT4 
;

vector : Date Value Value Value Value Value Value
{
  struct node *temp;
  temp = mknode();
  
  temp->vector[0] = (char *)$1;
  temp->vector[1] = (char *)$2;
  temp->vector[2] = (char *)$3;
  temp->vector[3] = (char *)$4;
  temp->vector[4] = (char *)$5;
  temp->vector[5] = (char *)$6;
  temp->vector[6] = (char *)$7;
  
  temp->data_type = 5;
  curr->next = temp;
  temp->prev = curr;
  curr = temp;
}
;
 
%%

struct node *mknode()
{
  struct node *temp;
  
  temp = (NODEPTR) util_do_malloc(sizeof(struct node));
  temp->keyword    = NULL;
  temp->value      = NULL;
  temp->block      = NULL;
  temp->Array      = NULL;
  temp->vector[0]  = NULL;
  temp->vector[1]  = NULL;
  temp->vector[2]  = NULL;
  temp->vector[3]  = NULL;
  temp->vector[4]  = NULL;
  temp->vector[5]  = NULL;
  temp->vector[6]  = NULL;
  temp->next       = NULL;
  temp->prev       = NULL;

  return(temp);
}

void tree (int data_type, char *keyword, char *value) 
{

  struct node *temp;
  temp = mknode();

  switch (data_type) {

  case HDR_ASSIGN:

    temp->data_type = HDR_ASSIGN;
    temp->keyword   = keyword;
    temp->value     = value;

    /*  Added "|| (root == NULL)" for PR 859 */
    if ((curr == NULL) || (root == NULL))
      root = curr = temp;
    else
      {
	curr->next = temp;
	temp->prev = curr;
	curr = temp;
      }
    break;

  case ASSIGN:
	
    temp->data_type = ASSIGN;
    temp->keyword   = keyword;
    temp->value     = value;

    if (block_flag == 1)
      {
	curr->block = temp;
	temp->prev = curr;
	block_flag = 0;
      }
    else
      {
	curr->next = temp;
	temp->prev = curr;
      }
    curr = temp;

    break;

  case ARRAY_:

    temp->data_type = ARRAY_;
    temp->keyword   = keyword;
    temp->value     = value;
    temp->Array     = array_list;
    
    curr_array = array_list = NULL;
    
    if (block_flag == 1)
      {
	curr->block = temp;
	block_flag = 0;
      }
    else
      {
	curr->next = temp;
	temp->prev = curr;
      }
    curr = temp;
  
    break;

  case COMMENT_:
    temp->data_type = COMMENT_;
    temp->keyword   = keyword;

    /*  Added "|| (root == NULL)" for PR 859 */
    if ((curr == NULL) || (root == NULL))
      root = curr = temp;
    else if (block_flag == 1)
      {
	curr->block = temp;
	temp->prev = curr;
	block_flag = 0;
      }
    else
      {
	curr->next = temp;
	temp->prev = curr;
	curr = temp;
      }
  }    
} 

