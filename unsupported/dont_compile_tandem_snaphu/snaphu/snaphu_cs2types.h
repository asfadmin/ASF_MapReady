/* defs.h */


typedef long excess_t;

typedef  /* arc */
   struct arc_st
{
   short            r_cap;           /* residual capasity */
   short            cost;            /* cost  of the arc*/
   struct node_st   *head;           /* head node */
   struct arc_st    *sister;         /* opposite arc */
}
  arc;

typedef  /* node */
   struct node_st
{
   arc              *first;           /* first outgoing arc */
   arc              *current;         /* current outgoing arc */
   arc              *suspended;
   double           price;            /* distance from a sink */
   struct node_st   *q_next;          /* next node in push queue */
   struct node_st   *b_next;          /* next node in bucket-list */
   struct node_st   *b_prev;          /* previous node in bucket-list */
   long             rank;             /* bucket number */
   excess_t         excess;           /* excess of the node */
   signed char      inp;              /* temporary number of input arcs */
} node;

typedef /* bucket */
   struct bucket_st
{
   node             *p_first;         /* 1st node with positive excess 
				         or simply 1st node in the buket */
} bucket;
