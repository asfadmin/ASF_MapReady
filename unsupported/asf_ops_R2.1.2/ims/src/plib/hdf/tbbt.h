/***************************************************************************
*
*
*                         NCSA HDF version 3.3r1
*                            September 20, 1993
*
* NCSA HDF Version 3.3 source code and documentation are in the public
* domain.  Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
*
* We ask, but do not require, that the following message be included in all
* derived works:
*
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign, in collaboration with the
* Information Technology Institute of Singapore.
*
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
*
*****************************************************************************/

/* "tbbt.h" -- Data types/routines for threaded, balanced, binary trees. */
/* Extended from Knuth 6.2.3, Algorithm A */

#ifndef TBBT_H
#define TBBT_H

#ifdef lint         /* lint always complains but may complain more if... */
# define   TBBT_INTERNALS   /* TBBT_INTERNALS not always defined */
#endif /* lint */

typedef   struct tbbt_node  TBBT_NODE;

struct tbbt_node {
  VOIDP     data;      /* Pointer to user data to be associated with node */
  VOIDP     key;       /* Field to sort nodes on */

#ifdef TBBT_INTERNALS
# define   PARENT  0
# define   LEFT    1
# define   RIGHT   2
  TBBT_NODE *link[3];   /* Pointers to parent, left child, and right child */
# define  Parent    link[PARENT]
# define  Lchild    link[LEFT]
# define  Rchild    link[RIGHT]
# define  TBBT_FLAG unsigned long
  TBBT_FLAG  flags;     /* Combination of the following bit fields: */
# define  TBBT_HEAVY(s) s   /* If the `s' sub-tree is deeper than the other */
# define  TBBT_DOUBLE   4   /* If "heavy" sub-tree is two levels deeper */
# define  TBBT_INTERN   8   /* If node is internal (has two children) */
# define  TBBT_BITS     4   /* Number of bits used above */
# define  TBBT_UNBAL    ( TBBT_HEAVY(LEFT) | TBBT_HEAVY(RIGHT) )
# define  TBBT_FLAGS    ( TBBT_UNBAL | TBBT_INTERN | TBBT_DOUBLE )
# define  TBBT_CHILD(s) ( TBBT_INTERN | TBBT_HEAVY(s) )
# define  LeftCnt(node) ( (node)->flags >> TBBT_BITS )  /* Left descendants */
# define  Cnt(node,s)   ( LEFT==(s) ? LeftCnt(node) : 0L )
# define  HasChild(n,s) ( TBBT_CHILD(s) & (n)->flags )
# define  Heavy(n,s)    ( TBBT_HEAVY(s) & (n)->flags )
# define  Intern(n)     ( TBBT_INTERN & (n)->flags )
# define  UnBal(n)      ( TBBT_UNBAL & (n)->flags )
# define  Double(n)     ( TBBT_DOUBLE & (n)->flags )
# define  Other(side)   ( LEFT + RIGHT - (side) )
# define  Delta(n,s)    (  ( Heavy(n,s) ? 1 : -1 )                          \
                            *  ( Double(n) ? 2 : UnBal(n) ? 1 : 0 )  )
#ifdef OLD_WAY
# define  SetFlags(n,s,c,b,i)   (  ( -2<(b) && (b)<-2 ? 0 : TBBT_DOUBLE )   \
    |  ( 0<(b) ? TBBT_HEAVY(side) : (b)<0 ? TBBT_HEAVY(Other(s)) : 0 )      \
    |  ( ( LEFT==(s) ? (c) : LeftCnt(n) ) << TBBT_BITS )                    \
    |  ( (i) ? TBBT_INTERN : 0 )  )
#else
#ifdef QAK
# define  SetFlags(n,s,c,b,i)   (  ( -2<(b) && (b)<2 ? 0 : TBBT_DOUBLE )   \
    |  ( 0<(b) ? TBBT_HEAVY(s) : (b)<0 ? TBBT_HEAVY(Other(s)) : 0 )        \
    |  ( ( LEFT==(s) ? (c) : LeftCnt(n) ) << TBBT_BITS )                   \
    |  ( (i) ? TBBT_INTERN : 0 )  )
#else
#if defined macintosh  | THINK_C /* There is a limit to recursive 
                                    macro substitution */
# define  SetFlags(n,s,c,b,i)   (  ( -2<(b) && (b)<2 ? 0 : TBBT_DOUBLE )   \
    |  ( 0>(b) ? TBBT_HEAVY(s) : (b)>0 ? TBBT_HEAVY( 1 + 2 - (s)) : 0 )    \
    |  ( ( LEFT==(s) ? (c) : LeftCnt(n) ) << TBBT_BITS )                   \
    |  ( (i) ? TBBT_INTERN : 0 )  )
#else /* !macintosh */
# define  SetFlags(n,s,c,b,i)   (  ( -2<(b) && (b)<2 ? 0 : TBBT_DOUBLE )   \
    |  ( 0>(b) ? TBBT_HEAVY(s) : (b)>0 ? TBBT_HEAVY(Other(s)) : 0 )        \
    |  ( ( LEFT==(s) ? (c) : LeftCnt(n) ) << TBBT_BITS )                   \
    |  ( (i) ? TBBT_INTERN : 0 )  )
#endif /* !macintosh */
#endif  /* QAK */
#endif /* OLD_WAY */
/* n=node, s=LEFT/RIGHT, c=`s' descendant count, b=balance(s), i=internal */
/* SetFlags( ptr, LEFT, Cnt(RIGHT,kid), -2, YES ) */
};
typedef   struct tbbt_tree  TBBT_TREE;
struct tbbt_tree {
  TBBT_NODE *root;
  unsigned long count;  /* The number of nodes in the tree currently */
  intn     (*compar) PROTO((VOIDP k1,VOIDP k2,intn cmparg));
  intn     cmparg;
#endif /* TBBT_INTERNALS */
};
#ifndef TBBT_INTERNALS
typedef   TBBT_NODE   **TBBT_TREE;
#endif /* TBBT_INTERNALS */

/* Return maximum of two scalar values (use arguments w/o side effects): */
#define   Max(a,b)  ( (a) > (b) ? (a) : (b) )

/* These routines are designed to allow use of a general-purpose balanced tree
 * implimentation.  These trees are appropriate for maintaining in memory one
 * or more lists of items, each list sorted according to key values (key values
 * must form a "completely ordered set") where no two items in a single list
 * can have the same key value.  The following operations are supported:
 *     Create an empty list
 *     Add an item to a list
 *     Look up an item in a list by key value
 *     Look up the Nth item in a list
 *     Delete an item from a list
 *     Find the first/last/next/previous item in a list
 *     Destroy a list
 * Each of the above operations requires Order(log(N)) time where N is the
 * number of items in the list (except for list creation which requires
 * constant time and list destruction which requires Order(N) time if the user-
 * supplied free-data-item or free-key-value routines require constant time).
 * Each of the above operations (except create and destroy) can be performed
 * on a subtree.
 *
 * Each node of a tree has associated with it a generic pointer (void *) which
 * is set to point to one such "item" and a generic pointer to point to that
 * item's "key value".  The structure of the items and key values is up to the
 * user to define.  The user must specify a method for comparing key values.
 * This routine takes three arguments, two pointers to key values and a third
 * integer argument.  You can specify a routine that expects pointers to "data
 * items" rather than key values in which case the pointer to the key value in
 * each node will be set equal to the pointer to the data item.
 *
 * Since the "data item" pointer is the first field of each tree node, these
 * routines may be used without this "tbbt.h" file.  For example, assume "ITM"
 * is the structre definition for the data items you want to store in lists:
 * ITM ***tbbtdmake( int (*cmp)(void *,void *,int), int arg );
 * ITM **root= NULL;        (* How to create an empty tree w/o tbbtdmake() *)
 * ITM **tbbtdfind( ITM ***tree, void *key, ITM ***pp );
 * ITM **tbbtfind( ITM **root, void *key, int (*cmp)(), int arg, ITM ***pp );
 * ITM **tbbtindx( ITM **root, long indx );
 * ITM **tbbtdins( ITM ***tree, ITM *item, void *key );
 * ITM **tbbtins( ITM ***root, ITM *item, void *key, int (*cmp)(), int arg );
 * ITM *tbbtrem( ITM ***root, ITM **node, void **kp );
 * ITM **tbbtfirst( ITM **root ), **tbbtlast( ITM **root );
 * ITM **tbbtnext( ITM **node ), **tbbtprev( ITM **node );
 * ITM ***tbbtdfree( ITM ***tree, void (*df)(ITM *), void (*kf)(void *) );
 * void tbbtfree( ITM ***root, void (*df)(ITM *), void (*kf)(void *) );
 */

TBBT_TREE *tbbtdmake
    PROTO((intn (*compar)(VOIDP ,VOIDP ,intn), intn arg ));
/* Allocates and initializes an empty threaded, balanced, binary tree and
 * returns a pointer to the control structure for it.  You can also create
 * empty trees without this function as long as you never use tbbtd* routines
 * (tbbtdfind, tbbtdins, tbbtdfree) on them.
 * Examples:
 *     int keycmp();
 *     TBBT_ROOT *root= tbbtmake( keycmp, (int)keysiz );
 * or
 *     void *root= tbbtmake( strcmp, 0 );
 * or
 *     TBBT_NODE *root= NULL;        (* Don't use tbbtd* routines *)
 * `cmp' is the routine to be used to compare two key values [in tbbtdfind()
 * and tbbtdins()].  The arguments to `cmp' are the two keys to compare
 * and `arg':  (*cmp)(k1,k2,arg).  `cmp' is expected to return 0 if its first
 * two arguments point to identical key values, -1 (or any integer less than 0)
 * if k1 points to a key value lower than that pointed to by k2, and 1 (or any
 * integer greater than 0) otherwise.  If `cmp' is NULL, memcmp is used.  If
 * `cmp' is NULL and `arg' is not greater than 0L, `1+strlen(key1)' is used in
 * place of `arg' to emulate strcmp():  memcmp( k1, k2, 1+strlen(k1) ).  You
 * can use strcmp() directly (as in the second example above) as long as your C
 * compiler does not assume strcmp() will always be passed exactly 2 arguments
 * (only newer, ANSI-influenced C compilers are likely to be able to make this
 * kind of assumption).  You can also use a key comparison routine that expects
 * pointers to data items rather than key values.
 *
 * Most of the other routines expect a pointer to a root node of a tree, not
 * a pointer to the tree's control structure (only tbbtdfind(), tbbtdins(),
 * and tbbtdfree() expect pointers to control structures).  However TBBT_TREE
 * is just defined as "**TBBT_NODE" (unless you have defined TBBT_INTERNALS so
 * you have access to the internal structure of the nodes) so
 *     TBBT_TREE *tree1= tbbtdmake( NULL, 0 );
 * is equivalent to
 *     TBBT_NODE **tree1= tbbtdmake( NULL, 0 );
 * So could be used as:
 *     node= tbbtdfind( tree1, key, NULL );
 *     node= tbbtfind( *tree1, key, compar, arg, NULL );
 *     node= tbbtdins( tree1, item, key );
 *     node= tbbtins( tree1, item, key, compar, arg );
 *     item= tbbtrem( tree1, tbbtdfind(tree1,key,NULL), NULL );
 *     item= tbbtrem( tree1, tbbtfind(*tree1,key,compar,arg,NULL), NULL );
 *     tree1= tbbtdfree( tree1, free, NULL );       (* or whatever *)
 * while
 *     TBBT_NODE *root= NULL;
 * would be used like:
 *     node= tbbtfind( root, key );
 *     node= tbbtins( &root, item, key );
 *     node= tbbtrem( &root, tbbtfind(root,key), NULL );
 *     tbbtfree( &root, free, NULL );               (* or whatever *)
 * Never use tbbtfree() on a tree allocated with tbbtdmake() or on a sub-tree
 * of ANY tree.  Never use tbbtdfree() except on a tbbtdmake()d tree.
 */

TBBT_NODE *tbbtdfind
    PROTO((TBBT_TREE *tree, VOIDP key, TBBT_NODE **pp));
TBBT_NODE *tbbtfind
    PROTO((TBBT_NODE *root,VOIDP key, intn (*cmp)(VOIDP,VOIDP,intn),
        intn arg, TBBT_NODE **pp));
/* Locate a node based on the key given.  A pointer to the node in the tree
 * with a key value matching `key' is returned.  If no such node exists, NULL
 * is returned.  Whether a node is found or not, if `pp' is not NULL, `*pp'
 * will be set to point to the parent of the node we are looking for (or that
 * node that would be the parent if the node is not found).  tbbtdfind() is
 * used on trees created using tbbtdmake() (so that `cmp' and `arg' don't have
 * to be passed).  tbbtfind() can be used on the root or any subtree of a tree
 * create using tbbtdmake() and is used on any tree (or subtree) created with-
 * out using tbbtdmake().
 */

TBBT_NODE *tbbtindx
    PROTO(( TBBT_NODE *root, int32 indx ));
/* Locate the node that has `indx' nodes with lesser key values.  This is like
 * an array lookup with the first item in the list having index 0.  For large
 * values of `indx', this call is much faster than tbbtfirst() followed by
 * `indx' tbbtnext()s.  Thus `tbbtindx(&root,0L)' is equivalent to (and almost
 * as fast as) `tbbtfirst(root)'.
 */

TBBT_NODE *tbbtdins
    PROTO((TBBT_TREE *tree, VOIDP item, VOIDP key));
TBBT_NODE *tbbtins
    PROTO((TBBT_NODE **root,VOIDP item,VOIDP key, intn (*cmp)(VOIDP,VOIDP,intn), intn arg ));
/* Insert a new node to the tree having a key value of `key' and a data pointer
 * of `item'.  If a node already exists in the tree with key value `key' or if
 * malloc() fails, NULL is returned (no node is inserted), otherwise a pointer
 * to the inserted node is returned.  `cmp' and `arg' are as for tbbtfind().
 */

VOIDP tbbtrem
    PROTO((TBBT_NODE **root, TBBT_NODE *node, VOIDP *kp ));
/* Remove the node pointed to by `node' from the tree with root `root'.  The
 * data pointer for the deleted node is returned.  If the second argument is
 * NULL, NULL is returned.  If `kp' is not NULL, `*kp' is set to point to the
 * key value for the deleted node.  Examples:
 *     data= tbbtrem( tree, tbbtdfind(tree,key), &kp );  free(data);  free(kp);
 *     data= tbbtrem( &root, tbbtfind(root,key,compar,arg), NULL );
 *     data= tbbtrem( &tree->root, tbbtdfind(tree,key), NULL );
 */

TBBT_NODE *tbbtfirst
    PROTO((TBBT_NODE *root));
TBBT_NODE *tbbtlast
    PROTO((TBBT_NODE *root ));
/* Returns a pointer to node from the tree with the lowest(first)/highest(last)
 * key value.  If the tree is empy NULL is returned.  Examples:
 *     node= tbbtfirst(*tree);
 *     node= tbbtfirst(root);
 *     node= tbbtlast(tree->root);
 *     node= tbbtlast(node);        (* Last node in a sub-tree *)
 */

TBBT_NODE *tbbtnext
    PROTO((TBBT_NODE *node));
TBBT_NODE *tbbtprev
    PROTO((TBBT_NODE *node));
/* Returns a pointer the node from the tree with the next highest (previous
 * lowest) key value relative to the node pointed to by `node'.  If `node'
 * points the last (first) node of the tree, NULL is returned.
 */

TBBT_TREE *tbbtdfree
    PROTO((TBBT_TREE *tree, VOID (*fd)(VOIDP), VOID (*fk)(VOIDP)));
VOID tbbtfree
    PROTO((TBBT_NODE **root, VOID (*fd)(VOIDP), VOID (*fk)(VOIDP)));
/* Frees up an entire tree.  `fd' is a pointer to a function that frees/
 * destroys data items, and `fk' is the same for key values.
 *     void free();
 *       tree= tbbtdfree( tree, free, free );
 *       tbbtfree( &root, free, free );
 * is a typical usage, where keys and data are individually malloc()d.  If `fk'
 * is NULL, no action is done for the key values (they were allocated on the
 * stack, as a part of each data item, or together with one malloc() call, for
 * example) and likewise for `fd'.  tbbtdfree() always returns NULL and
 * tbbtfree() always sets `root' to be NULL.
 */

VOID tbbtprint
	PROTO((TBBT_NODE *node));
/* Prints out the data in a node */

VOID tbbtdump
	PROTO((TBBT_TREE *tree, intn method));
/* Prints an entire tree.  The method variable determines which sort of
 * traversal is used:
 *	-1 : Pre-Order Traversal
 *	 1 : Post-Order Traversal
 *	 0 : In-Order Traversal 
 */

long tbbtcount
	PROTO((TBBT_TREE *tree));

#endif /* TBBT_H */
