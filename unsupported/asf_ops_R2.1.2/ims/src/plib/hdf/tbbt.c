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

/* "tbbt.c" -- Routines for using threaded, balanced, binary trees. */
/* Extended from (added threads to) Knuth 6.2.3, Algorithm A (AVL trees) */
/* Basic tree structure by Adel'son-Vel'skii and Landis */

#include <stdio.h>  /* NULL */
#include "hdf.h"
#define TBBT_INTERNALS
#include "tbbt.h"
#define   Alloc(cnt,typ)   (typ *) HDgetspace( (cnt) * sizeof(typ) )
#define   Free(x)           (HDfreespace((VOIDP)x))

# define   KEYcmp(k1,k2,a)   (  (NULL!=compar) ? (*compar)( k1, k2, a)          \
                                : HDmemcmp( k1, k2, 0<(a) ? (a) : HDstrlen(k1) )  )

VOID tbbt1dump
	PROTO((TBBT_NODE *node,intn method));

/* Returns pointer to end-most (to LEFT or RIGHT) node of tree: */
#ifdef PROTOTYPE
static TBBT_NODE *tbbt_end(TBBT_NODE *root, intn side)
#else
static TBBT_NODE *tbbt_end( root, side )
TBBT_NODE *root;
intn side;
#endif
{
    if(  NULL == root  )
        return( root );
    while(  HasChild( root, side )  ) {
        root= root->link[side];
    }
    return( root );
}

#ifdef PROTOTYPE
TBBT_NODE *tbbtfirst(TBBT_NODE *root)
#else
TBBT_NODE *tbbtfirst( root )
  TBBT_NODE *root;
#endif
{
    return(  tbbt_end( root, LEFT )  );
}

#ifdef PROTOTYPE
TBBT_NODE *tbbtlast(TBBT_NODE *root)
#else
TBBT_NODE *tbbtlast( root )
  TBBT_NODE *root;
#endif
{
    return(  tbbt_end( root, RIGHT )  );
}

/* Return address of parent's pointer to neighboring node (to LEFT or RIGHT) **
#ifdef PROTOTYPE
static TBBT_NODE **tbbt_anbr(TBBT_NODE *ptr, intn side )
#else
static TBBT_NODE **tbbt_anbr( ptr, side )
TBBT_NODE *ptr;
intn       side;
#endif
{
  TBBT_NODE **aptr;

    if(  ! HasChild( ptr, side )  )
        return(  & ptr->link[side]  );
    aptr= & ptr->link[side];
    while(  HasChild( *aptr, Other(side) )  )
        aptr= & (*aptr)->link[Other(side)];
    return( aptr );
}*/

/* Returns pointer to neighboring node (to LEFT or RIGHT): */
#ifdef PROTOTYPE
static TBBT_NODE *tbbt_nbr(TBBT_NODE * ptr, intn  side )
#else
static TBBT_NODE *tbbt_nbr( ptr, side )
TBBT_NODE *ptr;
intn       side;
#endif
{
    /* return( *tbbt_anbr(ptr,side) ); */

    if(  ! HasChild( ptr, side )  )
        return(  ptr->link[side]  ); 
/*        return(NULL); */
    ptr= ptr->link[side];
    while(  HasChild( ptr, Other(side) )  )
        ptr= ptr->link[Other(side)];
    return( ptr );
}

/* Returns pointer to node with previous key value: */
#ifdef PROTOTYPE
TBBT_NODE *tbbtnext( TBBT_NODE *node )
#else
TBBT_NODE *tbbtnext( node )
TBBT_NODE *node;
#endif
{
    return(  tbbt_nbr( node, RIGHT )  );
}

/* Returns pointer to node with next key value: */
#ifdef PROTOTYPE
TBBT_NODE *tbbtprev( TBBT_NODE *node)
#else
TBBT_NODE *tbbtprev( node )
TBBT_NODE *node;
#endif
{
    return(  tbbt_nbr( node, LEFT )  );
}

/* tbbtfind -- Look up a node in a tree based on a key value */
/* Returns a pointer to the found node (or NULL) */
#ifdef PROTOTYPE
TBBT_NODE *tbbtfind(TBBT_NODE *root,VOIDP key, intn (*compar) PROTO((VOIDP,VOIDP,intn)),intn arg,
        TBBT_NODE    **pp )
#else
TBBT_NODE *tbbtfind( root, key, compar, arg, pp )
  TBBT_NODE     *root;  /* Pointer to root node of tree (NULL if 0 elements) */
  VOIDP         key;    /* Key value of node we are looking for */
  intn         (*compar)(/* void *k1, void *k2, int arg */);
                        /* memcmp()-like routine to compare key values */
  intn           arg;   /* Third argument for (*compar)() */
  TBBT_NODE    **pp;    /* Place for pointer to parent (or NULL) */
#endif
{
    TBBT_NODE *ptr= root;
    TBBT_NODE *parent= NULL;
    intn       cmp= 1;

#ifdef QAK
printf("tbbtfind(): key=%d\n",(int)key);
#endif
    if(ptr) {
        intn side;

        while(  0 != ( cmp= KEYcmp(key,ptr->key,arg) )  ) {
            parent= ptr;
            side= (cmp<0) ? LEFT : RIGHT;
            if(  ! HasChild( ptr, side )  )
                break;
            ptr= ptr->link[side];
        }
    }
    if(  NULL != pp  )
        *pp= parent;
#ifdef QAK
printf("tbbtfind(): cmp=%d\n",cmp);
#endif
    return( (0==cmp) ? ptr : NULL );
}

/* tbbtdfind -- Look up a node in a "described" tree based on a key value */
/* Returns a pointer to the found node (or NULL) */
#ifdef PROTOTYPE
TBBT_NODE *tbbtdfind(TBBT_TREE *tree,VOIDP key,TBBT_NODE **pp)
#else
TBBT_NODE *tbbtdfind( tree, key, pp )
TBBT_TREE     *tree;  /* Pointer to tree description record */
VOIDP          key;   /* Pointer to key value to find node with */
TBBT_NODE    **pp;    /* Address of buffer for pointer to parent node */
#endif
{
    return(  tbbtfind( tree->root, key, tree->compar, tree->cmparg, pp )  );
}

/* tbbtindx -- Look up the Nth node (in key order) */
/* Returns a pointer to the `indx'th node (or NULL) */
#ifdef PROTOTYPE
TBBT_NODE *tbbtindx(TBBT_NODE *root,int32 indx)
#else
TBBT_NODE *tbbtindx( root, indx )
TBBT_NODE *root;  /* Pointer to root node of tree (NULL if 0 elements) */
int32      indx;  /* Sequence number of desired node (0 for first) */
#endif
{
    TBBT_NODE *ptr= root;

    if(  NULL == ptr  ||  indx < 0  )
        return( NULL );
    while(  indx != (int32)LeftCnt(ptr)  ) {
        if(  indx < (int32)LeftCnt(ptr)  ) {
            indx -= LeftCnt(ptr);
            ptr= ptr->Lchild;
        } else if(  HasChild(ptr,RIGHT)  )
            ptr= ptr->Rchild;
        else
            return( NULL ); /* Only `indx' or fewer nodes in tree */
    }
    return( ptr );
}

/* swapkid -- Often refered to as "rotating" nodes.  ptr and ptr's `side'
 * child, kid, are swapped so ptr becomes kid's `Other(side)' child.
 * Here is how a single swap (rotate) works:
 *
 *           |           --side-->         |
 *         (ptr)                         (kid)
 *        /     \                       /     \
 *    +-A-+    (kid)                 (ptr)    +-C-+
 *    |   |   /     \               /     \   |   |
 *    |...| +-B-+  +-C-+         +-A-+  +-B-+ |...|
 *          |   |  |   |         |   |  |   |
 *          |...|  |...|         |...|  |...|
 * `deep' contains the relative depths of the subtrees so, since we set
 * `deep[1]' (the relative depth of subtree [B]) to 0, `deep[2]' is the depth
 * of [C] minus the depth of [B] (-1, 0, or 1 since `kid' is never too out of
 * balance) and `deep[0]' is the depth of [A] minus the depth of [B].  These
 * values are used to compute the balance levels after the rotation.  Note that
 * [A], [B], or [C] can have depth 0 so `link[]' contains threads rather than
 * pointers to children.
 */
#ifdef PROTOTYPE
static TBBT_NODE *swapkid(TBBT_NODE **root,TBBT_NODE *ptr,intn side )
#else
static TBBT_NODE *swapkid( root, ptr, side )
  TBBT_NODE    **root;  /* Pointer to root of tree */
  TBBT_NODE     *ptr;   /* Pointer to parent node to be swapped */
  intn           side;  /* Which of parent's children is to be swapped */
#endif
{
  TBBT_NODE *kid= ptr->link[side];  /* Sibling to be swapped with parent */
  intn  deep[3];        /* Relative depths of three sub-trees involved. */
  /* 0:ptr->link[Other(side)], 1:kid->link[Other(side)], 2:kid->link[side] */
  TBBT_FLAG ptrflg;     /* New value for ptr->flags (ptr->flags used after set) */

    deep[2]= ( deep[1]= 0 ) + Delta(kid,side);
    deep[0]= Max(0,deep[2]) + 1 - Delta(ptr,side);
    kid->Parent= ptr->Parent;
    ptrflg= SetFlags( ptr, side, Cnt(ptr,side) - Cnt(kid,side) - 1,
            /*deep[1]-deep[0] */ deep[0]-deep[1], HasChild(ptr,Other(side)) && HasChild(kid,Other(side)) );
    if(  HasChild(kid,Other(side))  ) {
#ifdef QAK
printf("swapkid(): Real Child\n");
#endif
        ptr->link[side]= kid->link[Other(side)];    /* Real child */
        ptr->link[side]->Parent= ptr;
    } else {
#ifdef QAK
printf("swapkid(): Thread\n");
#endif
        ptr->link[side]= kid;                       /* Thread */
	}
    /* Update grand parent's pointer: */
    if(  NULL == ptr->Parent  ) {
#ifdef QAK
printf("swapkid(): NULL==ptr->Parent\n");
#endif
        *root= kid;
	}
    else if(  ptr/*->Lchild*/ == ptr->Parent->Lchild  ) {
#ifdef QAK
printf("swapkid(): case b\n");
#endif
#ifdef OLD_WAY
        ptr->Parent->Lchild= ptr;
#else
        ptr->Parent->Lchild= kid;
#endif
	}
    else {
#ifdef QAK
printf("swapkid(): case c\n");
#endif
#ifdef OLD_WAY
        ptr->Parent->Rchild= ptr;
#else
        ptr->Parent->Rchild= kid;
#endif
	}
    ptr->Parent= kid;
    kid->link[Other(side)]= ptr;
#ifndef QAK
    kid->flags= SetFlags( kid, Other(side),
            Cnt(ptr,Other(side)) + 1 + Cnt(kid,Other(side)),
            deep[2]-1-Max(deep[0],deep[1]), HasChild(kid,side) );
#else
    kid->flags= SetFlags( kid, side,
            Cnt(ptr,Other(side)) + 1 + Cnt(kid,Other(side)),
            deep[2]-1-Max(deep[0],deep[1]), HasChild(kid,side) );
#endif
    ptr->flags= ptrflg;
    return( kid );
}

/* balance -- Move up tree, incrimenting number of left children when needed
 * and looking for unbalanced ancestors.  Adjust all balance factors and re-
 * balance through "rotation"s when needed.
 */
#ifdef PROTOTYPE
static VOID balance(TBBT_NODE **root,TBBT_NODE *ptr,intn side,intn added )
#else
static VOID balance( root, ptr, side, added )
  TBBT_NODE    **root;  /* Root of tree being balanced */
  TBBT_NODE     *ptr;   /* Parent that just gained/lost a child */
  intn           side;  /* Which side child was inserted on or removed from */
  intn           added; /* 1 if a child was just added; -1 if just deleted */
#endif
{
  intn deeper= added;  /* 1 if sub-tree got longer; -1 if got shorter */

    while(  NULL != ptr  ) {
        if(  LEFT == side  )        /* One more/fewer left child: */
            if(  0 < added  )
                ptr->flags += (1<<TBBT_BITS);       /* LeftCnt(ptr)++ */
            else
                ptr->flags -= (1<<TBBT_BITS);       /* LeftCnt(ptr)-- */
        if(  0 != deeper  ) {   /* One leg got longer or shorter: */
            if(  deeper == Delta(ptr,side)  ) { /* Became too unbalanced: */
              TBBT_NODE *kid;

#ifdef QAK
printf("balance(): too unbalanced!\n");
#endif
                ptr->flags |= TBBT_DOUBLE;  /* Mark node too unbalanced */
                if(  deeper < 0  )      /* Just removed a node: */
                    side= Other(side);  /* Swap with child from other side. */
                else                    /* Just inserted a node: */
                    deeper= 0;          /* Fix will re-shorten sub-tree. */
                kid= ptr->link[side];
                if(  Heavy(kid,Other(side))  ) {    /* Double rotate needed: */
#ifdef QAK
printf("balance(): double rotate coming up!\n");
#endif
                    kid= swapkid( root, kid, Other(side) );
                    ptr= swapkid( root, ptr, side );
                } else {                    /* Just rotate parent and kid: */
#ifdef QAK
printf("balance(): single rotate coming up!\n");
printf("balance(): root=%p, ptr=%p, side=%d\n",root,ptr,side);
#endif
#ifdef OLD_WAY
                    if(  Heavy(kid,side)  ) /* In this case, sub-tree gets */
                        deeper= 0;  /* re-lengthened after a node removed. */
#else
                    if(  HasChild(kid,side)  ) /* In this case, sub-tree gets */
                        deeper= 0;  /* re-lengthened after a node removed. */
#endif
                    ptr= swapkid( root, ptr, side );
                }
            } else if(  UnBal(ptr)  ) {     /* Just became balanced: */
                ptr->flags &= ~TBBT_UNBAL;
                if(  0 < deeper  ) {  /* Shorter of legs lengthened */
		    ptr->flags|=TBBT_INTERN;  /* Mark as internal node now */
                    deeper= 0;      /* so max length unchanged */
		  } /* end if */
            } else if(  deeper < 0  ) {     /* Just became unbalanced: */
                ptr->flags |= TBBT_HEAVY(Other(side));  /* Other side longer */
                deeper= 0;  /* One of equal legs shortened; max length same */
            } else {                        /* Just became unbalanced: */
                ptr->flags |= TBBT_HEAVY(side); /* 0<deeper: Our side longer */
              } /* end else */
        }
        if(ptr->Parent) {
            if(ptr == (ptr->Parent->Rchild))
                side=RIGHT;
            else
                side=LEFT;
          } /* end if */
        ptr= ptr->Parent;   /* Move up the tree */
    }
    /* total tree depth += deeper; */
}
/* Here is how rotatation rebalances a tree:
 * Either the deletion of a node shortened the sub-tree [A] (to length `h')
 * while [B] or [C] or both are length `h+1'  or  the addition of a node
 * lengthened [B] or [C] to length `h+1' while the other and [A] are both
 * length `h'.  Each case changes `ptr' from being "right heavy" to being
 * overly unbalanced.
 * This           |                      Becomes:      |
 * sub-tree:    (ptr)                                (kid)
 *             /     \          --side-->           /     \
 *         +-A-+    (kid)                        (ptr)   +-C-+
 *         |   |   /     \                      /     \  |   |
 *         | h | +-B-+  +-C-+               +-A-+  +-B-+ | h |
 *         |   | |   |  |   |               |   |  |   | |   |
 *         +---+ | h |  | h |               | h |  | h | +---+
 *         : - : |   |  |   |               |   |  |   | : 1 :
 *         `- -' +---+  +---+               +---+  +---+ + - +
 *               : 1 :  : 1 :                      : 1 :
 *               + - +  + - +                      + - +
 *
 * However, if [B] is long (h+1) while [C] is short (h), a double rotate is
 * required to rebalance.  In this case, [A] was shortened or [X] or [Y] was
 * lengthened so [A] is length `h' and one of [X] and [Y] is length `h' while
 * the other is length `h-1'.  Swap `kid' with `babe' then `ptr' with `babe'.
 * This          |                         Becomes:     |
 * sub-tree:   (ptr)                                  (babe)
 *            /     \             --side-->          /      \
 *       +-A-+       (kid)                      (ptr)       (kid)
 *       |   |      /     \                    /     \     /     \
 *       | h |    (babe)   +-C-+             +-A-+ +-X-+ +-Y-+ +-C-+
 *       |   |   /      \  |   |             |   | |h-1| |h-1| |   |
 *       +---+ +-X-+ +-Y-+ | h |             | h | +---+ +---+ | h |
 *       : - : |h-1| |h-1| |   |             |   | : 1 : : 1 : |   |
 *       `- -' +---+ +---+ +---+             +---+ + - + + - + +---+
 *             : 1 : : 1 :
 *             + - + + - +
 *
 * Note that in the node insertion cases total sub-tree length always increases
 * by one then decreases again so after the rotation(s) no more rebalancing is
 * required.  In the node removal cases, the single rotation reduces total sub-
 * tree length unless [B] is length `h+1' (`ptr' ends of "right heavy") while
 * the double rotation ALWAYS reduces total sub-tree length.  Thus removing a
 * single node can require log(N) rotations for rebalancing.  On average, only
 * are usually required.
 */

/* Returns pointer to inserted node (or NULL) */
#ifdef PROTOTYPE
TBBT_NODE *tbbtins(TBBT_NODE **root,VOIDP item,VOIDP key,intn (*compar)
        PROTO((VOIDP k1,VOIDP k2,intn arg)),intn arg )
#else
TBBT_NODE *tbbtins( root, item, key, compar, arg )
TBBT_NODE    **root;  /* Address of pointer to root node of tree */
VOIDP         item;  /* Data to be associated with node */
VOIDP         key;   /* Key of node to be added */
intn         (*compar)(/* void *k1, void *k2, int arg */);
                        /* memcmp()-like routine to compare key values */
intn           arg;   /* Third argument for (*compar)() */
#endif
{
    intn cmp;
    TBBT_NODE *ptr, *parent;

    if(NULL != tbbtfind( *root, (key ? key : item), compar, arg, &parent )
            || NULL == ( ptr= Alloc(1,TBBT_NODE) ) )
        return( NULL );
    ptr->data= item;
    ptr->key= key ? key : item;
    ptr->Parent= parent;
    ptr->flags= 0L;     /* No children on either side */
    if(  NULL == parent  ) {    /* Adding first node to tree: */
        *root= ptr;
        ptr->Lchild= ptr->Rchild= NULL;
        return( ptr );
    }
    cmp= KEYcmp( ptr->key, parent->key, arg);
#ifdef QAK
printf("tbbtins(): cmp=%d\n",cmp);
#endif
    if(  cmp < 0  ) {
        ptr->Lchild= parent->Lchild;    /* Parent's thread now new node's */
        ptr->Rchild= parent;            /* New nodes right thread is parent */
        parent->Lchild= ptr;            /* Parent now has a left child */
    } else {
        ptr->Rchild= parent->Rchild;
        ptr->Lchild= parent;
        parent->Rchild= ptr;
    }
#ifdef QAK
printf("tbbtins(): before balancing\n");
tbbt1dump(*root,0);
#endif
    balance( root, parent, (cmp<0) ? LEFT : RIGHT, 1 );
#ifdef QAK
printf("tbbtins(): after balancing\n");
tbbt1dump(*root,0);
#endif
    return( ptr );
}

/* tbbtdins -- Insert a node into a "described" tree */
             /* Returns a pointer to the inserted node */
#ifdef PROTOTYPE
TBBT_NODE *tbbtdins(TBBT_TREE *tree,VOIDP item,VOIDP key )
#else
TBBT_NODE *tbbtdins( tree, item, key )
TBBT_TREE *tree;      /* Pointer to tree description record */
VOIDP     item;      /* Pointer to data item for new node */
VOIDP     key;       /* Pointer to key value for new node (or NULL) */
#endif
{
    TBBT_NODE *ret_node; /* the node to return */

    ret_node=tbbtins( &(tree->root), item, key, tree->compar, tree->cmparg );
    if(ret_node!=NULL)
	tree->count++;
    return(ret_node);
}

/* tbbtrem -- Remove a node from a tree.  You pass in the address of the
 * pointer to the root node of the tree along, a pointer to the node you wish
 * to remove, and optionally the address of a pointer to hold the address of
 * the key value of the deleted node.  The second argument is usually the
 * result from a lookup function call (tbbtfind, tbbtdfind, or tbbtindx) so if
 * it is NULL, tbbtrem returns NULL.  Otherwise tbbtrem removes the node from
 * the tree and returns a pointer to the data item for that node and, if the
 * third argument is not NULL, the address of the key value for the deleted
 * node is placed in the buffer that it points to.
 */
              /* Data item pointer of deleted node is returned */
#ifdef PROTOTYPE
VOIDP tbbtrem(TBBT_NODE **root,TBBT_NODE *node,VOIDP *kp)
#else
VOIDP tbbtrem( root, node, kp )
  TBBT_NODE **root; /* Root of tree to delete node from */
  TBBT_NODE  *node; /* Pointer to node to be deleted */
  VOIDP      *kp;   /* NULL or address of buffer for pointer to key value */
#endif
{
  TBBT_NODE *leaf;  /* Node with one or zero children */
  TBBT_NODE *par;   /* Parent of `leaf' */
  TBBT_NODE *next;  /* Next/prev node near `leaf' (`leaf's `side' thread) */
  intn  side;       /* `leaf' is `side' child of `par' */
  VOIDP data;       /* Saved pointer to data item of deleted node */

    if(  NULL == node  )
        return( NULL ); /* Argument couldn't find node to delete */
    data= node->data;   /* Save pointer to data item to be returned at end */
    if(  NULL != kp  )
        *kp= node->key;
    /* If the node to be removed is "internal" (children on both sides), we
     * replace it with it's previous (or next) node in the tree and delete that
     * previous (next) node (which has one or no children) instead. */
    if(  Intern(node)  ) {              /* Replace with a non-internal node: */
        if(  Heavy(node,RIGHT)  )       /* Pick "near-leaf" node from the */
            side= LEFT;                 /* heavier of the sub-trees. */
        else if(  Heavy(node,LEFT)  )
            side= RIGHT;
        else        /* If no sub-tree heavier, pick at "random" for "better */
            side= (0x10 & *(short *)&node) ? LEFT : RIGHT;      /* balance" */
        leaf= tbbt_nbr( next= node, Other(side) );
        par= leaf->Parent;
        if(  par == next  ) {   /* Case 2x: `node' had exactly 2 descendants */
            side= Other(side);  /* Transform this to Case 2 */
            next= leaf->link[side];
        }
        node->data= leaf->data;
        node->key= leaf->key;
    } else {                /* Node has one or zero children: */
        leaf= node;         /* Simply remove THIS node */
        par= leaf->Parent;
        if(  NULL == par  ) { /* Case 3: Remove root (of 1- or 2-node tree) */
            side= (intn)UnBal(node);  /* Which side root has a child on */
            if(  side  ) {  /* Case 3a: Remove root of 2-node tree: */
                *root= leaf= node->link[side];
                leaf->Parent= leaf->link[Other(side)]= NULL;
                leaf->flags= 0; /* No left children, balanced, not internal */
            } else          /* Case 3b: Remove last node of tree: */
                *root= NULL;
            Free( node );
            return( data );
        }
        side=  ( par->Rchild == leaf ) ? RIGHT : LEFT;
        next= leaf->link[side];
    }
    /* Now the deletion has been reduced to the following cases (and Case 3 has
     * been handled completely above and Case 2x has been transformed into
     * Case 2).  `leaf' is a node with one or zero children that we are going
     * to remove.  `next' points where the `side' thread of `leaf' points.
     * `par' is the parent of `leaf'.  The only posibilities (not counting
     * left/right reversals) are shown below:
     *       [Case 1]                  [Case 2]              [Case 2x]
     *            (next)                 (next)         ^         (next & par)
     *           /  ^   \               /  ^   \        |        /  ^         \
     *     . . .    |             . . .    |            |  (leaf)   /
     *   /          |           /          |            \_/      \_/
     * (par)        |         (par)        |             ^threads^
     *      \       |              \       |
     *     (leaf)   /             (leaf)   /            [Case 3a]    [Case 3b]
     *    /  ^   \_/<thread             \_/<thread       (root)
     * (n)   /                                                 \       (root)
     *    \_/<thread        --"side"-->                         (n)
     * Note that in Cases 1 and 2, `leaf's `side' thread can be NULL making
     * `next' NULL as well.  If you remove a node from a 2-node tree, removing
     * the root falls into Case 3a while removing the only leaf falls into
     * Case 2 (with `next' NULL and `par' the root node). */
    if(  ! UnBal(leaf)  ) {         /* Case 2: `leaf' has no children: */
        par->link[side]= leaf->link[side];
        par->flags &= ~( TBBT_INTERN | TBBT_HEAVY(side) );
    } else {                        /* Case 1: `leaf' has one child: */
      TBBT_NODE *n= leaf->link[Other(side)];
        par->link[side]= n;
        n->Parent= par;
        n->link[side]= next;
    }
    Free( leaf );
    balance( root, par, side, -1 );
	((TBBT_TREE *)root)->count--;
    return( data );
}

/* tbbtdmake - Allocate a new tree description record for an empty tree */
/* Returns a pointer to the description record */
#ifdef PROTOTYPE
TBBT_TREE *tbbtdmake(intn (*cmp) PROTO((VOIDP k1,VOIDP k2,intn arg)), intn arg)
#else
TBBT_TREE *tbbtdmake( cmp, arg )
intn (*cmp)(/* void *k1, void *k2, int arg */);
                /* memcmp()-link routine for comparing keys (or NULL) */
intn   arg;   /* Third argument for (*cmp)() */
#endif
{
  TBBT_TREE *tree= Alloc( 1, TBBT_TREE );

    if(  NULL == tree  )
        return( NULL );
    tree->root= NULL;
    tree->count=0;
    tree->compar= cmp;
    tree->cmparg= arg;
    return( tree );
}

#ifdef WASTE_STACK
/* You can have a very simple recursive version that wastes lots of stack
 * space, this next less-simple recursive version that wastes less stack space,
 * or the last non-recursive version which isn't simple but saves stack space.
 */
static VOID (*FD) PROTO((VOIDP item)), (*FK) PROTO((VOIDP key));
#ifdef PROTOTYPE
static VOID tbbt1free( TBBT_NODE *node)
#else
static VOID tbbt1free( node )
TBBT_NODE *node;
#endif
{
    if(  HasChild(node,LEFT)  )     tbbt1free( node->Lchild );
    if(  HasChild(node,RIGHT)  )    tbbt1free( node->Rchild );
    if(  NULL != FD  )              (*FD)( node->data );
    if(  NULL != FK  )              (*FK)( node->key );
    Free( node );
}

#ifdef PROTOTYPE
VOID tbbtfree(TBBT_NODE **root, VOID (*fd) PROTO((VOIDP item)),VOID (*fk) PROTO((VOIDP key)))
#else
VOID tbbtfree( root, fd, fk )
TBBT_NODE    **root;
VOID         (*fd)(/* void *item */);
VOID         (*fk)(/* void *key */);
#endif
{
    if(  NULL == *root  )
        return;
    FD= fd;   FK= fk;
    tbbt1free( *root );
    *root= NULL;
}
#else /* WASTE_STACK */

/* tbbtfree() - Free an entire tree not allocated with tbbtdmake(). */
#ifdef PROTOTYPE
VOID tbbtfree(TBBT_NODE **root, VOID (*fd) PROTO((VOIDP item)),VOID (*fk) PROTO((VOIDP key)))
#else
VOID tbbtfree( root, fd, fk )
TBBT_NODE    **root;                  /* Address of pointer to root node */
VOID         (*fd)(/* void *item */); /* Routine to free data items */
VOID         (*fk)(/* void *key */);  /* Routine to free key values */
#endif
{
  TBBT_NODE *par, *node= *root;

    while(  NULL != *root  ) {  /* While nodes left to be free()d */
        /* First time at this node (just moved down a new leg of tree) */
        if(  ! HasChild(node,LEFT)  )    node->Lchild= NULL;
        if(  ! HasChild(node,RIGHT)  )   node->Rchild= NULL;
        do {
            par= NULL;  /* Assume we aren't ready to move up tree yet */
            if(  NULL != node->Lchild  )
                node= node->Lchild; /* Move down this leg next */
            else if(  NULL != node->Rchild  )
                node= node->Rchild; /* Move down this leg next */
            else {                  /* No children; free node an move up: */
                par= node->Parent;  /* Move up tree (stay in loop) */
                if(  NULL != fd  )   (*fd)( node->data );
                if(  NULL != fk  )   (*fk)( node->key );
                if(  NULL == par  )     /* Just free()d last node */
                    *root= NULL;    /* NULL=par & NULL=*root gets fully out */
                else if(  node == par->Lchild  )
                    par->Lchild= NULL;  /* Now no longer has this child */
                else
                    par->Rchild= NULL;  /* Ditto */

                Free( node );

                node = par;  /* Move up tree; remember which node to do next */
            }
        } while(  NULL != par  );   /* While moving back up tree */
    }
}
#endif /* WASTE_STACK */

#ifdef PROTOTYPE
VOID tbbtprint(TBBT_NODE *node)
#else
VOID tbbtprint(node)
TBBT_NODE *node;
#endif
{
	printf("node=%p, key=%p, data=%p, flags=%x\n",node,node->key,node->data,node->flags);
#ifdef QAK
printf("*key=%d\n",*(int32 *)(node->key));
#endif
vprint(node->key);
	printf("Lchild=%p, Rchild=%p, Parent=%p\n",node->Lchild,node->Rchild,node->Parent);
}	/* end tbbtprint() */

#ifdef PROTOTYPE
VOID tbbt1dump(TBBT_NODE *node,intn method)
#else
VOID tbbt1dump(node,method)
TBBT_NODE *node;
intn method;
#endif
{
	if(node==NULL)
		return;
	switch(method) {
		case -1:	/* Pre-Order Traversal */
			tbbtprint(node);
			if(HasChild(node,LEFT))
				tbbt1dump(node->Lchild,method);
			if(HasChild(node,RIGHT))
				tbbt1dump(node->Rchild,method);
			break;

		case 1:		/* Post-Order Traversal */
			if(HasChild(node,LEFT))
				tbbt1dump(node->Lchild,method);
			if(HasChild(node,RIGHT))
				tbbt1dump(node->Rchild,method);
			tbbtprint(node);
			break;

		case 0:		/* In-Order Traversal */
		default:
			if(HasChild(node,LEFT))
				tbbt1dump(node->Lchild,method);
			tbbtprint(node);
			if(HasChild(node,RIGHT))
				tbbt1dump(node->Rchild,method);
			break;

	  }	/* end switch() */
}	/* end tbbt1dump() */

#ifdef PROTOTYPE
VOID tbbtdump(TBBT_TREE *tree,intn method)
#else
VOID tbbtdump(tree, method)
TBBT_TREE *tree;
intn method;
#endif
{
    printf("Number of nodes in the tree: %ld\n",tree->count);
	tbbt1dump(tree->root,method);
}	/* end tbbtdump() */

/* Always returns NULL */
#ifdef PROTOTYPE
TBBT_TREE *tbbtdfree(TBBT_TREE *tree,VOID (*fd) PROTO((VOIDP item)), VOID (*fk) PROTO((VOIDP key)))
#else
TBBT_TREE *tbbtdfree( tree, fd, fk )
TBBT_TREE *tree;                  /* Pointer to tree description record */
VOID     (*fd)(/* void *item */); /* Routine to free data items */
VOID     (*fk)(/* void *key */);  /* Routine to free key values */
#endif
{
    tbbtfree( &tree->root, fd, fk );
    Free( tree );
    return( NULL );
}


/* returns the number of nodes in the tree */
#ifdef PROTOTYPE
long tbbtcount(TBBT_TREE *tree)
#else
long tbbtcount(tree)
TBBT_TREE *tree;                  /* Pointer to tree description record */
#endif
{
    if(tree==NULL)
        return(-1);
    else
	return(tree->count);
}

