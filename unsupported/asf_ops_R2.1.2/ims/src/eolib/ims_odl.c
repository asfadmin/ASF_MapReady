static char *sccs = "@(#)ims_odl.c	5.6  06/27/97";
/***************************************************************
**
** File:        ims_odl.c
**
** Function:    Library functions for building ODL files.
**
** Author:      Dan Crichton
**
** Date:        9/27/95
**
*****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_timeConv.h>
#include <ims_keyword.h>

#include <odldef.h>
#include <odlinter.h>
#include <ims_odl.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_odl.h.
** They are listed here for reference.
**
** int ims_addODLObject(IMS_MSG_STRUCT *, IMS_ODL_TREE *,
**   IMS_ODL_TREE **, char *, int, int );
** int ims_addODLKeyword(IMS_MSG_STRUCT *, IMS_ODL_TREE *, char *, int,
**      char *);
** int ims_buildPMF(IMS_MSG_STRUCT *, IMS_ODL_TREE *, char *, FILE *);
** int ims_create_cmn_hdr(IMS_MSG_STRUCT *, IMS_ODL_TREE **, char *,
**   int);
** int ims_create_catalog(IMS_MSG_STRUCT *, IMS_ODL_TREE *,
**   IMS_KEYWORD_ARRAY [], int);
** int ims_buildAggregate(IMS_MSG_STRUCT *, IMS_ODL_TREE *,AGGREGATE *);
** int ims_parseODLFile (IMS_MSG_STRUCT *, char *, char *,
**   IMS_KEYWORD_LIST **);
** int ims_parseODLBuffer (IMS_MSG_STRUCT *, char *, char *,
**   IMS_KEYWORD_LIST **);
** int ims_findListItem (IMS_MSG_STRUCT *, IMS_KEYWORD_LIST *, char *,
**   int, int, IMS_KEYWORD_LIST **);
** void ims_freeKeywordList (IMS_KEYWORD_LIST *);
*/

/*
** Local Functions.
*/
static  int build_leafs (IMS_MSG_STRUCT *, IMS_ODL_TREE *, AGGREGATE *);
static int populateKeywordList (IMS_MSG_STRUCT *, AGGREGATE, char *,
    IMS_KEYWORD_LIST **);
static  int ims_aggregateToTree ( IMS_MSG_STRUCT *, AGGREGATE,
    IMS_ODL_TREE **, int );

/***************************************************************
**
** ims_buildPMF
**
*****************************************************************/
int ims_buildPMF(
    IMS_MSG_STRUCT *msgDesc,
    IMS_ODL_TREE *tree_ptr,
    char *filename,
    FILE *fptr)
{

    int open = TRUE;
    AGGREGATE root;
    int status;

    /*
    ** If file is not open, then need to open it.
    */


    if (fptr == NULL)
    {
        open = FALSE;
        if (filename == NULL)
        {
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Filename not specified.  Can't build PMF file.");
            return(IMS_ERROR);
        }

        if ((fptr = fopen(filename, "w")) == NULL)
        {
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not open file %s: %s.", filename,
                strerror(errno));
            return(IMS_ERROR);
        }

    }

    /*
    ** Recursively build the tree links.
    */
    root = NULL;

    if (build_leafs(msgDesc, tree_ptr, &root) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not build PMF file. ");
        status = IMS_ERROR;
        goto errorExit;
    }

    /*
    ** Write to disk
    */

    WriteLabel(fptr, root);
    RemoveAggregate(root);

    status = IMS_OK;

errorExit:

    if (open == FALSE)
    {
        (void ) fclose(fptr);
    }
    return(status);
}

/***************************************************************
**
** ims_buildAggregate
**
*****************************************************************/
int ims_buildAggregate(
    IMS_MSG_STRUCT *msgDesc,
    IMS_ODL_TREE *tree_ptr,
    AGGREGATE *rootAggregate)
{

    AGGREGATE root;

    /*
    ** Recursively build the tree links.
    */
    root = NULL;

    if (build_leafs(msgDesc, tree_ptr, &root) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not build root aggregate. ");
        return(IMS_ERROR);
    }

    *rootAggregate = root;

    return(IMS_OK);

}

/***************************************************************
**
** build_leafs
**
**  Take the IMS_ODL_TREE structure and process the child and next leafs
**  to build the ODL tree.
**
*****************************************************************/
static  int build_leafs(
    IMS_MSG_STRUCT *msgDesc,
    IMS_ODL_TREE *tree_ptr,
    AGGREGATE *parent)
{
    PARAMETER param;
    AGGREGATE agr;


    /*
    ** If the tree is built then return.
    */

    if (tree_ptr == NULL)
    {
        return(IMS_OK);
    }

    /*
    ** If the parent is null, and the type is not an object, then
    ** this is an error because we don't have a root.
    */

    if ((*parent == NULL) && (tree_ptr->type != IMS_OBJECT)  &&
        (tree_ptr->type != IMS_ODL_GROUP) ){
        (void) ims_msg(msgDesc, IMS_ERROR,
            "ODL tree list must have a root aggregate.");
        return(IMS_ERROR);
    }

    if (tree_ptr->type == IMS_OBJECT)
    {
        /*
        ** Create object
        */

        if (*parent == NULL)
        {
            *parent = NewAggregate(NULL, KA_OBJECT, "root", NULL);
            agr = NewAggregate(*parent, KA_OBJECT, tree_ptr->node_name,
                NULL);
        }
        else
        {
            agr = NewAggregate(*parent, KA_OBJECT, tree_ptr->node_name,
                NULL);
        }

        /*
        ** Generate all of the keywords and embedded objects.
        */

        (void) build_leafs(msgDesc, tree_ptr->children, &agr);

        /*
        ** Generate next object or attribute for current parent
        */

        (void) build_leafs(msgDesc, tree_ptr->next, parent);
    }
    else  if (tree_ptr->type == IMS_ODL_GROUP){
        /*
        ** Create group
        */
        if (*parent == NULL){
            *parent = NewAggregate(NULL, KA_GROUP, "root", NULL);
            agr = NewAggregate(*parent, KA_GROUP, tree_ptr->node_name,
                NULL);
        }
        else{
            agr = NewAggregate(*parent, KA_GROUP, tree_ptr->node_name,
                NULL);
        }

        /*
        ** Generate all of the keywords and embedded objects.
        */
        (void) build_leafs(msgDesc, tree_ptr->children, &agr);

        /*
        ** Generate next object or attribute for current parent
        */
        (void) build_leafs(msgDesc, tree_ptr->next, parent);
    }
    else if (tree_ptr->type == IMS_KEYWORD){
        /*
        ** Create attribute
        */
        if (tree_ptr->value.valid == 1)
        {
           param = NewParameter(*parent, KP_ATTRIBUTE,
            tree_ptr->node_name);

           param->value_kind = KV_SCALAR;
           NewValue(param, &(tree_ptr->value));
        }

       /*
       ** Process next attribute. Child attributes are ignored.
       */
        (void) build_leafs(msgDesc, tree_ptr->next, parent);
    }
    return(IMS_OK);
}


/***************************************************************
**
** ims_addODLObject
**
** Add an object to the IMS ODL tree structure.  The object is either
** a child of the current object pointed to by tree_ptr, or it is a
** new object.
*****************************************************************/

int ims_addODLObject(
    IMS_MSG_STRUCT *msgDesc,
    IMS_ODL_TREE *tree_ptr,
    IMS_ODL_TREE **new_tree,
    char *keyword,
    int child,
    int  type )  /*  IMS_OBJECT or IMS_ODL_GROUP  */
{

    IMS_ODL_TREE *ptr, *old_ptr;

    /*
    ** Need to create root.
    */


    ptr = tree_ptr;
    old_ptr = tree_ptr;

    if ((tree_ptr != NULL) && (tree_ptr->next != NULL) &&
        (child == FALSE)){
        while (ptr != NULL)
        {
            old_ptr = ptr;
            ptr = ptr->next;
        }
    }

    /*
    ** Create a new node.
    */

    ptr = malloc(sizeof(IMS_ODL_TREE));

    if (ptr == NULL)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not allocate memory for new object.");
        return(IMS_ERROR);
    }


    /*
    ** Add to list
    */

    if ((tree_ptr != NULL) && (tree_ptr->children == NULL) &&
        (child == TRUE)){
        tree_ptr->children = ptr;
    }
    else if ((tree_ptr!= NULL) && (tree_ptr->next == NULL) &&
        (child == FALSE)){
        tree_ptr->next = ptr;
    }
    else if ((tree_ptr != NULL) && (child == FALSE))
    {
        old_ptr->next = ptr;
    }
    else if ((tree_ptr != NULL) && (child == TRUE))
    {
        if (tree_ptr->children == NULL)
            tree_ptr->children = ptr;
        else
        {
            old_ptr = tree_ptr->children;

            while (old_ptr->next != NULL)
            {
                old_ptr = old_ptr->next;
            }
            old_ptr->next = ptr;
        }

    }

    /*
    ** Set values
    */

    ptr->node_name = malloc(strlen(keyword) + 1);

    if (ptr->node_name == NULL)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not allocate memory for ODL label.");
        return(IMS_FATAL);
    }

    (void )strcpy(ptr->node_name, keyword);

    ptr->type = type;

    ptr->children = NULL;
    ptr->next = NULL;

    *new_tree = ptr;

    return(IMS_OK);
}


/***************************************************************
**
** ims_addODLKeyword
**
** Add a new keyword to the current object pointed to by tree_ptr.
*****************************************************************/
int ims_addODLKeyword(
    IMS_MSG_STRUCT *msgDesc,
    IMS_ODL_TREE *tree_ptr,
    char *keyword,
    int type,
    char *value)
{
    IMS_ODL_TREE *ptr, *old_ptr;
    char *sp;

    ptr = tree_ptr;
    old_ptr = tree_ptr;

    if (tree_ptr->children != NULL)
    {
        ptr = ptr->children;

        while (ptr != NULL)
        {
            old_ptr = ptr;
            ptr = ptr->next;
        }
    }

    /*
    ** Create a new node.
    */

    ptr = malloc(sizeof(IMS_ODL_TREE));

    if (ptr == NULL)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not allocate memory for keyword.");
        return(IMS_ERROR);
    }

    ptr->next = NULL;
    ptr->children = NULL;



    /*
    ** Add to list
    */

    if (tree_ptr->children == NULL)
    {
        tree_ptr->children = ptr;
    }
    else
    {
        old_ptr->next = ptr;
    }

    /*
    ** Set values
    */

    /*
    ** Create keyword label.
    */

    ptr->type = IMS_KEYWORD;

    ptr->node_name = malloc(strlen(keyword) + 1);

    (void ) strcpy(ptr->node_name, keyword);

    if (ptr->node_name == NULL)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not allocate memory for ODL label.");
        return(IMS_FATAL);
    }

    if (value == NULL)
    {
        switch(type)
        {
            case TV_INTEGER:
                ptr->value.type      = TV_INTEGER;
                ptr->value.length    = 0;
                ptr->value.valid     = 1;
                ptr->value.precision = 0;
                ptr->value.value.integer.units  = NULL;
                break;

            case TV_REAL:
                ptr->value.type      = TV_REAL;
                ptr->value.length    = 0;
                ptr->value.valid     = 1;
                break;

            case TV_SYMBOL:
                ptr->value.type      = TV_SYMBOL;
                ptr->value.length    = 0;
                ptr->value.format    = 0;
                ptr->value.precision = 0;
                ptr->value.valid = 1;
                break;

            case TV_STRING:
                ptr->value.type      = TV_STRING;
                ptr->value.length    = 0;
                ptr->value.format    = 0;
                ptr->value.precision = 0;
                ptr->value.valid = 1;
                break;


            case TV_DATE_TIME:
                ptr->value.type = TV_DATE_TIME;
                ptr->value.length = 0;
                ptr->value.valid = 1;
                ptr->value.format = 0;
                ptr->value.precision = 0;
                break;

        }
        return(IMS_OK);


    }

    /*
    ** Setup value data
    */

    switch(type)
    {
        case TV_INTEGER:
            ptr->value = ODLConvertInteger(value, strlen(value));
            break;

        case TV_REAL:
            ptr->value = ODLConvertReal(value, strlen(value));
            break;

        case TV_SYMBOL:
            ptr->value = ODLConvertSymbol(value, strlen(value), 2);
            break;

        case TV_STRING:
            ptr->value = ODLConvertString(value, strlen(value));
            break;

        case TV_DATE:
            ptr->value = ODLConvertDate(value, strlen(value));
            break;

        case TV_TIME:
            ptr->value = ODLConvertTime(value, strlen(value));
            break;

        case TV_DATE_TIME:
            /*
            ** ODL library modifies read-only memory addresses.
            ** This can bus error therefore we will setup the
            ** value data structure ourselves.
            */
#if 0
            ptr->value = ODLConvertDateTime(value, strlen(value));
#endif
            ptr->value.type = type;
            ptr->value.length = strlen(value);
            ptr->value.valid = 1;
            ptr->value.format = 0;
            ptr->value.precision = 0;
            ODLExtractDate(value, &(ptr->value));
            sp = strchr(value, 'T') + 1;
            ODLExtractTime(sp, &(ptr->value));
            break;

    }


    return(IMS_OK);
}


/***************************************************************
**
** ims_attachODLData()
**
*****************************************************************/

int ims_attachODLData(
    IMS_ODL_TREE *tree,
    char *data)
{
    IMS_ODL_TREE *ptr = tree;

    /*
    ** Attach data information to keyword which is last item
    ** attach to the object.
    */

    ptr = tree->children;

    while ((ptr != NULL) && (ptr->next != NULL))
    {
        ptr = ptr->next;
    }

    if (ptr != NULL)
    {
        ptr->data = (char *) data;
    }
    return(IMS_OK);
}

/***************************************************************
**
** ims_delete_tree
**
*****************************************************************/
int ims_delete_tree(
    IMS_MSG_STRUCT *msgDesc,
    IMS_ODL_TREE *tree_ptr)
{

    /*
    ** If the tree is built then return.
    */

    if (tree_ptr== NULL)
    {
        return(IMS_OK);
    }

    (void) ims_delete_tree(msgDesc, tree_ptr->next);
    tree_ptr->next = NULL;

    (void) ims_delete_tree(msgDesc, tree_ptr->children);
    tree_ptr->children = NULL;

    if (tree_ptr->node_name != NULL)
    {
        free(tree_ptr->node_name);
    }

    free(tree_ptr);

    return(IMS_OK);
}


/***************************************************************
**
** ims_create_cmn_hdr
**
*****************************************************************/

int ims_create_cmn_hdr(
    IMS_MSG_STRUCT *msgDesc,
    IMS_ODL_TREE **cmn_hdr,
    char *msg_type,
    int num_recs)
{

    IMS_NUMERIC_DATE dateStruct;
    char num_recs_str[15];
    char time[IMS_DATETIME_LEN+1];

    if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,"Could not get current date");
        return(IMS_ERROR);
    }

    ims_numericDateToIMSA(&dateStruct, time);

    (void) ims_addODLObject(msgDesc, NULL, cmn_hdr,
                "COMMON_HEADER", FALSE, IMS_OBJECT);

    (void) ims_addODLKeyword(msgDesc, *cmn_hdr, "TIME", TV_DATE_TIME,
        time);

    (void) ims_addODLKeyword(msgDesc, *cmn_hdr, "MSG_TYPE", TV_STRING,
        msg_type);

    (void) ims_addODLKeyword(msgDesc, *cmn_hdr, "DESTINATION",
        TV_STRING, "IMS");

    (void) ims_addODLKeyword(msgDesc, *cmn_hdr, "SOURCE", TV_STRING,
        "IMS");

    (void) sprintf(num_recs_str, "%d", num_recs);

    (void) ims_addODLKeyword(msgDesc, *cmn_hdr, "NUMBER_OF_RECORDS",
        TV_INTEGER, num_recs_str);

    return(IMS_OK);

}

/***************************************************************
**
** ims_create_catalog
**
*****************************************************************/

int ims_create_catalog(
    IMS_MSG_STRUCT *msgDesc,
    IMS_ODL_TREE *hdr,
    IMS_KEYWORD_ARRAY keyword[],
    int keyword_count)
{
    IMS_ODL_TREE *catalog;
    int i;

    (void) ims_addODLObject(msgDesc, hdr, &catalog,
                "CATALOG_METADATA", FALSE, IMS_OBJECT);

    for (i = 0; i < keyword_count; i++)
    {
        (void) ims_addODLKeyword(msgDesc, catalog, keyword[i].label,
            keyword[i].type, keyword[i].value);
    }
    return(IMS_OK);
}

/***************************************************************
**
** ims_parseODLFile ()
**
** Parse an ODL file using the ODL ReadLabel() function.
**
*****************************************************************/

int ims_parseODLFile (
    IMS_MSG_STRUCT *msgDesc,
    char *fileSpec,
    char *targetObject,
    IMS_KEYWORD_LIST **keywordList)
{
    FILE *fd;
    AGGREGATE rootAggregate;
    int status;

    /*
    ** Open the Metadata file for read.
    */
    if ((fd = fopen (fileSpec, "r")) == (FILE *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Unable to open ODL file '%s'. %s",
            fileSpec, strerror (errno));
        return (IMS_ERROR);
    }

    /*
    ** Allocate the structure for the root node of the ODL tree.
    */
    if ((rootAggregate = NewAggregate (NULL, KA_GROUP, "root", NULL)) ==
        (AGGREGATE) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Unable to allocate the structure for the root node "
            "of the ODL tree.");
        (void) fclose (fd);
        return (IMS_FATAL);
    }

    /*
    ** Call the ODL function to read and parse the Metadata file
    ** into the ODL tree.
    */
    if ((ReadLabel (fd, rootAggregate)) == 0)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "An error occured while parsing the ODL file '%s'.",
            fileSpec);
        (void) RemoveAggregate (rootAggregate);
        (void) fclose (fd);
        return (IMS_ERROR);
    }

    /*
    ** Populate the keyword list from the ODL tree.
    */
    if ((status = populateKeywordList (msgDesc, rootAggregate,
        targetObject, keywordList)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not populate the keyword list.");
        (void) RemoveAggregate (rootAggregate);
        (void) fclose (fd);
        return (status);
    }

    /*
    ** Free the entire ODL tree and close the file.
    */
    if ((RemoveAggregate (rootAggregate)) != (AGGREGATE) NULL)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
            "Could not free the ODL tree structure.");
    }
    (void) fclose (fd);

    return (IMS_OK);
}

/***************************************************************
**
** ims_ODLFileToTree ()
**
** Load an ODL file into the IMS ODL Tree Structure.
**
*****************************************************************/

int ims_ODLFileToTree (
    IMS_MSG_STRUCT *msgDesc,
    char *fileSpec,
    IMS_ODL_TREE **tree)
{
    FILE *fd;
    AGGREGATE rootAggregate;
    int status;

    /*
    ** Open the Metadata file for read.
    */
    if ((fd = fopen (fileSpec, "r")) == (FILE *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Unable to open ODL file '%s'. %s",
            fileSpec, strerror (errno));
        return (IMS_ERROR);
    }

    /*
    ** Allocate the structure for the root node of the ODL tree.
    */
    if ((rootAggregate = NewAggregate (NULL, KA_GROUP, "root", NULL)) ==
        (AGGREGATE) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Unable to allocate the structure for the root node of "
            "the ODL tree.");
        (void) fclose (fd);
        return (IMS_FATAL);
    }

    /*
    ** Call the ODL function to read and parse the Metadata file
    ** into the ODL tree.
    */
    if ((ReadLabel (fd, rootAggregate)) == 0)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "An error occured while parsing the ODL file '%s'.",
            fileSpec);
        (void) RemoveAggregate (rootAggregate);
        (void) fclose (fd);
        return (IMS_ERROR);
    }

    /*
    ** Populate the tree structure
    */

    *tree = NULL;

    if ((status = ims_aggregateToTree (msgDesc,
            NextAggregate(rootAggregate), tree, FALSE)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not build the IMS ODL Tree.");
        (void) RemoveAggregate (rootAggregate);
        (void) fclose (fd);
        return (status);
    }

    /*
    ** Free the entire ODL tree and close the file.
    */
    if ((RemoveAggregate (rootAggregate)) != (AGGREGATE) NULL)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
            "Could not free the ODL tree structure.");
    }
    (void) fclose (fd);

    return (IMS_OK);
}

/***************************************************************
**
** ims_aggregateToTree
**
*****************************************************************/

static  int ims_aggregateToTree (
    IMS_MSG_STRUCT *msgDesc,
    AGGREGATE root,
    IMS_ODL_TREE **root_tree,
    int child)
{
    PARAMETER currParameter;
    VALUE currValue;
    char stmt[IMS_COL255_LEN+1];
    IMS_ODL_TREE *new_tree;
    int column;
    int i;

    if (root == NULL)
        return(IMS_OK);


    (void) ims_addODLObject(msgDesc, *root_tree, &new_tree, root->name,
        child, IMS_OBJECT);

    if (*root_tree == NULL)
        *root_tree = new_tree;

    currParameter = FirstParameter (root);
    while (currParameter != NULL)
    {
        /*
        ** Create Keywords
        */

        currValue = FirstValue (currParameter);

        switch (currValue->item.type)
        {
            case TV_INTEGER:
                ODLFormatInteger(stmt, &(currValue->item));
                break;

            case TV_REAL:
                ODLFormatReal(stmt, &(currValue->item));
                break;

            case TV_DATE_TIME:
                ODLFormatDateTime(stmt, &(currValue->item));
                break;

            case TV_STRING:
                column = 0;
                ODLFormatString(stmt, &(currValue->item),
                    &column, 0, 132 , FALSE, TRUE);

                /*
                ** Get rid of quotes around string.
                */

                stmt[strlen(stmt) - 1] = '\0';
                for (i = 0; i < (int) strlen(stmt) -1; i++)
                {
                    stmt[i] = stmt[i+1];
                }
                stmt[i] = '\0';

                break;

            case TV_SYMBOL:
                ODLFormatSymbol(stmt, &(currValue->item));

                /*
                ** Get rid of quotes around string.
                */

                stmt[strlen(stmt) - 1] = '\0';
                for (i = 0; i < (int) strlen(stmt) -1; i++)
                {
                    stmt[i] = stmt[i+1];
                }
                stmt[i] = '\0';

                break;
        }

        (void) ims_addODLKeyword(msgDesc, new_tree, currParameter->name,
                currValue->item.type, stmt);

        currParameter = NextParameter (currParameter);
    }

    (void) ims_aggregateToTree (msgDesc, root->first_child, &new_tree,
        TRUE);
    (void) ims_aggregateToTree (msgDesc, root->right_sibling, root_tree,
        child);

    return(IMS_OK);

}




/***************************************************************
**
** ims_parseODLBuffer ()
**
** Parse an ODL buffer using the ODL ReadLabel_buf() function.
**
*****************************************************************/

int ims_parseODLBuffer (
    IMS_MSG_STRUCT *msgDesc,
    char *odlBuffer,
    char *targetObject,
    IMS_KEYWORD_LIST **keywordList)
{
    AGGREGATE rootAggregate;
    int status;

    /*
    ** Allocate the structure for the root node of the ODL tree.
    */
    if ((rootAggregate = NewAggregate (NULL, KA_GROUP, "root", NULL)) ==
        (AGGREGATE) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Unable to allocate the structure for the root node of "
            "the ODL tree.");
        return (IMS_FATAL);
    }

    /*
    ** Call the ODL function to read and parse the Metadata file
    ** into the ODL tree.
    */
    if ((ReadLabel_buf (odlBuffer, rootAggregate)) == 0)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "An error occured while parsing the ODL buffer.");
        (void) RemoveAggregate (rootAggregate);
        return (IMS_ERROR);
    }

    /*
    ** Populate the keyword list from the ODL tree.
    */
    if ((status = populateKeywordList (msgDesc, rootAggregate,
        targetObject, keywordList)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not populate the keyword list.");
        (void) RemoveAggregate (rootAggregate);
        return (status);
    }

    /*
    ** Free the entire ODL tree.
    */
    if ((RemoveAggregate (rootAggregate)) != (AGGREGATE) NULL)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
            "Could not free the ODL tree structure.");
    }

    return (IMS_OK);
}

/*******************************************************************
**
** ims_findListItem ()
**
** Find the given item in the keyword list and return a pointer
** to its associated information.
**
*********************************************************************/

int ims_findListItem (
    IMS_MSG_STRUCT *msgDesc,
    IMS_KEYWORD_LIST *keywordList,
    char *keyword,
    int itemType,
    int mandatoryFlag,
    IMS_KEYWORD_LIST **listPtr)
{

    *listPtr = (IMS_KEYWORD_LIST *) NULL;

    /*
    ** Try to find a match with the given item.
    */
    while (keywordList != (IMS_KEYWORD_LIST *) NULL)
    {
        if ((strcmp (keyword, keywordList->keyword) == 0) &&
            (keywordList->item_type == itemType))
        {
            *listPtr = keywordList;
            return (IMS_OK);
        }

        keywordList = keywordList->next;
    }

    /*
    ** We didn't find the item so check to see
    ** if its mandatory.
    */
    if (mandatoryFlag == IMS_TRUE)
    {
        if (itemType == IMS_KEYWORD)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Keyword '%s' is mandatory and not present.",
                keyword);
            return (IMS_ERROR);
        }
        else
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Object '%s' is mandatory and not present.",
                keyword);
            return (IMS_ERROR);
        }
    }

    return (IMS_OK);
}

/*******************************************************************
**
** ims_freeKeywordList ()
**
** Free the IMS_KEYWORD_LIST structure.
**
*********************************************************************/

void ims_freeKeywordList (
    IMS_KEYWORD_LIST *currPtr)
{
    IMS_KEYWORD_LIST *nextPtr;

    while (currPtr != (IMS_KEYWORD_LIST *) NULL)
    {
        nextPtr = currPtr->next;
        free (currPtr);
        currPtr = nextPtr;
    }

    return;
}

/*******************************************************************
**
** populateKeywordList ()
**
*********************************************************************/

static int populateKeywordList (
    IMS_MSG_STRUCT *msgDesc,
    AGGREGATE rootAggregate,
    char *targetObject,
    IMS_KEYWORD_LIST **keywordList)
{
    IMS_KEYWORD_LIST *currList;
    IMS_KEYWORD_LIST *prevList;
    AGGREGATE baseAggregate;
    AGGREGATE currAggregate;
    AGGREGATE subAggregate;
    PARAMETER currParameter;
    AGGREGATE aggregate;
    VALUE currValue;
    int listCount;
    int keywordCount;
    unsigned int milliseconds;
    char  str[129];
    long  i;


    /*
    ** Initialize variables.
    */
    baseAggregate = (AGGREGATE) NULL;
    listCount = 0;
    keywordCount = 0;

    /*
    ** Process the target aggregate, if given, otherwise the root is the
    ** target. This includes any sub-aggregates along with the
    ** associated parameters and values.
    */
    if (targetObject != (char *) NULL)
    {
        if ((baseAggregate = FindAggregate (rootAggregate,
            targetObject)) == (AGGREGATE) NULL)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The target object '%s' was not found in the ODL tree.",
                targetObject);
            return (IMS_ERROR);
        }

        currAggregate = baseAggregate;
    }
    else
    {
        currAggregate = rootAggregate;
    }

    prevList = (IMS_KEYWORD_LIST *) NULL;
    while (currAggregate != (AGGREGATE) NULL)
    {
        /*
        ** Add the OBJECT or GROUP name to the keyword list.
        */

        /*
        ** Allocate space for the IMS_KEYWORD_LIST structure.
        */
        if ((currList = (IMS_KEYWORD_LIST *) malloc
            ((size_t) sizeof (IMS_KEYWORD_LIST))) ==
            (IMS_KEYWORD_LIST *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Could not allocate memory for IMS_KEYWORD_LIST "
                "structure.");
            return (IMS_FATAL);
        }

        /*
        ** keywordList points to the first element in the list.
        */
        if (++listCount == 1)
        {
            *keywordList = currList;
        }
        else
        {
            prevList->next = currList;
        }

        /* Initialize structure members. */
        (void) strcpy (currList->keyword, currAggregate->name);
        currList->item_type = IMS_OBJECT;
        currList->next = (IMS_KEYWORD_LIST *) NULL;
        prevList = currList;

        /*
        ** Process the parameters for the current aggregate.
        */
        currParameter = FirstParameter (currAggregate);
        while (currParameter != (PARAMETER) NULL)
        {
            /*
            **  need to expand to multiple values: use the same
            **      left value, multiple entries.
            */
            currValue = FirstValue (currParameter);
            for( i=0 ; i  <  currParameter->value_count ; i++ ){

                /*
                ** Allocate space for the IMS_KEYWORD_LIST structure.
                */
                if ((currList = (IMS_KEYWORD_LIST *) malloc
                    ((size_t) sizeof (IMS_KEYWORD_LIST))) ==
                    (IMS_KEYWORD_LIST *) NULL)
                {
                    (void) ims_msg (msgDesc, IMS_FATAL,
                        "Could not allocate memory for IMS_KEYWORD_LIST"
                        " structure.");
                    return (IMS_FATAL);
                }

                /*
                ** keywordList points to the first element in the list.
                */
                if (++listCount == 1)
                {
                    *keywordList = currList;
                }
                else
                {
                    prevList->next = currList;
                }

                /* Initialize structure members. */
                (void) strcpy (currList->keyword, currParameter->name);
                currList->item_type = IMS_KEYWORD;
                currList->next = (IMS_KEYWORD_LIST *) NULL;
                prevList = currList;
                keywordCount++;

                /* Process the value for the current parameter. */

                /* See if value exceeds maximum length. */
                if (currValue->item.length > IMS_COL255_LEN)
                {
                    (void) ims_msg (msgDesc, IMS_ERROR,
                        "Value for keyword '%s' exceeded maximum "
                        "length of '%d'.",
                        currParameter->name, IMS_COL255_LEN);
                    return (IMS_ERROR);
                }

                /* Copy the value to the list based on data type. */
                switch (currValue->item.type)
                {
                case TV_INTEGER:
                    currList->value_integer =
                        currValue->item.value.integer.number;
                    currList->data_type = IMS_INT4_TYPE;
                    break;

                case TV_REAL:
                    currList->value_real =
                        currValue->item.value.real.number;
                    currList->data_type = IMS_FLOAT8_TYPE;
                    break;

                case TV_SYMBOL:
                    (void) strcpy (currList->value_string,
                        currValue->item.value.string);
                    currList->data_type = IMS_SYMBOL_TYPE;
                    break;

                case TV_STRING:
                    (void) strcpy (currList->value_string,
                        currValue->item.value.string);
                    currList->data_type = IMS_STRING_TYPE;
                    break;

                case TV_DATE:
                    (void) ims_msg (msgDesc, IMS_ERROR,
                        "The value for keyword '%s', has an "
                        "unsupported value type, 'DATE'. Please"
                        " use the 'DATETIME' value format.\n",
                        currParameter->name);
                    currList->data_type = IMS_NO_DATA_TYPE;
                    break;

                case TV_TIME:
                    (void) ims_msg (msgDesc, IMS_ERROR,
                       "The value for keyword '%s', has an unsupported"
                       " value type, 'TIME'. Please use the 'DATETIME' "
                        "value format.\n",
                        currParameter->name);
                    currList->data_type = IMS_NO_DATA_TYPE;
                    break;

                case TV_DATE_TIME:
                    milliseconds = (unsigned int) ims_nano2msecs(
                        currValue->item.value.date_time.nanoseconds);
                    (void) sprintf (currList->value_string,
                        "%04u-%03uT%02u:%02u:%02u.%03u",
                        currValue->item.value.date_time.year,
                        currValue->item.value.date_time.doy,
                        currValue->item.value.date_time.hours,
                        currValue->item.value.date_time.minutes,
                        currValue->item.value.date_time.seconds,
                        milliseconds);

                    currList->data_type = IMS_DATETIME_TYPE;

                    /*
                    ** Validate that the milliseconds are correct range.
                    */

                    if ((milliseconds < 0) ||
                        (milliseconds > 999))
                    {
                        (void) ims_msg (msgDesc, IMS_ERROR,
                           "The value for keyword '%s', has an invalid "
                            "millisecond value '%d'.",
                            currParameter->name, milliseconds);
                        return (IMS_ERROR);
                    }
                    break;

                default:
                    (void) ims_msg (msgDesc, IMS_ERROR,
                        "The value for keyword '%s', has an invalid "
                        "value type.", currParameter->name);
                    return (IMS_ERROR);
                }
                currValue = currValue->right_sibling;
            }

            currParameter = NextParameter (currParameter);
        }

        /*
        ** Find the next aggregate to process.
        */
        if (baseAggregate != (AGGREGATE) NULL)
        {
            /* Process all aggregates in the base aggregate. */
            currAggregate = NextSubAggregate (baseAggregate,
                currAggregate);
        }
        else
        {
            if(  currAggregate->first_child  ==  NULL  ){
                /*
                **  put in the end sections: at least one here
                */
                aggregate = currAggregate;
                /*
                ** Allocate space for the IMS_KEYWORD_LIST structure.
                */
                if ((currList = (IMS_KEYWORD_LIST *) malloc
                    ((size_t) sizeof (IMS_KEYWORD_LIST))) ==
                    (IMS_KEYWORD_LIST *) NULL){
                    (void) ims_msg (msgDesc, IMS_FATAL,
                       "Could not allocate memory for IMS_KEYWORD_LIST "
                        "structure.");
                    return (IMS_FATAL);
                }
                prevList->next = currList;

                /* Initialize structure members. */
                (void) strcpy( str, "END_" );
                (void) strcat (str, aggregate->name);

                (void) strcpy (currList->keyword, str );
                currList->item_type = IMS_KEYWORD;
                currList->next = (IMS_KEYWORD_LIST *) NULL;
                keywordCount++;
                prevList = currList;
                while( aggregate  !=  (AGGREGATE) NULL ){
                    if(  aggregate  ==  rootAggregate  ){
                        aggregate = NULL;
                        break;
                    }
                    else  if(  aggregate->right_sibling  !=  NULL  ){
                        aggregate = aggregate->right_sibling;
                        break;
                    }
                    aggregate = aggregate->parent;
                    /*
                    ** Allocate space for IMS_KEYWORD_LIST structure.
                    */
                    if ((currList = (IMS_KEYWORD_LIST *) malloc
                        ((size_t) sizeof (IMS_KEYWORD_LIST))) ==
                        (IMS_KEYWORD_LIST *) NULL){
                        (void) ims_msg (msgDesc, IMS_FATAL,
                            "Could not allocate memory for "
                            "IMS_KEYWORD_LIST structure.");
                        return (IMS_FATAL);
                    }
                    prevList->next = currList;

                    /* Initialize structure members. */
                    (void) strcpy( str, "END_" );
                    (void) strcat (str, aggregate->name);

                    (void) strcpy (currList->keyword, str );
                    currList->item_type = IMS_KEYWORD;
                    currList->next = (IMS_KEYWORD_LIST *) NULL;
                    keywordCount++;
                    prevList = currList;
                }
            }

            /* Process all aggregates in the root aggregate. */
            if ((subAggregate = NextSubAggregate (rootAggregate,
                currAggregate)) == (AGGREGATE) NULL)
            {
                currAggregate = NextAggregate (currAggregate);
            }
            else
            {
                currAggregate = subAggregate;
            }
        }
    }

    /*
    ** Check the keyword count.
    */
    if (keywordCount <= 0)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
            "No keywords were found in the ODL tree.");
        return (IMS_WARNING);
    }
    else
    {
        return (IMS_OK);
    }
}
