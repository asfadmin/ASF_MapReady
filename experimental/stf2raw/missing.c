/*************************
Missing.c: Data types, utilities to describe the missing
(dropped) frames and lines of a data take.

Implements all routines listed in "missing.h"
*/
#include "asf.h"
#include "decoder.h"
#include "aux.h"


/*************** A small linked list class, which is used below********/
typedef struct {
	void *next;/*Pointer to next entry in linked list, or NULL*/
	int frameNo;/*First frame number this entry applies to.*/
} linked_list_entry;

/*Add the given entry to the tail of the given linked list.
frameNo should already be filled out.*/
void link_addToEnd(linked_list_entry **list,linked_list_entry *addThis)
{
/*Run to end of list*/
	linked_list_entry *cur=(*list),*prev=NULL;
	while (cur!=NULL)
	{
		prev=cur;
		cur=(linked_list_entry *)prev->next;
	}
/*Add this entry to the tail of list*/
	if (prev==NULL)
		*list=addThis;/*List was empty-- add new first element*/
	else
		prev->next=(void *)addThis;/*Link element into end of list*/
/*Set new element's next to NULL*/
	addThis->next=NULL;
}

/*Extract the first linked_list entry whose sucessor's frame number
is greater than frameNo. (otherwise the entry with no sucessor) */
linked_list_entry *link_findBefore(linked_list_entry *list,int frameNo)
{
	linked_list_entry *cur=list,*prev=NULL;
	while (cur!=NULL)
	{
		if (cur->frameNo>frameNo)
			return prev;
		prev=cur;
		cur=(linked_list_entry *)cur->next;
	}
	return prev;
}

/*Extract the linked_list entry for the given frame number, or return NULL if none exists*/
linked_list_entry *link_find(linked_list_entry *list,int frameNo)
{
	linked_list_entry *cur=list;
	while (cur!=NULL)
	{
		if (cur->frameNo==frameNo)
			return cur;
		cur=(linked_list_entry *)cur->next;
	}
	return NULL;
}

/*FREE() all entries in the given linked list (if any).*/
void list_free(linked_list_entry *list)
{
	linked_list_entry *cur=list;
	while (cur!=NULL)
	{
		linked_list_entry *next=(linked_list_entry *)cur->next;
		cur->next=NULL;
		cur->frameNo=-1;
		FREE(cur);
		cur=next;
	}
}



/******************* Structures used to keep track of missing frames/lines
and data window position for each frame in the file ***********************/

/*Here's some pseudo object-oriented design: dwp_entry and missing_entry
are linked_list entries, but they drag around some extra info (like derived
classes).  Since the linked_list code treats them only as MALLOC'd
pointers, the extra info stays with the structure, but keeps out of
linked_list's way.
*/
typedef struct {
	linked_list_entry link;/*inherited linked-list state*/
	int DWP_code;/*Data window position code*/
} DWP_entry;

typedef struct {
	linked_list_entry link;/*inherited linked-list state*/
	missing_data missing;/*# of missing frames and lines*/
} missing_entry;


#define framesPerGroup 1024
#define framesPerGroupShift 10 /*Base-2 log of framesPerGroup*/

/*I maintain just one frame_group for every group of frames.
This keeps the size of the frame_group array small.  Note that 
the value of framesPerGroup is chosen big enough that we don't 
need too many of them, but small enough that the list of missing
lines, etc. stays small (& quick to search).  Normally, of course,
DWP_list and missing_list are empty.
*/
typedef struct {
	int DWP_code; /*Data window position code for this group.*/
	DWP_entry *DWP_list;/*Linked list of dwp_entries.  Supercedes above if non-NULL*/
	missing_entry *missing_list;/*Linked list of missing data entries.*/
} frame_group;

typedef struct {
	frame_group *groups;/*Array of fileSize/bytesPerFrame/framesPerGroup--
				one structure for every framesPerGroup frames in the file*/
	int nGroups;/*Number of groups in above array*/
} missing_state;

/************************* read data **********************
Find the number of missing (dropped) frames between frame
number 1 and frame number 2 in the given file.*/
void findMissing(bin_state *s,int frame1,int frame2,missing_data *missing)
{
	int frameNo;
	missing->lines=missing->frames=0;/*Start sum at zero*/
	for (frameNo=frame1+1;frameNo<=frame2;frameNo++)
	{
		missing_data temp;
		find1Missing(s,frameNo,&temp);
		missing->lines+=temp.lines;
		missing->frames+=temp.frames;
	}
}

/*Find the number of missing (dropped) frames 
just before the given frame in the given file.*/
void find1Missing(bin_state *s,int frame,missing_data *missing)
{
	missing_state *m=(missing_state *)s->missing;
	if (m!=NULL)
	{
		int groupNo=frame>>framesPerGroupShift;
		frame_group *g=&m->groups[groupNo];
		linked_list_entry *link;
		if (NULL!=(link=link_find((linked_list_entry *)g->missing_list,frame)))
		{
			missing_entry *mlink=(missing_entry *)link;
			missing->lines=mlink->missing.lines;
			missing->frames=mlink->missing.frames;
			return;
		}
	}
	/*No missing data-- never mind*/
	missing->lines=missing->frames=0;
}

/*Return the expected data window position code at the given frame*/
int expectedDWP_code(bin_state *s,int frame)
{
	missing_state *m=(missing_state *)s->missing;
	if (m!=NULL)
	{
		int groupNo=frame>>framesPerGroupShift;
		frame_group *g=&m->groups[groupNo];
		linked_list_entry *link;
		if (NULL!=(link=link_findBefore((linked_list_entry *)g->DWP_list,frame)))
		{
			DWP_entry *dlink=(DWP_entry *)link;
			return dlink->DWP_code;
		} else/*No missing data-- never mind*/
			return m->groups[groupNo].DWP_code;
	}
	return -100;/*Since there's no missing_state structure, return a bogus value*/
}


/********************** write (initialize) data **********************
Create & initialize the missing data/DWP_code array for the 
given bin_state structure (which must already have its binary file open).
*/
void createMissing(bin_state *s)
{
	missing_state *m=(missing_state *)MALLOC(sizeof(missing_state));
	int i;
	m->nGroups=(int)ceil((double)s->bytesInFile/s->bytesPerFrame/framesPerGroupShift);
	m->groups=(frame_group *)MALLOC(m->nGroups*sizeof(frame_group));
	for (i=0;i<m->nGroups;i++)
	{
		m->groups[i].DWP_list=NULL;
		m->groups[i].missing_list=NULL;
	}
	s->missing=(void *)m;
}
void freeMissing(bin_state *s)
{
	missing_state *m=(missing_state *)s->missing;
	int i;
	for (i=0;i<m->nGroups;i++)
	{
		list_free((linked_list_entry *)m->groups[i].DWP_list);	
		list_free((linked_list_entry *)m->groups[i].missing_list);
	}
	FREE(m->groups);
	FREE(s->missing);
	s->missing=NULL;
}

/*Add the given missing frames immediately before the given frame.
Missing data must be added in frame order.*/
void addMissing(bin_state *s,int frame,const missing_data *missing)
{
	missing_state *m=(missing_state *)s->missing;
	int groupNo=frame>>framesPerGroupShift;
	if ((groupNo<0)||(groupNo<m->nGroups)) /*Skip out-of-bounds frames*/
	{
		frame_group *g=&m->groups[groupNo];
		missing_entry *entry=(missing_entry *)MALLOC(sizeof(missing_entry));
		entry->link.frameNo=frame;
		entry->missing=*missing;
		link_addToEnd((linked_list_entry **)&g->missing_list,(linked_list_entry *)entry);
	}
}

/*Add the given data window code starting at the given frame, 
and continuing onward.*/
void addDWP_code(bin_state *s,int frame,int DWP_code)
{
	missing_state *m=(missing_state *)s->missing;
	int i;
	int groupNo=frame>>framesPerGroupShift;
	if ((groupNo<0)||(groupNo<m->nGroups)) /*Skip out-of-bounds frames*/
	{
		frame_group *g=&m->groups[groupNo];
		DWP_entry *entry=(DWP_entry *)MALLOC(sizeof(DWP_entry));
		entry->link.frameNo=frame;
		entry->DWP_code=DWP_code;
	/*Set the referenced frame's DWP using the DWP_list*/
		link_addToEnd((linked_list_entry **)&g->DWP_list,(linked_list_entry *)entry);
	/*Set the DWP of all subsequent frames using frame_group's DWP_code*/
		for (i=groupNo+1;i<m->nGroups;i++)
			m->groups[i].DWP_code=DWP_code;
	}
}







