/*=============================================================================
 |  @(#)Q.c	1.4 98/02/10 10:43:46
 |
 |  Circular Queue Object.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include <stdio.h>
#include <errno.h>
#include "Q.h"

static const char sccsid_Q_c[] =
        "@(#)Q.c	1.4 98/02/10 10:43:46";

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
void Q_Lock(Q_t* q)
{
    pthread_mutex_lock(&q->Mutex);
}

void Q_Unlock(Q_t* q)
{
    pthread_mutex_unlock(&q->Mutex);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
void* Q_Insert(Q_t* q, void* e)
{
    off_t i, k, j;
    pthread_mutex_lock(&q->Mutex);

    while (!q->quit && (i = (q->tail + 1) % q->size) == q->head) {
#ifdef	DEBUG
	printf("%s: FULL\n", q->name);
#endif
	pthread_cond_wait(&q->NotFullCond, &q->Mutex);
    }
#ifdef	DEBUG
    if (!strcmp(q->name, "merge_Q"))
    printf("%s: INS head %d, tail %d\n", q->name, q->head, q->tail);
#endif
    if (q->quit) { 
	pthread_cond_broadcast(&q->NotEmptyCond);
	pthread_mutex_unlock(&q->Mutex);
	return NULL;
    }
    if (! q->cmp) {
	q->base[q->tail = i] = *(void**) e;
	pthread_cond_signal(&q->NotEmptyCond);
    }
    else {
	for (i = q->tail;
	     i != q->head && (*q->cmp)(*(void**)e, q->base[i]) < 0;
	     i = (i-1) % q->size)
	     q->base[(i+1) % q->size] = q->base[i];

	q->base[(i+1) % q->size] = *(void**) e;
	q->tail = (q->tail + 1) % q->size;
#ifdef	DEBUG
    if (!strcmp(q->name, "merge_Q")) {
    printf("Merge_Q\n");
    i = q->head;
    while (i != q->tail) {
	i = (i+1) % q->size;
	Segmt_Dump(q->base[i]);
    }
    printf("\n");
    }
#endif
	pthread_cond_signal(&q->NotEmptyCond);
    }
    if (q->insert)
	return (*q->insert)(q, *(void**) e, q->insertData);

    pthread_mutex_unlock(&q->Mutex);
    return *(void**) e;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
void* Q_Tail (Q_t* q, void* e)
{
    pthread_mutex_lock(&q->Mutex);
    *(void**) e = (q->head == q->tail) ? NULL : q->base[(q->tail+1) % q->size];
    pthread_mutex_unlock(&q->Mutex);
    return *(void**) e;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
void* Q_Remove(Q_t* q, void* e)
{
    off_t i;
    pthread_mutex_lock(&q->Mutex);
L:
    while (q->head == q->tail && !q->quit) {
#ifdef	DEBUG
	printf("%s: EMPTY\n", q->name);
#endif
	pthread_cond_wait(&q->NotEmptyCond, &q->Mutex);
    }
#ifdef	DEBUG
    if (!strcmp(q->name, "merge_Q"))
    printf("%s: REM head %d, tail %d\n", q->name, q->head, q->tail);
#endif
    if (q->head == q->tail) {
	pthread_cond_broadcast(&q->NotFullCond);
	pthread_mutex_unlock(&q->Mutex);
	return NULL;
    }
    else {
	*(void**) e = q->base[i = (q->head + 1) % q->size];
	if (q->rem && !(*q->rem)(q, *(void**) e, q->removeData)) {
	    pthread_cond_wait(&q->NotEmptyCond, &q->Mutex);
	    goto L;
	}
	q->head = i;
#ifdef	DEBUG
    if (!strcmp(q->name, "merge_Q"))
    printf("%s: REM AFTER head %d, tail %d\n", q->name, q->head, q->tail);
#endif
	pthread_cond_signal(&q->NotFullCond);
    }
    if (q->remove)
	return (*q->remove)(q, *(void**) e, q->removeData);

    pthread_mutex_unlock(&q->Mutex);
    return (*(void**) e);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
void* Q_Peek(Q_t* q, void* e)
{
    pthread_mutex_lock(&q->Mutex);
    *(void**) e = (q->head == q->tail) ? NULL : q->base[(q->head+1) % q->size];
#ifdef	DEBUG
    if (!strcmp(q->name, "merge_Q"))
    printf("%s: PEEK %d, head %d, tail %d\n", q->name, 
	   (q->head+1) % q->size, q->head, q->tail);
#endif
    pthread_mutex_unlock(&q->Mutex);
    return *(void**) e;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
Q_t* Q_Init(Q_t* q, void* base, size_t size, int (*cmp)(),
	    void* (*insert)(), void* insert_data,
	    void* (*remove)(), void* remove_data, int (*remove_test)(),
	    char* name)
{
    q->quit = 0;
    q->head = q->tail = 0;
    q->base = base;
    q->size = size;
    q->cmp  = (int (*)(void*, void*)) cmp;
    q->rem  = (int (*)(Q_t*, void*, void*)) remove_test;
    q->insert = (void* (*)(Q_t*, void*, void*)) insert;
    q->insertData = insert_data;
    q->remove = (void* (*)(Q_t*, void*, void*)) remove;
    q->removeData = remove_data;
    q->name = name;
    
    if (pthread_mutex_init(&q->Mutex, pthread_mutexattr_default)
	== -1) {
	return NULL;
    }
    if (pthread_cond_init(&q->NotFullCond, pthread_condattr_default)
	== -1) {
	pthread_mutex_destroy(&q->Mutex);
	return NULL;
    }
    if (pthread_cond_init(&q->NotEmptyCond, pthread_condattr_default)
	== -1) {
	pthread_cond_destroy (&q->NotFullCond);
	pthread_mutex_destroy(&q->Mutex);
	return NULL;
    }
    return q;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
void Q_Reset(Q_t* q)
{
    q->head = q->tail = q->quit = 0;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
void Q_Shutdown(Q_t* q)
{
    pthread_mutex_lock(&q->Mutex);
#ifdef	DEBUG
	printf("%s: SHUTDOWN\n", q->name);
#endif
    q->quit = 1;
    pthread_cond_broadcast(&q->NotFullCond);
    pthread_cond_broadcast(&q->NotEmptyCond);
    pthread_mutex_unlock(&q->Mutex);
}
/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
void Q_Destroy(Q_t* q)
{
    Q_Shutdown(q);
    pthread_cond_destroy(&q->NotEmptyCond);
    pthread_cond_destroy(&q->NotFullCond);
    pthread_mutex_destroy(&q->Mutex);
}
