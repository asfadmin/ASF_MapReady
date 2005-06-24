/* Implementation of the interface in vector.h.  */

#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include <glib.h>

#include "vector_wmc.h"

// Queue of GMemChunk thingies (to be used as a stack actually).
static GQueue *memchunk_queue = NULL;
// Size of each memory chunk, in Vector instances.
static const size_t chunk_size = VECTOR_INSTANCE_CACHE_SIZE;
// For efficiency, we maintain a pointer to the top of the stack.
static GMemChunk *current_chunk = NULL;
// If we happen to be allocating and freeing right around a multiple
// of the chunk size, we don't want to end up repeatedly allocating
// and freeing whole chunks.  So we have the notion of a reserve
// chunk, which doesn't get released until we have another reserve
// chunk ready (because lots of freeing has happened).
static GMemChunk *reserve_chunk = NULL;
// Number of instances saved in current chunk.
static size_t current_chunk_usage = 0;

static void
push_chunk (void)
{
  if ( reserve_chunk != NULL ) {
    current_chunk = reserve_chunk;
    reserve_chunk = NULL;
  }
  else {
    current_chunk = g_mem_chunk_create (Vector, chunk_size, G_ALLOC_AND_FREE);
  }
  current_chunk_usage = 0;
  g_queue_push_head (memchunk_queue, current_chunk);
}

static void
pop_chunk (void)
{
  // We don't ever want to free the last chunk.
  g_assert (g_queue_get_length (memchunk_queue) >= 2);
  if ( reserve_chunk != NULL ) {
    g_mem_chunk_destroy (reserve_chunk);
  }
  reserve_chunk = g_queue_pop_head (memchunk_queue);
  current_chunk = g_queue_peek_head (memchunk_queue);
}

static Vector *
vector_new_uninitialized (void)
{
  if ( G_UNLIKELY (memchunk_queue == NULL) ) {
    memchunk_queue = g_queue_new ();
    push_chunk ();
  }
  else if ( G_UNLIKELY (current_chunk_usage == chunk_size) ) {
    push_chunk ();
  }

  current_chunk_usage++;
  return g_chunk_new (Vector, current_chunk);
}

Vector *
vector_new (double x, double y, double z)
{
  Vector *self = vector_new_uninitialized ();

  self->x = x;
  self->y = y;
  self->z = z;

  return self;
}

Vector *
vector_copy (Vector *a)
{
  Vector *self = vector_new_uninitialized ();

  self->x = a->x;
  self->y = a->y;
  self->z = a->z;

  return self;
}

Vector *
vector_cross (Vector *a, Vector *b)
{
  Vector *self = vector_new_uninitialized ();
  
  self->x = a->y * b->z - a->z * b->y;
  self->y = a->z * b->x - a->x * b->z;
  self->z = a->x * b->y - a->y * b->x;

  return self;
}

void
vector_set (Vector *self, double x, double y, double z)
{
  self->x = x;
  self->y = y;
  self->z = z;
}

double
vector_dot (Vector *self, Vector *other)
{
  return self->x * other->x + self->y * other->y + self->z * other->z;
}

void
vector_add (Vector *self, Vector *other)
{
  self->x += other->x;
  self->y += other->y;
  self->z += other->z;
}

void
vector_subtract (Vector *self, Vector *other)
{
  self->x -= other->x;
  self->y -= other->y;
  self->z -= other->z;
}

void
vector_multiply (Vector *self, double factor)
{
  self->x *= factor;
  self->y *= factor;
  self->z *= factor;
}

double
vector_magnitude (Vector *self)
{
  return sqrtl (powl (self->x, 2) + powl (self->y, 2) + powl (self->z, 2));
}

double
vector_angle (Vector *self, Vector *other)
{
  assert (vector_magnitude (self) > 0.0);
  assert (vector_magnitude (other) > 0.0);

  return acos (vector_dot (self, other) / (vector_magnitude (self) 
					   * vector_magnitude (other)));
}

void
vector_free (Vector *self)
{
  g_assert (current_chunk_usage > 0);

  g_chunk_free (self, current_chunk);  

  current_chunk_usage--;

  if ( current_chunk_usage == 0 ) {
    if ( g_queue_get_length (memchunk_queue) >= 2 ) {
      pop_chunk ();
    }
  }
}
