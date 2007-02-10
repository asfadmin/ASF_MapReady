// Test code to exercise the table_file class.  The exercise may not
// be totally complete but it should help.
//
// The basic strategy is to just have a few different threads perform
// the following operations:
//
//   1. Add field.
//   2. Remove field.
//   3. Reset field value.
//   4. Get field value.
//   5. Add, hold lock, reset value.
//
// We do this once with the operation being performed at top speed,
// with no pauses anywhere.  The idea is that this tests the
// correctness of the synchronization mechanisms under fast changing
// conditions, where problems relating to simultaneous calls and race
// conditions are most likely to show up.  We then do it again with
// the calls coming at random intervals, and with the operations
// themselves using random sleeps with the data deliberately set to a
// detectable "corrupt" state while the associated required locks are
// held.  This tests for problems or lockups that might occur when
// callers hold locks for a long time.
//
///////////////////////////////////////////////////////////////////////////////

#include <errno.h>
#include <math.h>
#include <mqueue.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "graph.h"
#include "table_file.h"
#include "utilities.h"

#ifdef TEST_TABLE_FILE_LONG_TEST_RUN
#  define TEST_TABLE_FILE_APPROXIMATE_DURATION 900.0
#else
#  define TEST_TABLE_FILE_APPROXIMATE_DURATION 5.0
#endif

// The path name to the test table file we will create.
#define TEST_TABLE_FILE_NAME "/tmp/test_table_file"

// This pointer gets filled in at the start of main to the actual
// instance we will use in the tests.
static TableFile *tf;

// The maximum number of fields we will allow our test table to have.
static const int field_count = 20;

// Names of field keys to use. 
static GPtrArray *keys = NULL;

// Total process_count (single and multithreaded).
static const unsigned int total_process_count = 15;

// Number of simultaneous single-threaded processes to run.
static const unsigned int single_thread_process_count = 5;

// Number of simultaneous multithreaded processes to run.
// FIXME: multithread functionality of TableFile isn't working yet.
static const unsigned int multithreaded_process_count = 5;

// Threads per multithreaded process.
static const unsigned int thread_count = 5;

// Time for each thread (including singlethreaded process threads) to
// run.
static const gdouble thread_test_run_time
  = TEST_TABLE_FILE_APPROXIMATE_DURATION;

// We use a named semaphore to help coordinate the assigment of reply
// message queues to the test threads.  Semaphore names are system
// wide so we try to pick a unique one (but we can't use PIDs or
// anything since these guys persist).
const char *queue_list_semaphore_name 
  = "/test_table_file_queue_list_semaphore";

///////////////////////////////////////////////////////////////////////////////
//
// Concurrent Routines
//
// To test multithreaded code, at some point we have to do some
// concurrent stuff.  But we can't do anything that would result in
// deadlock.  So we have a process dedicated to maintaining a graph
// and decided if a prospective operation would cause deadlock.  The
// test threads communicate with it using POSIX message queues.
//
///////////////////////////////////////////////////////////////////////////////

// Threads return pointers, so here we have som actual values for the
// return pointers to point to.
static const guint thread_success = 0, thread_failure = 1;

// Lock operations.
typedef enum {
  ATTEMPT_READER_LOCK,
  ACHIEVE_READER_LOCK,
  ATTEMPT_WRITER_LOCK,
  ACHIEVE_WRITER_LOCK,
  ACHIEVE_READER_UNLOCK,
  ACHIEVE_WRITER_UNLOCK
} request_type_t;

// The threads identify themselves to the deadlock detection process
// with IDs formed with a string of the form "process n thread m"
// where n and m are the PID and GThread address, respectively.
#define MAX_REQUESTOR_ID_LENGTH 200
// The possible locks also have to have IDs by which the deadlock
// detector knows them.
#define MAX_LOCK_ID_LENGTH 200

// A query from a worker to the deadlock detector.  This message might
// reflect a shutdown request.  Otherwise, it reflects something a
// thread would like to do to a lock.  If the operation is ok (won't
// deadlock), the detector will change its graph and messages back to
// the attemptor to proceed.  The lock_id is normally just the field
// name of the field we are interested in locking.  We have one
// special values that lock_id can take on: "ENTIRE_TABLE_LOCK", which
// means that this is the lock for te entire table.  As far as the
// deadlock detector is concerned, this is just another lock, though.
typedef struct {
  gboolean shutdown_request;
  char requestor_id[MAX_REQUESTOR_ID_LENGTH];
  mqd_t response_queue;
  request_type_t request_type;
  char lock_id[MAX_LOCK_ID_LENGTH];
} op_query_t;

// A response from the deadlock detector to a worker.
typedef struct {
  gboolean op_ok;
} op_query_response_t;

// The queue that clients use to query the deadlock detector, and an
// associated semaphore to protect the sender end of the queue.
static mqd_t request_queue = (mqd_t) -1;
static const char *request_queue_semaphore_name
  = "/test_table_file_request_queue_semaphore";
static sem_t *request_queue_semaphore = NULL;

// List of pointers to the mqd_t objects that the deadlock detector
// uses to answer queries.  Each thread will grab one of these when it
// starts, so we use a named semaphore to protect the list.
static sem_t *queue_list_semaphore = NULL;
static GPtrArray *response_queues = NULL;
// This will get assigned to point in to shared memory from which the
// threads can read the lowest numbered unclaimed response queue
// available.  Note that an alternative to this whole approach would
// be to just have the threads create the queues themselves, and
// include their names in their requests.
static int *queue_number = NULL;

// We cheat a bit and keep some meaningful values in the pointers in
// the contents entries of the graph edges.
#define READER_EDGE (NULL)
#define WRITER_EDGE ((gpointer) !NULL)

// Is the lock with lock_id currently writer_locked?  IMPROVEME: this
// is inefficient, we ought to maintain a seperate map from locks to
// their holders so we don't have to search the lists all the time.
static gboolean
is_writer_locked (GString *id, GHashTable *held_writer_locks)
{
  GPtrArray *keys, *values;
  my_g_hash_table_keys_and_values (held_writer_locks, &keys, &values);

  guint ii;
  for ( ii = 0 ; ii < values->len ; ii++ ) {
   // Current lock list.
    GPtrArray *cll = g_ptr_array_index (values, ii);
    guint jj;
    for ( jj = 0 ; jj < cll->len ; jj++ ) {
      // Current lock.
      GString *cl = g_ptr_array_index (cll, jj);

      if ( g_string_equal (id, cl) ) {
	my_g_ptr_array_really_free (values, NULL);
	my_g_ptr_array_really_free (keys, NULL);
	return TRUE;
      }
    }
  }

  my_g_ptr_array_really_free (values, NULL);
  my_g_ptr_array_really_free (keys, NULL);

  return FALSE;
}

// Form a new GString version of request_type.
static GString *
request_type_as_gstring (request_type_t request_type)
{
  GString *result = g_string_new ("");

  switch ( request_type ) {
  case ATTEMPT_READER_LOCK:
    g_string_append (result, "attempt_reader");
    break;
  case ATTEMPT_WRITER_LOCK:
    g_string_append (result, "attempt_writer");
    break;
  case ACHIEVE_READER_LOCK:
    g_string_append (result, "achieve_reader");
    break;
  case ACHIEVE_WRITER_LOCK:
    g_string_append (result, "achieve_writer");
    break;
  case ACHIEVE_WRITER_UNLOCK:
    g_string_append (result, "achieve_writer_unlock");
    break;
  case ACHIEVE_READER_UNLOCK:
    g_string_append (result, "achieve_reader_unlock");
    break;
  default:
    g_assert_not_reached ();
    break;
  }
  
  return result;
}

static void
check_invariants (Graph *pending_graph, Graph *block_graph,
		  GHashTable *pending_nodes, GHashTable *held_writer_locks,
		  GHashTable *node_names, request_type_t op_type,
		  GString *target_name)
{
  // For each node in the block graph that has incoming edges, all
  // incoming edges directed into its twin in the pending graph should
  // also be present in the block graph.
  guint ii;
  for ( ii = 0 ; ii < pending_graph->nodes->len ; ii++ ) {
    GraphNode *cpn = g_ptr_array_index (pending_graph->nodes, ii);
    GraphNode *cbn = cpn->contents;
    if ( cbn->in_edges->len > 0 ) {
      guint jj;
      for ( jj = 0 ; jj < cpn->in_edges->len ; jj++ ) {
	GraphEdge *cpe = g_ptr_array_index (cpn->in_edges, jj);
	g_assert (cpe->contents != NULL);
      }
    }
  }

  // Each node in the block graph which is write locked (meaning a
  // write lock exists or is sought after in the table itself) should
  // have a twin in the pending graph the incoming edges of which all
  // also appear in the block graph.
  GPtrArray *keys, *values;
  my_g_hash_table_keys_and_values (held_writer_locks, &keys, &values);
  for ( ii = 0 ; ii < keys->len ; ii++ ) {
    // Current lock list.
    GPtrArray *cll = g_ptr_array_index (values, ii);
    guint jj;
    for ( jj = 0 ; jj < cll->len ; jj++ ) {
      // Current lock.
      GString *cl = g_ptr_array_index (cll, jj);
      GraphNode *pgn = g_hash_table_lookup (pending_nodes, cl);
      GraphNode *bgn = pgn->contents;
      guint kk;
      // Temporary list of block graph incoming edges, formed from the
      // incoming edges in the pending graph.  When fully formed, this
      // list should contain the same edges as the bgn->in_edges list.
      GPtrArray *bgiel = g_ptr_array_new ();
      for ( kk = 0 ; kk < pgn->in_edges->len ; kk++ ) {
	GraphEdge *ce = g_ptr_array_index (pgn->in_edges, kk);
	if ( ce->contents == NULL ) {
	  GString *op_type_gs = request_type_as_gstring (op_type);
	  g_error ("Lock %s is write locked after %s operation on target %s, "
		   "but an edge into its pending graph entry has NULL "
		   "contents\n", cl->str, op_type_gs->str, target_name->str);
	  my_g_string_free (op_type_gs);
	}
	g_assert (ce->contents != NULL);
	g_ptr_array_add (bgiel, ce->contents);
      }
      if ( !my_g_ptr_array_equals_as_multiset (bgiel, bgn->in_edges,
					       g_direct_equal) ) {
	g_error ("Problem with incoming edges sets for lock %s\n", cl->str);
      }
      g_assert (my_g_ptr_array_equals_as_multiset (bgiel, bgn->in_edges,
						   g_direct_equal));
      my_g_ptr_array_really_free (bgiel, NULL);
    }
  }  
  my_g_ptr_array_really_free (values, NULL);
  my_g_ptr_array_really_free (keys, NULL);
}

static gboolean
try_op (GHashTable *held_reader_locks, GHashTable *held_writer_locks,
	GHashTable *pending_nodes, GHashTable *block_nodes,
	Graph *pending_graph, Graph *block_graph, op_query_t *query)
{
  static FILE *try_op_log_file = NULL;
  if ( try_op_log_file == NULL ) {
    try_op_log_file = fopen ("try_op_log_file.txt", "w");
    g_assert (try_op_log_file != NULL);
  }

  // GString equivalents for the the requestor and lock IDs.
  GString *requestor = g_string_new (query->requestor_id);
  GString *lock = g_string_new (query->lock_id);

  // For dumping file names and such we use a space free version of the id.
  GString *requestor_space_free = g_string_new ("");
  guint ii;
  for ( ii = 0 ; ii < requestor->len ; ii++ ) {
    gchar cc = (requestor->str)[ii];
    if ( cc == ' ' ) {
      g_string_append_c (requestor_space_free, '_');
    }
    else {
      g_string_append_c (requestor_space_free, cc);
    }
  }

  // The lists of reader and writer locks already held by the requestor.
  GPtrArray *requestor_rlocks
    = g_hash_table_lookup (held_reader_locks, requestor);  
  GPtrArray *requestor_wlocks
    = g_hash_table_lookup (held_writer_locks, requestor);

  if ( requestor_rlocks == NULL ) {
    g_assert (requestor_wlocks == NULL);
    requestor_rlocks = g_ptr_array_new ();
    g_hash_table_insert (held_reader_locks, g_string_new (requestor->str),
			 requestor_rlocks);
    requestor_wlocks = g_ptr_array_new ();
    g_hash_table_insert (held_writer_locks, g_string_new (requestor->str),
			 requestor_wlocks);
  }

  GPtrArray *requestor_locks = my_g_ptr_array_sum (requestor_rlocks,
						   requestor_wlocks);

  gboolean result;

  GraphNode *pending_target_node = g_hash_table_lookup (pending_nodes, lock);
  GraphNode *block_target_node = g_hash_table_lookup (block_nodes, lock);
  
  // If the lock isn't in the graphs and lookup tables yet, add it to
  // both.
  if ( pending_target_node == NULL ) {
    g_assert (block_target_node == NULL);
    block_target_node = graph_add_node (block_graph, NULL);
    g_hash_table_insert (block_nodes, g_string_new (lock->str),
			 block_target_node);
    // We keep a pointer to the twin of the pending node in the
    // block graph in the pending node contents, to help with the
    // process of promoting links from the pending graph to the
    // block graph.  Sing ho! for weird data structures.
    pending_target_node = graph_add_node (pending_graph, block_target_node);
    g_hash_table_insert (pending_nodes, g_string_new (lock->str),
			 pending_target_node);
  }

  // Form a reverse map in which we can look up the names for nodes
  // from their addresses.
  GHashTable *node_names = g_hash_table_new (g_direct_hash, g_direct_equal);
  GPtrArray *pkeys, *pvalues, *bkeys, *bvalues;
  my_g_hash_table_keys_and_values (pending_nodes, &pkeys, &pvalues);
  g_assert (pkeys->len == pvalues->len);
  my_g_hash_table_keys_and_values (block_nodes, &bkeys, &bvalues);
  g_assert (bkeys->len == bvalues->len);
  for ( ii = 0 ; ii < pkeys->len ; ii++ ) {
    GraphNode *cn = g_ptr_array_index (pvalues, ii);
    GString *cnn = g_ptr_array_index (pkeys, ii);
    g_hash_table_insert (node_names, cn, cnn);
  }
  for ( ii = 0 ; ii < bkeys->len ; ii++ ) {
    GraphNode *cn = g_ptr_array_index (bvalues, ii);
    GString *cnn = g_ptr_array_index (bkeys, ii);
    g_hash_table_insert (node_names, cn, cnn);
  }

  g_assert (g_hash_table_size (pending_nodes) + g_hash_table_size (block_nodes)
	    == g_hash_table_size (node_names));

  if ( query->request_type == ATTEMPT_READER_LOCK ) {
    
    if ( requestor_locks->len == 0 ) {
      // Note that we have to add the lock to the list of locks held
      // by the requestor now, rather than when the lock is actually
      // achieved.  The reason is that we must always be conservative
      // in the deadlock_preventer about which operation attempts we
      // approve.  From the moment we approve a lock attempt, we must
      // assume that that lock may have occured, even if an ACHIEVE
      // message hasn't yet been sent to the deadlock_preventer.
      // Therefore, from the moment the attempt is cleared to proceed,
      // we consider that it has been achieved as far as the lock list
      // is concerned.  Similarly, a conservative construction of the
      // dependency graphs requires that links be added before any
      // wait condition may exist, and not be removed until the
      // possibility of a wait is entirely eliminated.
      g_ptr_array_add (requestor_rlocks, g_string_new (lock->str));
      result = TRUE;
    }

    else {
      // Prospective edges for the pending and block graphs.
      GPtrArray *prospective_pedges = g_ptr_array_new ();
      guint ii;
      for ( ii = 0 ; ii < requestor_locks->len ; ii++ ) {
	GString *lock_id = g_ptr_array_index (requestor_locks, ii);
	if ( g_string_equal (lock_id, lock) ) {
	  g_error ("erroneous relock attempt by %s of %s", requestor->str,
		   lock_id->str);
	}
	g_assert (!g_string_equal (lock_id, lock));
	GraphNode *plock_node = g_hash_table_lookup (pending_nodes, lock_id);
	g_assert (plock_node != NULL);
	GraphEdge *prospective_edge
	  = graph_add_edge (pending_graph, plock_node, pending_target_node,
			    NULL);
	g_ptr_array_add (prospective_pedges, prospective_edge);
  
	// Flag to set true iff the target node is or will be under
	// contention if this lock is performed.
	gboolean is_contended = FALSE;
	GraphNode *block_target_node = pending_target_node->contents;
	if ( block_target_node->in_edges->len > 0 
	     || is_writer_locked (lock, held_writer_locks) ) {
	  is_contended = TRUE;
	}
	
	if ( is_contended ) {
	  GraphNode *block_node = plock_node->contents;
	  GraphEdge *prospective_bedge
	    = graph_add_edge (block_graph, block_node, block_target_node,
			      READER_EDGE);
	  prospective_edge->contents = prospective_bedge;
	}
      }

      GQueue *path;
      gboolean has_cycle
	= graph_node_in_cycle (block_graph, block_target_node, &path);

      if ( has_cycle ) {
	for ( ii = 0 ; ii < prospective_pedges->len ; ii++ ) {
	  GraphEdge *pending_edge = g_ptr_array_index (prospective_pedges, ii);
	  g_assert (pending_edge->contents != NULL);
	  graph_remove_edge (block_graph, pending_edge->contents);
	  graph_remove_edge (pending_graph, pending_edge);
	}	
	result = FALSE;
      }
      else {
	g_ptr_array_add (requestor_rlocks, g_string_new (lock->str));
	result = TRUE;
      }
      
      g_queue_free (path);

      my_g_ptr_array_really_free (prospective_pedges, NULL);
    }
  }
  
  else if ( query->request_type == ATTEMPT_WRITER_LOCK ) {

    // New edges that will be added to the pending graph and the block
    // graph if the operation is approved.
    GPtrArray *prospective_edges = g_ptr_array_new ();
    guint ii;
    for ( ii = 0 ; ii < requestor_locks->len ; ii++ ) {
      GString *lock_id = g_ptr_array_index (requestor_locks, ii);
      if ( g_string_equal (lock_id, lock) ) {
	g_error ("erroneous relock attempt by %s of %s", requestor->str,
		 lock_id->str);
      }
      g_assert (!g_string_equal (lock_id, lock));
      
      GraphNode *block_node = g_hash_table_lookup (block_nodes, lock_id);
      g_assert (block_node != NULL);
      GraphEdge *new_b_edge = graph_add_edge (block_graph, block_node,
					      block_target_node, 
					      WRITER_EDGE);
      GraphNode *pend_node = g_hash_table_lookup (pending_nodes, lock_id);
      g_assert (pend_node != NULL);
      GraphEdge *new_p_edge = graph_add_edge (pending_graph, pend_node,
					      pending_target_node,
					      new_b_edge);
      g_ptr_array_add (prospective_edges, new_p_edge);
    }
    
    // Edges already in the pending graph that will have twins added
    // in the block graph if the operation is approved.
    GPtrArray *prospective_promotions = g_ptr_array_new ();
    for ( ii = 0 ; ii < pending_target_node->in_edges->len ; ii++ ) {
      GraphEdge *ce = g_ptr_array_index (pending_target_node->in_edges, ii);
      if ( ce->contents == NULL ) {
	// Note that if the edge wasn't already in the block graph, it
	// must be a reader request.
	GraphEdge *pp = graph_add_edge (block_graph, ce->from_node->contents,
					block_target_node, READER_EDGE);
	ce->contents = pp;
	g_ptr_array_add (prospective_promotions, ce);
      }
    }
    
    GQueue *path;
    gboolean has_cycle
      = graph_node_in_cycle (block_graph, block_target_node, &path);
    
    // If we find the operation would create a cycle in the block
    // graph, undo all the graph changes and return false.
    if ( has_cycle ) {
      
      for ( ii = 0 ; ii < prospective_edges->len ; ii++ ) {
	GraphEdge *pending_edge = g_ptr_array_index (prospective_edges, ii);
	g_assert (pending_edge->contents != NULL);
	graph_remove_edge (block_graph, pending_edge->contents);
	graph_remove_edge (pending_graph, pending_edge);
      }	
      for ( ii = 0 ; ii < prospective_promotions->len ; ii++ ) {
	GraphEdge *ce = g_ptr_array_index (prospective_promotions, ii);
	graph_remove_edge (block_graph, ce->contents);
	ce->contents = NULL;
      }
      result = FALSE;
    }

    else {
      g_ptr_array_add (requestor_wlocks, g_string_new (lock->str));
      result = TRUE;
    }

    g_queue_free (path);
    
    my_g_ptr_array_really_free (prospective_promotions, NULL);
    my_g_ptr_array_really_free (prospective_edges, NULL);
  }
                                            
  else if ( query->request_type == ACHIEVE_READER_LOCK ) {
    // This assertion is not valid: 
    // g_assert (!is_writer_locked (lock, held_writer_locks));
    //
    // The reason is that another thread may already have attempted to
    // write lock the thread, which though the attempt will not
    // succeed yet (since this reader lock has gotten in first), will
    // result in the thread being added to the lock list in
    // anticipation of its possible completion.
    guint ii;
    for ( ii = 0 ; ii < requestor_locks->len ; ii++ ) {
      GString *lock_id = g_ptr_array_index (requestor_locks, ii);
      // In general we want to remove one link representing a reader
      // lock from the pending graph (and also from the block graph if
      // the removed link is represented there) for each other lock
      // held by the requestor.  The exception is the lock we are just
      // achieving, which for reasons discusseed elsewhere is already
      // in the lock list.
      if ( g_string_equal (lock_id, lock) ) {
	continue;
      }
      GraphNode *from_p_node = g_hash_table_lookup (pending_nodes, lock_id);
      guint jj;
      gboolean remove_successful = FALSE;
      for ( jj = 0 ; jj < pending_target_node->in_edges->len ; jj++ ) {
	GraphEdge *ce
	  = g_ptr_array_index (pending_target_node->in_edges, jj);
	if ( ce->from_node == from_p_node
	     && (ce->contents == NULL 
		 || (((GraphEdge *) ce->contents)->contents
		     == READER_EDGE)) ) {
	  if ( ce->contents != NULL ) {
	    graph_remove_edge (block_graph, ce->contents);
	  }
	  graph_remove_edge (pending_graph, ce);
	  remove_successful = TRUE;
	  break;
	}
      }
      g_assert (remove_successful);
    }

    result = TRUE;
  }

  else if ( query->request_type == ACHIEVE_WRITER_LOCK ) {
    g_assert (is_writer_locked (lock, held_writer_locks));
    guint ii;
    for ( ii = 0 ; ii < requestor_locks->len ; ii++ ) {
      GString *lock_id = g_ptr_array_index (requestor_locks, ii);
      // See comments under request type ACHIEVE_READER_LOCK.
      if ( g_string_equal (lock_id, lock) ) {
	continue;
      }
      GraphNode *from_p_node = g_hash_table_lookup (pending_nodes, lock_id);
      guint jj;
      gboolean remove_successful = FALSE;
      for ( jj = 0 ; jj < pending_target_node->in_edges->len ; jj++ ) {
	GraphEdge *ce
	  = g_ptr_array_index (pending_target_node->in_edges, jj);
	if ( ce->from_node == from_p_node ) {
	  // This link was in contention with the writer that was just
	  // achieved, so it had better appear in the block graph as
	  // well.
	  g_assert (ce->contents != NULL);
	  if ( ((GraphEdge *) ce->contents)->contents == WRITER_EDGE ) {
	    graph_remove_edge (block_graph, ce->contents);
	    graph_remove_edge (pending_graph, ce);
	    remove_successful = TRUE;
	    break;
	  }
	}
      }
      g_assert (remove_successful);
    }
    result = TRUE;
  }

  else if ( query->request_type == ACHIEVE_READER_UNLOCK ) {
    guint ii;
    gboolean found_lock = FALSE;
    for ( ii = 0 ; ii < requestor_rlocks->len ; ii++ ) {
      GString *lock_id = g_ptr_array_index (requestor_rlocks, ii);
      if ( g_string_equal (lock_id, lock) ) {
	g_assert (!found_lock);	// Shouldn't be locked more than once.
	// IMPROVEME: this invalidates requestor_locks, which doesn't
	// matter at the moment, but is a very fragile arrangement.
	g_ptr_array_remove_index_fast (requestor_rlocks, ii);
	ii--;
	my_g_string_free (lock_id);
	found_lock = TRUE;
      }
    }
    if ( !found_lock ) {
      g_error ("failed to find lock %s in function %s, file "__FILE__
	       ", line %d\n", lock->str, __func__, __LINE__);
    }
    g_assert (found_lock);

    result = TRUE;
  }

  else if ( query->request_type == ACHIEVE_WRITER_UNLOCK ) {
    guint ii;
    gboolean found_lock = FALSE;
    for ( ii = 0 ; ii < requestor_wlocks->len ; ii++ ) {
      GString *lock_id = g_ptr_array_index (requestor_wlocks, ii);
      if ( g_string_equal (lock_id, lock) ) {
	g_assert (!found_lock);
	// IMPROVEME: this invalidates requestor_locks, which doesn't
	// matter at the moment, but is a very fragile arrangement.
	g_ptr_array_remove_index_fast (requestor_wlocks, ii);
	ii--;
	my_g_string_free (lock_id);
	found_lock = TRUE;
      }
    }
    if ( !found_lock ) {
      g_error ("failed to find lock %s in function %s, file "__FILE__
	       ", line %d\n", lock->str, __func__, __LINE__);
    }
    g_assert (found_lock);

    // If the remaining links are all reader lock attempts and the
    // node is not listed as (potentially) locked (first condition
    // should imply the second, but not necesarilly the other way
    // around), there is no further contention and we can remove all
    // the links from the block graph.
    gboolean still_contended = FALSE;
    gboolean incoming_writer_edges_exist = FALSE;
    for ( ii = 0 ; ii < pending_target_node->in_edges->len ; ii++ ) {
      GraphEdge *ce = g_ptr_array_index (pending_target_node->in_edges, ii);
      GraphEdge *cbe = ce->contents;
      if ( cbe->contents == WRITER_EDGE ) {
	incoming_writer_edges_exist = TRUE;
	break;
      }
    }
    if ( incoming_writer_edges_exist ) {
      g_assert (is_writer_locked (lock, held_writer_locks));
      still_contended = TRUE;
    }
    else if ( is_writer_locked (lock, held_writer_locks) ) {
      still_contended = TRUE;
    }
    if ( !still_contended ) {
      for ( ii = 0 ; ii < pending_target_node->in_edges->len ; ii++ ) {
	GraphEdge *ce = g_ptr_array_index (pending_target_node->in_edges,
					   ii);
	GraphEdge *cbe = ce->contents;
	g_assert (cbe != NULL);
	graph_remove_edge (block_graph, cbe);
	ce->contents = NULL;
      }
    }
    result = TRUE;
  }

  else {
    g_assert_not_reached ();
  }

  check_invariants (pending_graph, block_graph, pending_nodes,
		    held_writer_locks, node_names, query->request_type, lock);

  my_g_ptr_array_really_free (pkeys, NULL);
  my_g_ptr_array_really_free (pvalues, NULL);
  my_g_ptr_array_really_free (bkeys, NULL);
  my_g_ptr_array_really_free (bvalues, NULL);

  g_hash_table_destroy (node_names);

  my_g_ptr_array_really_free (requestor_locks, NULL);

  my_g_string_free (requestor_space_free);

  my_g_string_free (lock);
  my_g_string_free (requestor);

  return result;
}

// We need to ensure that as we execute random operations to exercise
// the class, we don't do anything that would result in deadlock.  To
// do this, we have a dedicated process that maintains a graph
// representing pending lock requests and can tell us if a given
// operation is ok to attempt.  We have to synchronize with this
// process at the appropriate times using IPC, of course.
static void
deadlock_preventer (void)
{
  // These graphs have nodes which correspond to the table keys, plus
  // one additional node for the entire table lock.  The edges
  // represent attempts by a thread holding the lock from which the
  // edge originates to obtain the lock to which the edge points.  The
  // pending graph containts all pending edges, the block graph
  // contains only those edges which contend which each other or an
  // existing write lock on the target node.  In other words, the
  // pending graph is the graph of lock requests that will succeed
  // under the current set of granted and pending locks, while the
  // block graph is the graph of lock requests that may block under
  // the current set.
  Graph *pending_graph = graph_new ();
  Graph *block_graph = graph_new ();
  
  // Hashes of requestor_id GString instances to GPtrArray instances
  // consisting of lock_id GString instances of the locks the threads
  // hold.
  GHashTable *held_reader_locks
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     NULL);
  GHashTable *held_writer_locks
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     NULL);

  // Mapping of lock ID strings to graph nodes in the pending graph.
  // The key is automaticly freed on removal, but the client is
  // responsible for dealing with the graph node itself.
  GHashTable *pending_nodes
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     NULL);

  // Mapping of lock ID strings to graph nodes in the block graph.
  GHashTable *block_nodes
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free,
			     NULL);

  struct mq_attr request_queue_attr;
  int return_code = mq_getattr (request_queue, &request_queue_attr);
  g_assert (return_code == 0);
  g_assert (request_queue_attr.mq_msgsize >= sizeof (op_query_t));
  void *op_query_buffer = g_malloc (request_queue_attr.mq_msgsize);

  for ( ; ; ) {
    // Wait for a message from somebody wanting us to do something.
    errno = 0;
    ssize_t message_size = mq_receive (request_queue, op_query_buffer,
				       request_queue_attr.mq_msgsize, NULL);
    if ( errno != 0 ) {
      perror ("mq_receive error");
      g_assert_not_reached ();
    }
    g_assert (message_size == sizeof (op_query_t));

    op_query_t *query = op_query_buffer;

    // If somebody wants us to shutdown, do so.
    if ( query->shutdown_request ) {
      // IMPROVEME: For now we go ahead and leak the graph and other
      // paraphenalia, it won't matter.
      break;
    }

    op_query_response_t query_response;

    // Determine if the current operation will result in deadlock,
    // returning TRUE if it will not and FALSE if it will.  If TRUE is
    // returned, we also modify the current graphs and lock state to
    // represent the new situation.    
    query_response.op_ok = try_op (held_reader_locks, held_writer_locks,
				   pending_nodes, block_nodes,
				   pending_graph, block_graph, query);

    // And let the thread that queried the operation know the result
    // is ready.
    int return_code = mq_send (query->response_queue, (void *) &query_response,
			       sizeof (op_query_response_t), 0);
    if ( return_code != 0 ) {
      perror ("mq_send failed");
      g_assert_not_reached ();
    }
  }

  exit (EXIT_SUCCESS);
}

// Types of test tasks we might try to perform.
typedef enum {
  TASK_TYPE_ADD_FIELD,
  TASK_TYPE_REMOVE_FIELD,
  TASK_TYPE_READER_LOCK_FIELD,
  TASK_TYPE_READER_UNLOCK_FIELD,
  TASK_TYPE_WRITER_LOCK_FIELD,
  TASK_TYPE_WRITER_TRYLOCK_FIELD,
  TASK_TYPE_WRITER_UNLOCK_FIELD,
  TASK_TYPE_SET_FIELD_VALUE,
  TASK_TYPE_GET_FIELD_VALUE,
  TASK_TYPE_READER_LOCK_TABLE,
  TASK_TYPE_READER_UNLOCK_TABLE,
  TASK_TYPE_WRITER_LOCK_TABLE,
  TASK_TYPE_WRITER_UNLOCK_TABLE
} task_type_t;

// Return a pointer to a static GString describing task_type.  The
// return string is owned by this function and is only gauranteed to
// remain unchanged until it is next called.
//
// This is a fully working function useful for debugging, but is
// currently unused so is commented out to avoid a compiler warning.
/*
static GString *
task_type_as_gstring (task_type_t task_type)
{
  static GString *result = NULL;
  if ( result == NULL ) {
    result = g_string_new ("");
  }  

  switch ( task_type ) {
  case TASK_TYPE_ADD_FIELD:
    g_string_assign (result, "ADD_FIELD");
    break;
  case TASK_TYPE_REMOVE_FIELD:
    g_string_assign (result, "REMOVE_FIELD");
    break;
  case TASK_TYPE_READER_LOCK_FIELD:
    g_string_assign (result, "READER_LOCK_FIELD");
    break;
  case TASK_TYPE_READER_UNLOCK_FIELD:
    g_string_assign (result, "READER_UNLOCK_FIELD");
    break;
  case TASK_TYPE_WRITER_LOCK_FIELD:
    g_string_assign (result, "WRITER_LOCK_FIELD");
    break;
  case TASK_TYPE_WRITER_UNLOCK_FIELD:
    g_string_assign (result, "WRITER_UNLOCK_FIELD");
    break;
  case TASK_TYPE_SET_FIELD_VALUE:
    g_string_assign (result, "SET_FIELD_VALUE");
    break;
  case TASK_TYPE_GET_FIELD_VALUE:
    g_string_assign (result, "GET_FIELD_VALUE");
    break;
  case TASK_TYPE_READER_LOCK_TABLE:
    g_string_assign (result, "READER_LOCK_TABLE");
    break;
  case TASK_TYPE_READER_UNLOCK_TABLE:
    g_string_assign (result, "READER_UNLOCK_TABLE");
    break;
  case TASK_TYPE_WRITER_LOCK_TABLE:
    g_string_assign (result, "WRITER_LOCK_TABLE");
    break;
  case TASK_TYPE_WRITER_UNLOCK_TABLE:
    g_string_assign (result, "WRITER_UNLOCK_TABLE");
    break;
  }

  return result;
}
*/

// Return a random task type, possibly with a weighting that seems
// likely to make the testing cover things well.
static task_type_t
random_op (void)
{
  // Hackish way to try to ensure that there are the number of
  // possibilities we think there are.  Of course this doesn't really
  // cover the possibilities.
  g_assert (TASK_TYPE_ADD_FIELD == 0);
  g_assert (TASK_TYPE_WRITER_UNLOCK_TABLE == 11);

  int result;

  gboolean selected = FALSE;

#ifdef TEST_TABLE_FILE_ENABLE_RANDOM_DELAYS
  random_delay (0.03);
#endif

  while ( !selected ) {
    result = floor (my_random (TASK_TYPE_WRITER_UNLOCK_TABLE + 1 - 0.000001));
    g_assert (result <= 11);
    
    // Make table write lock results rarer than others.  This just
    // makes things a bit more interesting by alleviating the tendency
    // for all the deadlock conditions to involve the table lock.  On
    // longer test runs, we can get away with even rarer table write
    // lock operations.
    if ( result == TASK_TYPE_WRITER_LOCK_TABLE ) {
#ifdef TEST_TABLE_FILE_LONG_TEST_RUN
      const gdouble write_lock_rarity = 0.02;
#else
      const gdouble write_lock_rarity = 0.2;
#endif
      if ( my_random (1.0) > 1.0 - write_lock_rarity ) {
	selected = TRUE;
      }
    }
    else {
      selected = TRUE;
    }
  }

  return result;
}

// Send a message to the deadlock_preventer.  The arguments are the id
// of the requesting thread, the response_queue that the
// deadlock_preventer should use when it answers, the type of request
// we are making, and the lock_id which is the target of the request.
static void
send_message (sem_t *request_queue_semaphore, char *requestor_id,
	      mqd_t response_queue, request_type_t request_type, char *lock_id)
{
#ifdef TEST_TABLE_FILE_ENABLE_RANDOM_DELAYS
  random_delay (0.1);
#endif

  op_query_t op_query;
  op_query.shutdown_request = FALSE;
  strcpy (op_query.requestor_id, requestor_id);
  op_query.response_queue = response_queue;
  op_query.request_type = request_type;
  strcpy (op_query.lock_id, lock_id);
  int return_code = sem_wait (request_queue_semaphore);
  g_assert (return_code == 0);
  return_code = mq_send (request_queue, (void *) &op_query,
			     sizeof (op_query_t), 0);
  g_assert (return_code == 0);
  return_code = sem_post (request_queue_semaphore);
  g_assert (return_code == 0);
 
#ifdef TEST_TABLE_FILE_ENABLE_RANDOM_DELAYS
  random_delay (0.05);
#endif
}


// Receive a message from the deadlock_preventer.  Blocks until the
// result is received and then returns the success or failure which is
// (as of this writing) the meat of the message.
static gboolean
receive_message (mqd_t *response_queue)
{
  struct mq_attr response_queue_attr;
  int return_code = mq_getattr (*response_queue, &response_queue_attr);
  g_assert (return_code == 0);
  g_assert (response_queue_attr.mq_msgsize  >= sizeof (op_query_response_t));
  void *response_buffer = g_malloc (response_queue_attr.mq_msgsize);
  ssize_t receive_results 
    = mq_receive (*response_queue, response_buffer,
		  response_queue_attr.mq_msgsize, NULL);
  g_assert (receive_results == sizeof (op_query_response_t));
  op_query_response_t *query_response = response_buffer;
  gboolean result = query_response->op_ok;
  g_free (response_buffer);

  return result;
}

// Return the pointers to GString instances in keys which do not have
// g_string_equal instances in used_keys.  All the values in used_keys
// are expected to exist in keys.  The GPtrArray returned is new, but
// the pointers in contains are still owned by keys.
static GPtrArray *
get_free_keys (GPtrArray *keys, GPtrArray *used_keys)
{
  // We start out with the result in a hash table so we can more
  // efficiently compute the disunion.
  GHashTable *result = g_hash_table_new ((GHashFunc) g_string_hash,
					 (GEqualFunc) g_string_equal);
  guint ii;
  for ( ii = 0 ; ii < keys->len ; ii++ ) {
    GString *element = g_ptr_array_index (keys, ii);
    g_hash_table_insert (result, element, NULL);
  }

  for ( ii = 0 ; ii < used_keys->len ; ii++ ) {
    GString *element = g_ptr_array_index (used_keys, ii);
    gboolean remove_result = g_hash_table_remove (result, element);
    g_assert (remove_result);
  }

  GPtrArray *result_array = my_g_hash_table_keys (result);

  g_hash_table_destroy (result);

  return result_array;
}

// The code that actually exercises the TableFile instance from a
// thread.
static gpointer
test_thread_func (gpointer data)
{
  GString *pts = pid_thread_string ();

  // First we have to grab a queue on which to listen to the deadlock
  // preventer.
  sem_t *queue_list_semaphore = sem_open (queue_list_semaphore_name, 0);
  g_assert (queue_list_semaphore != SEM_FAILED);
  int result_code = sem_wait (queue_list_semaphore);
  g_assert (result_code == 0);
  mqd_t *response_queue = g_ptr_array_index (response_queues, *queue_number);
  (*queue_number)++;
  result_code = sem_post (queue_list_semaphore);
  g_assert (result_code == 0);

  // We also have to have access to the named semaphore which protects
  // the write end of the request queue.
  sem_t *request_queue_semaphore = sem_open (request_queue_semaphore_name, 0);
  g_assert (request_queue_semaphore != SEM_FAILED);
  sem_t *rqs = request_queue_semaphore;	// Shorthand.

  // Form ID string we use to refer to ourselves.
  GString *requestor_id = g_string_new ("");
  g_string_append_printf (requestor_id, "process %lld thread %p",
			  (long long int) getpid (),
			  g_thread_self ());

  // Flags true iff we hold the lock in question.
  gboolean have_table_writer = FALSE, have_table_reader = FALSE;

  // Existence hash recording which field names we hold writer locks for.
  GHashTable *writer_locks
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free, NULL);
  // Existence hash recording which fields names we hold reader locks for.
  GHashTable *reader_locks
    = g_hash_table_new_full ((GHashFunc) g_string_hash,
			     (GEqualFunc) g_string_equal,
			     (GDestroyNotify) my_g_string_free, NULL);

  GTimer *timer = g_timer_new ();
  gulong junk;			// For useless parameter of g_timer_elapsed().

  while ( g_timer_elapsed (timer, &junk) < thread_test_run_time
	  || have_table_writer || have_table_reader
	  || g_hash_table_size (writer_locks) > 0
	  || g_hash_table_size (reader_locks) > 0 ) {

    task_type_t task_type = random_op ();

    switch ( task_type ) {

    case TASK_TYPE_ADD_FIELD:
      {
	if ( g_timer_elapsed (timer, &junk) >= thread_test_run_time ) {
	  break;
	}
	if ( have_table_writer ) {
	  // Check if we think we have room for another field.
	  GPtrArray *fields = table_file_catalog (tf);
	  GPtrArray *free_keys = get_free_keys (keys, fields);
	  if ( fields->len < field_count ) {
	    g_assert (free_keys->len > 0);
	    GString *field_name
	      = g_ptr_array_index (free_keys, free_keys->len - 1);

	    // The field is born with a field writer lock, and we want
	    // the lock graph to reflect this, so we ask for a writer
	    // lock (which had better be granted).
	    send_message (rqs, requestor_id->str, *response_queue,
			  ATTEMPT_WRITER_LOCK, field_name->str);
	    gboolean attempt_result = receive_message (response_queue);
	    g_assert (attempt_result);
	    send_message (rqs, requestor_id->str, *response_queue,
			  ACHIEVE_WRITER_LOCK, field_name->str);
	    gboolean achieve_result = receive_message (response_queue);
	    g_assert (achieve_result);
	    gboolean add_result
	      = table_file_add_field (tf, field_name->str, "field_value");
	    g_assert (add_result);
	    g_hash_table_insert (writer_locks, g_string_new (field_name->str),
				 NULL);
	  }
	  my_g_ptr_array_really_free (free_keys, NULL);
	  my_g_ptr_array_really_free (fields, (FreeFunc) my_g_string_free);
	}
	break;
      }
    case TASK_TYPE_REMOVE_FIELD:
      {
	if ( g_timer_elapsed (timer, &junk) >= thread_test_run_time ) {
	  break;
	}
	GPtrArray *choices = my_g_hash_table_keys (writer_locks);
	if ( have_table_writer && choices->len > 0 ) {
	  guint choice_index = floor (my_random (choices->len - 0.0001));
	  GString *field_name = g_ptr_array_index (choices, choice_index);
	  table_file_remove_field (tf, field_name->str);
	  // The field takes its writer lock with it when it is
	  // removed so we go ahead and request the release of this
	  // lock in the deadlock_preventer.
	  send_message (rqs, requestor_id->str, *response_queue,
			ACHIEVE_WRITER_UNLOCK, field_name->str);
	  gboolean attempt_result = receive_message (response_queue);
	  g_assert (attempt_result);
	  g_hash_table_remove (writer_locks, field_name);
	}
	my_g_ptr_array_really_free (choices, NULL);
	break;
      }
    case TASK_TYPE_READER_LOCK_FIELD:
      {
	if ( g_timer_elapsed (timer, &junk) >= thread_test_run_time ) {
	  break;
	}
	if ( have_table_writer || have_table_reader ) {
	  GPtrArray *fields = table_file_catalog (tf);
	  if ( fields->len > 0 ) {
	    guint field_to_lock = floor (my_random (fields->len - 0.0000001));
	    GString *field_name = g_ptr_array_index (fields, field_to_lock);
	    if ( !my_g_hash_table_entry_exists (reader_locks, field_name)
		 && !my_g_hash_table_entry_exists (writer_locks,
						   field_name) ) {
	      send_message (rqs, requestor_id->str, *response_queue,
			    ATTEMPT_READER_LOCK, field_name->str);
	      gboolean attempt_result = receive_message (response_queue);
	      if ( attempt_result ) {
		table_file_field_reader_lock (tf, field_name->str);
		send_message (rqs, requestor_id->str, *response_queue,
			      ACHIEVE_READER_LOCK, field_name->str);
		gboolean attempt_result = receive_message (response_queue);
		g_assert (attempt_result);
		g_hash_table_insert (reader_locks,
				     g_string_new (field_name->str), NULL);
	      }
	    }
	  }
	  // Free the fields list and its contents.
	  my_g_ptr_array_really_free (fields, (FreeFunc) my_g_string_free);
	}
	break;
      }
    case TASK_TYPE_READER_UNLOCK_FIELD:
      {
	GPtrArray *choices = my_g_hash_table_keys (reader_locks);
	if ( choices->len > 0 ) {
	  guint field_to_unlock 
	    = floor (my_random (choices->len - 0.0000001));	
	  GString *field_name = g_ptr_array_index (choices, field_to_unlock);
	  field_name = g_string_new (field_name->str);
	  g_hash_table_remove (reader_locks, field_name);
	  table_file_field_reader_unlock (tf, field_name->str);
	  send_message (rqs, requestor_id->str, *response_queue,
			ACHIEVE_READER_UNLOCK, field_name->str);
	  gboolean attempt_result = receive_message (response_queue);
	  g_assert (attempt_result);
	}
	my_g_ptr_array_really_free (choices, NULL);
	break;
      }
    case TASK_TYPE_WRITER_LOCK_FIELD:
      {
	if ( g_timer_elapsed (timer, &junk) >= thread_test_run_time ) {
	  break;
	}
	if ( have_table_writer || have_table_reader ) {
	  GPtrArray *fields = table_file_catalog (tf);
	  if ( fields->len > 0 ) {
	    guint field_to_lock 
	      = floor (my_random (fields->len - 0.0000001));
	    GString *field_name
	      = g_ptr_array_index (fields, field_to_lock);
	    if ( !my_g_hash_table_entry_exists (reader_locks, field_name)
		 && !my_g_hash_table_entry_exists (writer_locks, field_name)
		 ) {
	      send_message (rqs, requestor_id->str, *response_queue,
			    ATTEMPT_WRITER_LOCK, field_name->str);
	      gboolean attempt_result = receive_message (response_queue);
	      if ( attempt_result ) {
		table_file_field_writer_lock (tf, field_name->str);
		send_message (rqs, requestor_id->str, *response_queue,
			      ACHIEVE_WRITER_LOCK, field_name->str);
		gboolean attempt_result = receive_message (response_queue);
		g_assert (attempt_result);
		g_hash_table_insert (writer_locks,
				     g_string_new (field_name->str), NULL);
	      }
	    }
	  }
	  my_g_ptr_array_really_free (fields, (FreeFunc) my_g_string_free);
	}
	break;
      }
    case TASK_TYPE_WRITER_UNLOCK_FIELD:
      {
	GPtrArray *choices = my_g_hash_table_keys (writer_locks);
	if ( choices->len > 0 ) {
	  guint field_to_unlock 
	    = floor (my_random (choices->len - 0.0000001));	
	  GString *field_name = g_ptr_array_index (choices, field_to_unlock);
	  table_file_field_writer_unlock (tf, field_name->str);
	  send_message (rqs, requestor_id->str, *response_queue,
			ACHIEVE_WRITER_UNLOCK, field_name->str);
	  gboolean attempt_result = receive_message (response_queue);
	  g_assert (attempt_result);
	  g_hash_table_remove (writer_locks, field_name);
	}
	my_g_ptr_array_really_free (choices, NULL);
	break;
      }
    case TASK_TYPE_SET_FIELD_VALUE:
      {
	GPtrArray *choices = my_g_hash_table_keys (writer_locks);
	if ( choices->len > 0 ) {
	  guint field_to_set = floor (my_random (choices->len - 0.0000001));
	  GString *field_name = g_ptr_array_index (choices, field_to_set);
	  GString *test_value = g_string_new ("field value ");
	  // Field values are randomized with this value as the
	  // largest possible random integer to get appended.
	  const int mafvc = 100;
	  g_string_append_printf (test_value, "%d",
				  (int) floor (my_random (mafvc)));
	  table_file_set_field_value (tf, field_name->str, test_value->str);
	  my_g_string_free (test_value);
	}
	my_g_ptr_array_really_free (choices, NULL);
	break;
      }
    case TASK_TYPE_GET_FIELD_VALUE:
      {
	GPtrArray *reader_choices = my_g_hash_table_keys (reader_locks);
	GPtrArray *writer_choices = my_g_hash_table_keys (writer_locks);
	GPtrArray *choices = my_g_ptr_array_sum (reader_choices,
						 writer_choices);
	// FIXME: should also include writer locked fields in choices.
	if ( choices->len > 0 ) {
	  guint field_to_get = floor (my_random (choices->len - 0.0000001));
	  GString *field_name = g_ptr_array_index (choices, field_to_get);
	  GString *field_value = table_file_get_field_value (tf,
							     field_name->str);
	  my_g_string_free (field_value);
	}
	my_g_ptr_array_really_free (choices, NULL);
	my_g_ptr_array_really_free (writer_choices, NULL);
	my_g_ptr_array_really_free (reader_choices, NULL);
	break;
      }
    case TASK_TYPE_READER_LOCK_TABLE:
      {
	if ( g_timer_elapsed (timer, &junk) >= thread_test_run_time ) {
	  break;
	}
	if ( !have_table_writer && !have_table_reader ) {
	  GString *lock_name = g_string_new ("ENTIRE_TABLE_LOCK");
	  send_message (rqs, requestor_id->str, *response_queue,
			ATTEMPT_READER_LOCK, lock_name->str);
	  gboolean attempt_result = receive_message (response_queue);
	  if ( attempt_result ) {
	    table_file_table_reader_lock (tf);
	    send_message (rqs, requestor_id->str, *response_queue,
			  ACHIEVE_READER_LOCK, lock_name->str);
	    gboolean attempt_result = receive_message (response_queue);
	    g_assert (attempt_result);
	    have_table_reader = TRUE;
	  }
	  my_g_string_free (lock_name);
	}
	break;
      }
    case TASK_TYPE_READER_UNLOCK_TABLE:
      {
	if ( have_table_reader ) {
	  GString *lock_name = g_string_new ("ENTIRE_TABLE_LOCK");
	  table_file_table_reader_unlock (tf);
	  send_message (rqs, requestor_id->str, *response_queue,
			ACHIEVE_READER_UNLOCK, lock_name->str);
	  gboolean attempt_result = receive_message (response_queue);
	  g_assert (attempt_result);
	  have_table_reader = FALSE;
	  my_g_string_free (lock_name);
	}
	break;
      }
    case TASK_TYPE_WRITER_LOCK_TABLE:
      {
	if ( g_timer_elapsed (timer, &junk) >= thread_test_run_time ) {
	  break;
	}
	if ( !have_table_writer && !have_table_reader ) {
	  GString *lock_name = g_string_new ("ENTIRE_TABLE_LOCK");
	  send_message (rqs, requestor_id->str, *response_queue,
			ATTEMPT_WRITER_LOCK, lock_name->str);
	  gboolean attempt_result = receive_message (response_queue);
	  if ( attempt_result ) {
	    table_file_table_writer_lock (tf);
	    send_message (rqs, requestor_id->str, *response_queue,
			  ACHIEVE_WRITER_LOCK, lock_name->str);
	    gboolean attempt_result = receive_message (response_queue);
	    g_assert (attempt_result);
	    have_table_writer = TRUE;
	  }
	  my_g_string_free (lock_name);
	}
	break;
      }
    case TASK_TYPE_WRITER_UNLOCK_TABLE:
      {
	if ( have_table_writer ) {
	  GString *lock_name = g_string_new ("ENTIRE_TABLE_LOCK");
	  table_file_table_writer_unlock (tf);
	  send_message (rqs, requestor_id->str, *response_queue,
			ACHIEVE_WRITER_UNLOCK, lock_name->str);
	  gboolean attempt_result = receive_message (response_queue);
	  g_assert (attempt_result);
	  have_table_writer = FALSE;
	  my_g_string_free (lock_name);
	}
	break;
      }
    default:
      g_assert_not_reached ();
      break;
    }

  }

  my_g_string_free (pts);

  return (guint *) &thread_success;
}


// This routine performs testing in sigle threaded process.
static void
single_threaded_process (void)
{
  gpointer result_pointer = test_thread_func (NULL);

  if ( *((guint *) result_pointer) == thread_success ) {
    exit (EXIT_SUCCESS);
  }
  else {
    exit (EXIT_FAILURE);
  }
}

// Once the groundwork is laid, this routine takes over in each of the
// processes in which multithreaded testing is to take place.
static void
multithreaded_process (void)
{
  GPtrArray *test_threads = g_ptr_array_new ();

  unsigned int ii;

  for ( ii = 0 ; ii < thread_count ; ii++ ) {
    GThread *tmp = g_thread_create (test_thread_func, NULL, TRUE, NULL);
    g_assert (tmp != NULL);
    g_ptr_array_add (test_threads, tmp);
  }

  for ( ii = 0 ; ii < thread_count ; ii++ ) {
    gpointer return_pointer
      = g_thread_join (g_ptr_array_index (test_threads, ii));
    g_assert (*((int *) return_pointer) == thread_success);
  }

  exit (EXIT_SUCCESS);
}

///////////////////////////////////////////////////////////////////////////////


// This handler routine mostly works (as well as ever for multiprocess
// code, I think), but it is unused in the non-debugging version of
// this test driver so it is commented out for the moment.
/*
static void
backtrace_log_handler (const gchar *log_domain, GLogLevelFlags log_level,
		       const gchar *message, gpointer user_data)
{
  g_log_default_handler (log_domain, log_level, message, user_data);

  g_on_error_stack_trace ("/home/bkerin/projects/asf_2nd_time/trunk/src/ssv/"
			  "test_table_file");
}
*/

int
main (void)
{
  // Uncomment this and fix the name of the executable in the
  // backtrace_log_handler to get log traces sometimes (output still
  // seems confusing when combined with multiprocess output).
  // 
  //  g_log_set_handler ("", (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_ERROR 
  //			  | G_LOG_LEVEL_CRITICAL), backtrace_log_handler,
  //		     NULL);

  // Critical warnings are bugs.
  g_log_set_fatal_mask ("", (G_LOG_FLAG_RECURSION | G_LOG_FLAG_FATAL
			     | G_LOG_LEVEL_ERROR | G_LOG_LEVEL_CRITICAL));

  // Critical warnings from glib are also unacceptable.
  g_log_set_fatal_mask ("GLib", (G_LOG_FLAG_RECURSION | G_LOG_FLAG_FATAL
  				 | G_LOG_LEVEL_ERROR | G_LOG_LEVEL_CRITICAL));

  g_assert (multithreaded_process_count + single_thread_process_count
	    == total_process_count);

  // Since named message queues are system wide resources we try to
  // use a name that will be unique.  We don't weave in the process
  // number or anything though, since these structures have kernel
  // persistence and we don't want them to end up accumulating.
  // IMPROVEME: it would be nicer to use unnamed queues but this would
  // mean more mucking around with shared memory, which is a pain.
  GString *request_queue_name
    = g_string_new ("/test_table_file_request_queue");
  // We have a number of these to contend with, so this is just the
  // base name and we affix a number to the end later.
  GString *response_queue_base_name
    = g_string_new ("/test_table_file_response_queue_");

  // Set up the queue that requestors use to ask questions of the
  // deadlock preventer.
  int return_code = mq_unlink (request_queue_name->str);
  if ( return_code != 0 ) {
    g_assert (errno == ENOENT);
  }
  request_queue = mq_open (request_queue_name->str, O_RDWR | O_CREAT,
			   S_IRUSR | S_IWUSR, NULL);
  g_assert (request_queue != (mqd_t) -1);

  // Set up the queues that the deadlock preventer uses to answer the
  // requestors.
  response_queues = g_ptr_array_new ();
  guint total_thread_count = (single_thread_process_count 
			      + multithreaded_process_count * thread_count);
  guint ii;
  for ( ii = 0 ; ii < total_thread_count ; ii++  ) {
    mqd_t *current_descriptor = g_new (mqd_t, 1);
    GString *queue_name = g_string_new (response_queue_base_name->str);
    g_string_append_printf (queue_name, "%ud", ii);
    int return_code = mq_unlink (queue_name->str);
    if ( return_code != 0 ) {
      g_assert (errno == ENOENT);
    }
    *current_descriptor = mq_open (queue_name->str, O_RDWR | O_CREAT,
				   S_IRUSR | S_IWUSR, NULL);
    g_assert (*current_descriptor !=  (mqd_t) -1);
    g_ptr_array_add (response_queues, current_descriptor);
  }


  // Set up the semaphore that we use for queue number assignment.
  //
  // Delete any existing semaphore of the name we want to use.
  return_code = sem_unlink (queue_list_semaphore_name);
  g_assert (return_code == 0 || errno == ENOENT);
  // Create new semaphore.
  queue_list_semaphore = sem_open (queue_list_semaphore_name,
				   O_CREAT | O_EXCL, S_IRWXU, 1);
  g_assert (queue_list_semaphore != SEM_FAILED);
  int sem_value;
  return_code = sem_getvalue (queue_list_semaphore, &sem_value);
  g_assert (return_code == 0);
  g_assert (sem_value == 1);

  // We need the queue number in shared memory, and initialize it.
  queue_number = mmap (0, sizeof (int), PROT_READ | PROT_WRITE,
		       MAP_SHARED | MAP_ANONYMOUS, -1, 0);
  *queue_number = 0;

  // Set up the semaphore we use to coordinate writes to the request
  // queue.
  //
  // Delete any existing semaphore of the name we want to use.
  return_code = sem_unlink (request_queue_semaphore_name);
  g_assert (return_code == 0 || errno == ENOENT);
  // Create new semaphore.
  request_queue_semaphore = sem_open (request_queue_semaphore_name,
				      O_CREAT | O_EXCL, S_IRWXU, 1);
  g_assert (request_queue_semaphore != SEM_FAILED);
  return_code = sem_getvalue (request_queue_semaphore, &sem_value);
  g_assert (return_code == 0);
  g_assert (sem_value == 1);

  // Form the key and value strings to be used in the tests.
  keys = g_ptr_array_new ();
  for ( ii = 0 ; ii < field_count ; ii++ ) {
    GString *key = g_string_new ("");
    g_string_append_printf (key, "key_%d", ii);
    g_ptr_array_add (keys, key);
  }

  // Create the new table file we will use for testing, after
  // unlinking any file left over from a previous test.
  return_code = unlink (TEST_TABLE_FILE_NAME);
  if ( return_code != 0 && errno != ENOENT ) {
    perror ("unlink failed");
    g_assert_not_reached ();
  }
  tf = table_file_new (TEST_TABLE_FILE_NAME);

  // Start up the dedicated deadlock detection/prevention process.
  pid_t dp_pid = fork ();
  g_assert (dp_pid != -1);
  if ( dp_pid == 0 ) {
    deadlock_preventer ();
  }

  // PIDs for the actual test processes.
  pid_t *pids = g_new (pid_t, total_process_count);

  // Single thread test processes.
  for ( ii = 0 ; ii < single_thread_process_count ; ii++ ) {
    pids[ii] = fork ();
    g_assert (pids[ii] != -1);
    if ( pids[ii] == 0 ) {
      // Child.
      single_threaded_process ();
    }
  }

  // Multithread test processes.
  // Unfortunately, at the moment the multithread functionality of
  // TableFile doesn't work yet, hence this assertion.
  g_assert (multithreaded_process_count == 0);
  for ( ii = 0 ; ii < multithreaded_process_count ; ii++ ) {
    pid_t pid = fork ();
    g_assert (pid != -1);
    if ( pid == 0 ) {
      // Child.
      multithreaded_process ();
    }
    pids[single_thread_process_count + ii] = pid;
  }

  // Wait for all the test processes to exit successfully.
  gboolean all_processes_ok = TRUE;
  for ( ii = 0 ; ii < total_process_count ; ii++ ) {
    int child_status;
    pid_t pid = waitpid (-1, &child_status, 0);
    g_assert (pid != (pid_t) -1);
    if ( !WIFEXITED (child_status) ) {
      if ( WIFSIGNALED (child_status) ) {
	g_warning ("Process %lld was killed by signal number %d",
		   (long long int) pid, WTERMSIG (child_status));
      }
      else {
	g_warning ("Process %lld didn't exit normally.", (long long int) pid);
      }
      all_processes_ok = FALSE;
    }
    else {
      if ( WEXITSTATUS (child_status) != 0 ) {
	g_warning ("Process %lld returned non-zero exit statud", 
		   (long long int) pid);
	all_processes_ok = FALSE;
      }
    }
  }
  g_assert (all_processes_ok);

  // Send a shutdown request to the deadlock prevention process.
  op_query_t shutdown_request;
  shutdown_request.shutdown_request = TRUE;
  int result_code = mq_send (request_queue, (void *) &shutdown_request,
			     sizeof (op_query_t), 0);
  g_assert (result_code == 0);
  int child_status;
  pid_t waitpid_result = waitpid (dp_pid, &child_status, 0);
  g_assert (waitpid_result == dp_pid);
  g_assert (WIFEXITED (child_status));
  g_assert (WEXITSTATUS (child_status) == 0);
  
  // IMPROVEME: We don't bother to free a bunch of stuff.  This is
  // just test code, after all.

  exit (EXIT_SUCCESS);
}
