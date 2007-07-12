// Test code for the depth first search functionality of the Graph
// class.

#include <stdlib.h>

#include <glib.h>

#include "graph.h"

int
main (void)
{
  Graph *test_graph = graph_new ();

  GraphNode *a = graph_add_node (test_graph, NULL);
  GraphNode *b = graph_add_node (test_graph, NULL);
  GraphNode *c = graph_add_node (test_graph, NULL);
  GraphNode *d = graph_add_node (test_graph, NULL);
  GraphNode *e = graph_add_node (test_graph, NULL);

  GraphEdge *ab = graph_add_edge (test_graph, a, b, NULL);
  GraphEdge *ba = graph_add_edge (test_graph, b, a, NULL);
  ba = ba;
  GraphEdge *bc = graph_add_edge (test_graph, b, c, NULL);
  GraphEdge *dc = graph_add_edge (test_graph, d, c, NULL);
  dc = dc;
  GraphEdge *ce = graph_add_edge (test_graph, c, e, NULL);

  graph_dump_by_edges (test_graph, "test_graph.dot", NULL);

  GQueue *path;
  gboolean search_result
    = graph_depth_first_search_for_node (test_graph, a, e, &path);

  if ( search_result ) {
    g_assert (g_queue_peek_head (path) == ce);
    g_assert (g_queue_peek_nth (path, 0) == ce);
    g_assert (g_queue_peek_nth (path, 1) == bc);
    g_assert (g_queue_peek_tail (path) == ab);
  }

  g_queue_free (path);

  exit (EXIT_SUCCESS);
}
