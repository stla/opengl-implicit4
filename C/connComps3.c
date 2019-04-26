#include <stdlib.h>
#include <stdio.h>
#include <cirque.h>
#include <connComps.h>

static void connected_components_internal(const edge *edges, unsigned int n,
        int *components, unsigned int order, unsigned int vertex,
        unsigned int component)
{
    unsigned int i;
    /* Put this vertex in the current component */
    components[vertex] = component;
    cirque *queue = cirque_create();
    if (!queue) {
        cirque_delete(queue);
        exit(1);
    }
    cirque_insert(queue, &vertex);
    while (cirque_get_count(queue)) {
        unsigned int e;
        unsigned int *current = cirque_remove(queue);
        for (e = 0; e < n; e++) {
            if (edges[e].first == *current || edges[e].second == *current) {
                const unsigned int *neighbour = edges[e].first == *current ?
                    &edges[e].second : &edges[e].first;
                if (components[*neighbour] == -1) {
                    components[*neighbour] = component;
                    cirque_insert(queue, (void*)neighbour);
                }
            }
        }
    }
    cirque_delete(queue);
}

unsigned int connected_components(const edge *edges, unsigned int n, unsigned int order,
   int **components)
{
    unsigned int i;
    unsigned int component = 0;
    *components = malloc(order * sizeof(int));
    if (components == NULL) {
        exit(1);
    }
    for (i = 0; i < order; i++) {
        (*components)[i] = -1;
    }

    for (i = 0; i < order; i++) {
        if ((*components)[i] == -1) {
            connected_components_internal(edges, n, *components, order, i, component);
            component++;
        }
    }
    return component;
}

unsigned int min(unsigned int a, unsigned int b){
	return a < b ? a : b;
}

unsigned int max(unsigned int a, unsigned int b){
	return a < b ? b : a;
}

unsigned int disjointFaces(
	unsigned int f0, unsigned int f1, unsigned int f2,
	unsigned int g0, unsigned int g1, unsigned int g2){
	return max(max(f0,f1),f2) <= min(min(g0,g1),g2);
}

unsigned int connected_faces(
	unsigned int f0, unsigned int f1, unsigned int f2,
	unsigned int g0, unsigned int g1, unsigned int g2){
	unsigned int x = 0;
	if(f0 == g0 || f0 == g1 || f0 == g2){
		x++;
	}
	if(f1 == g0 || f1 == g1 || f1 == g2){
		x++;
	}
	if(f2 == g0 || f2 == g1 || f2 == g2){
		x++;
	}
	return x == 2;
}

edge* makeEdges(unsigned int *faces, unsigned int order, unsigned int *n){
  edge* edges = malloc(order * 3 * sizeof(edge));
  if (edges == NULL) {
      exit(1);
  }
  *n = 0;
  printf("Calculate edges ...\n");
  for(unsigned int i=0; i<order; i++){
    unsigned int nconnections = 0;
    unsigned int I = 3*i;
    unsigned int f0 = faces[I];
    unsigned int f1 = faces[I+1];
    unsigned int f2 = faces[I+2];
    for(unsigned int j=i+1; j<order; j++){
      unsigned int J = 3*j;
      unsigned int g0 = faces[J];
      unsigned int g1 = faces[J+1];
      unsigned int g2 = faces[J+2];
      unsigned int disjoint = disjointFaces(f0, f1, f2, g0, g1, g2);
      if(disjoint){
        break;
      }
      unsigned int connection = connected_faces(f0, f1, f2, g0, g1, g2);
      if(connection){
        edges[*n].first = i;
        edges[*n].second = j;
        (*n)++;
        nconnections++;
        if(nconnections == 3){
          break;
        }
      }
    }
  }
  printf("done - there are %u edges.\n", *n);
  return edges;
}

int* connComps(unsigned int *faces, unsigned int order){
	// edge *edges;
  // unsigned int n = 0;

  unsigned int n;
  edge* edges = makeEdges(faces, order, &n);

  int *components;
  unsigned int c;
  // edges = malloc(order * 3 * sizeof(edge));
  // if (edges == NULL) {
  //     exit(1);
  // }
  // printf("Calculate edges ...\n");
  // for(unsigned int i=0; i<order; i++){
  //   unsigned int nconnections = 0;
  // 	unsigned int I = 3*i;
  // 	unsigned int f0 = faces[I];
  // 	unsigned int f1 = faces[I+1];
  // 	unsigned int f2 = faces[I+2];
  // 	for(unsigned int j=i+1; j<order; j++){
  // 		unsigned int J = 3*j;
  // 		unsigned int g0 = faces[J];
  // 		unsigned int g1 = faces[J+1];
  // 		unsigned int g2 = faces[J+2];
  // 		unsigned int disjoint = disjointFaces(f0, f1, f2, g0, g1, g2);
  // 		if(disjoint){
  // 			break;
  // 		}
  // 		unsigned int connection = connected_faces(f0, f1, f2, g0, g1, g2);
  // 		if(connection){
  // 			edges[n].first = i;
  // 			edges[n].second = j;
  // 			n++;
  //       nconnections++;
  //       if(nconnections == 3){
  //         break;
  //       }
  // 		}
  // 	}
  // }
  // printf("done - there are %u edges.\n", n);
  printf("Calculate connected components...\n");
  c = connected_components(edges, n, order, &components);
  if(components == NULL) {
      free(edges);
      exit(1);
  }
  free(edges);
  printf("There are %u components.\n", c);
  return components;
}
