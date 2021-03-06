#include <stdlib.h>
#include <stdio.h>
#include <cirque.h>

typedef struct {
    unsigned int first;
    unsigned int second;
} edge;

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

unsigned int connected_faces(unsigned int *faces, unsigned int i, unsigned int j){
	unsigned int x = 0;
	unsigned int I = 3*i;
	unsigned int J = 3*j;
	for(unsigned int k = 0; k < 3; k++){
		if(faces[J+k] == faces[I] || faces[J+k] == faces[I+1] || faces[J+k] == faces[I+2]){
			x ++;
		}
	}
	return x==2;
}

int* connComps(unsigned int *faces, unsigned int order){
	edge *edges;
  unsigned int n = 0;
  int *components;
  unsigned int c;
  edges = malloc(order * 3 * sizeof(edge));
  if (edges == NULL) {
      exit(1);
  }
  unsigned nconnections;
  printf("Calculate edges ...\n");
  for(unsigned int i=0; i<order; i++){
  	nconnections = 0;
  	for(unsigned int j=i+1; j<order; j++){
  		unsigned int connection = connected_faces(faces, i, j);
  		if(connection){
  			edges[n].first = i;
  			edges[n].second = j;
  			n++;
  			nconnections++;
  			if(nconnections == 3){
  				break;
  			}
  		}
  	}
  }
  printf("done - there are %u edges.\n", n);
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
