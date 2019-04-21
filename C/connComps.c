#include <stdlib.h>

typedef struct {
    unsigned int first;
    unsigned int second;
} edge;

void connected_components_recursive(const edge *edges, unsigned int n,
        int *components, unsigned int order, unsigned int vertex,
        unsigned int component)
{
    unsigned int i;
    /* Put this vertex in the current component */
    components[vertex] = component;
    for (i = 0; i < n; i++) {
        if (edges[i].first == vertex || edges[i].second == vertex) {
            /* Adjacent */
            const unsigned int neighbour = edges[i].first == vertex ?
                    edges[i].second : edges[i].first;
            if (components[neighbour] == -1) {
                /* Not yet visited */
                connected_components_recursive(edges, n, components, order, neighbour, component);
            }
        }
    }
}

unsigned int connected_components(const edge *edges, unsigned int n, unsigned int order,
        int **components)
{
    unsigned int i;
    unsigned int component = 0;
    *components = malloc(order * sizeof(int));
    if (components == NULL) {
        return 0;
    }
    for (i = 0; i < order; i++) {
        (*components)[i] = -1;
    }

    for (i = 0; i < order; i++) {
        if ((*components)[i] == -1) {
            connected_components_recursive(edges, n, *components, order, i, component);
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
  c = connected_components(edges, n, order, &components);
  if(components == NULL) {
      free(edges);
      exit(1);
  }
  free(edges);
  return components;
}
