#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>

// GC related
typedef struct {
	uint32_t size;
	void* pointer;
} GCObject;

typedef struct GCGraph {
	uint32_t id;
	struct GCGraph* child;
	uint32_t size;
} GCGraph;

// void* heap;
GCObject* objects;
uint32_t  objects_count;
GCGraph   graph;
bool* walked;

void walk(GCGraph graph) {
	for (uint32_t i = 0; i < graph.size; i++) {
		if (graph.child[i].id != 0) {
			if (walked[graph.child[i].id] == false) {
				walked[graph.child[i].id] = true;
				walk(graph.child[i]);
			}
		}
	}
}

void gc() {
	clock_t start = clock();
	// mark
	memset(walked, false, sizeof(bool)*objects_count);
	walk(graph);

	// sweep
	for (uint32_t i = 0; i < objects_count; i++) {
		if (walked[i+1] == false) {
			if (objects[i].pointer != NULL) {
				printf("Freed %d bytes on ID %d\n", objects[i].size, i+1);
				free(objects[i].pointer);
				objects[i].pointer = NULL;
			}
		}
	}

	clock_t end = clock();
	printf("GC Time: %.5f\n", (float) (end-start) / (float) (CLOCKS_PER_SEC));
}

void init() {
	// heap = malloc(0x1000);
	objects_count = 0x1000;
	objects = calloc(objects_count, sizeof(GCObject));

	graph.id    = 0;
	graph.size  = 0x100;
	graph.child = calloc(graph.size, sizeof(GCGraph));

	walked = calloc(objects_count, sizeof(bool));

	printf("Initilized GC\n");
}

uint32_t id;
void* new(uint32_t size) {
	for (uint32_t i = 0; i < objects_count; i++) {
		if (objects[i].pointer == NULL) {
			objects[i].size = size;
			objects[i].pointer = malloc(size);
			id = i+1;
			return objects[i].pointer;
		}
	}

	objects = realloc(objects, sizeof(GCObject) * ++objects_count);
	objects[objects_count-1].size = size;
	objects[objects_count-1].pointer = malloc(size);
	id = objects_count;
	return objects[objects_count-1].pointer;
}

GCGraph* graph_append(GCGraph* g, uint32_t id) {
	for (uint32_t i = 0; i < g->size; i++) {
		if (g->child[i].child == NULL && g->child[i].id == 0) {
			g->child[i].id = id;
			g->child[i].child = NULL;
			g->child[i].size = 0;
			return &g->child[i];
		}
	}

	g->child = realloc(g->child, sizeof(GCGraph) * g->size+1);
	g->child[g->size].id = id;
	g->child[g->size].child = NULL;
	g->child[g->size].size = 0;
	g->size++;
	return &g->child[g->size-1];
}

void graph_free(GCGraph* g) {
	for (uint32_t i = 0; i < g->size; i++) {
		if (g->child != NULL) graph_free(&g->child[i]);
	}
	free(g->child);
	g->child = NULL;
	g->id = 0;
}

int main() {
	init();

	int* ptr_1 = new(sizeof(int));
	uint32_t id_1 = id;
	int* ptr_2 = new(sizeof(int));
	uint32_t id_2 = id;
	int** ptr_3 = new(sizeof(int*));
	uint32_t id_3 = id;

	*ptr_1 = 69;
	*ptr_2 = 420;
	*ptr_3 = ptr_2;

	// Root -> ID 3 -> ID 2

	GCGraph* graph_3 = graph_append(&graph, id_3);
	GCGraph* graph_2 = graph_append(graph_3, id_2);

	gc();

	graph_free(graph_3);

	gc();
}
