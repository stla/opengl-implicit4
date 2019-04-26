#ifndef CIRQUE_H
#define CIRQUE_H

struct cirque {
    unsigned int head; /* First element */
    unsigned int tail; /* 1 past the last element */
    unsigned int is_full;
    void ** entries;
    unsigned int size;
};
typedef struct cirque cirque;

typedef void (*cirque_forfn)(void*);

cirque * cirque_create(void);
void cirque_delete(cirque * queue);
unsigned int cirque_insert(cirque * queue, void * data);
void *  cirque_remove(cirque * queue);
void *cirque_peek(const cirque * queue);
unsigned int cirque_get_count(const cirque * queue);
void cirque_for_each(const cirque * queue, cirque_forfn fun);

#endif /* CIRQUE_H */
