#include <stdlib.h>
#include <cirque.h>

cirque * cirque_create(void)
{
    const unsigned int size = 4;
    cirque * queue = malloc(sizeof(cirque));
    if (queue) {
        queue->entries = malloc(size * sizeof(void *));
        if (queue->entries) {
            queue->size = size;
            queue->head = 0;
            queue->tail = 0;
            queue->is_full = 0;
        }
        else {
            free(queue);
            queue = NULL;
        }
    }
    return queue;
}

void cirque_delete(cirque * queue)
{
    if (queue) {
        free(queue->entries);
        free(queue);
    }
}

static void cirque_resize(cirque * queue)
{
    void **temp = malloc(queue->size * 2 * sizeof(void *));
    if (temp) {
        unsigned int i = 0;
        unsigned int h = queue->head;
        do {
            temp[i] = queue->entries[h++];
            if (h == queue->size) {
                h = 0;
            }
            i++;
        } while (h != queue->tail);
        free(queue->entries);
        queue->entries = temp;
        queue->head = 0;
        queue->tail = queue->size;
        queue->size *= 2;
        queue->is_full = 0;
    }
}

static unsigned int cirque_is_empty(const cirque * queue)
{
    return (queue->head == queue->tail) && !queue->is_full;
}

unsigned int cirque_insert(cirque * queue, void * data)
{
    unsigned int result;
    if (queue->is_full) {
        cirque_resize(queue);
        if (queue->is_full) {
            result = 0;
        }
    }
    if (!queue->is_full) {
        queue->entries[queue->tail++] = data;
        if (queue->tail == queue->size) {
            queue->tail = 0;
        }
        if (queue->tail == queue->head) {
            queue->is_full = 1;
        }
        result = 1;
    }
    return result;
}

void * cirque_remove(cirque * queue)
{
    void * data = NULL;
    if (!cirque_is_empty(queue)) {
        if (queue->is_full) {
            queue->is_full = 0;
        }
        data = queue->entries[queue->head++];
        if (queue->head == queue->size) {
            queue->head = 0;
        }
    }
    return data;
}

void *cirque_peek(const cirque * queue)
{
    void *data = NULL;
    if (!cirque_is_empty(queue)) {
        data = queue->entries[queue->head];
    }
    return data;
}

unsigned int cirque_get_count(const cirque * queue)
{
    unsigned int count;
    if (cirque_is_empty(queue)) {
        count = 0;
    }
    else if (queue->is_full) {
        count = queue->size;
    }
    else if (queue->tail > queue->head) {
        count = queue->tail - queue->head;
    }
    else {
        count = queue->size - queue->head;
        if (queue->tail > 0) {
            count += queue->tail - 1;
        }
    }
    return count;
}

void cirque_for_each(const cirque * queue, cirque_forfn fun)
{
    if (!cirque_is_empty(queue)) {
        unsigned int h = queue->head;
        do {
            fun(queue->entries[h++]);
            if (h == queue->size) {
                h = 0;
            }
        } while (h != queue->tail);
    }
}
