#include <stdio.h>
#include <stdlib.h>

#include "stack.h"

void stack_init(struct stack_elem** top_elem) {
    *top_elem = 0;
}

void stack_free(struct stack_elem** top_elem) {
    while(*top_elem) {
        stack_pop(top_elem);
    }
}

void* stack_top(struct stack_elem** top_elem) {
    if (*top_elem) {
        return (*top_elem)->data;
    }

    return 0;
}

void stack_push(struct stack_elem** top_elem, void* data) {
    struct stack_elem* new_elem = (struct stack_elem*) malloc(sizeof(struct stack_elem));
    new_elem->data = data;
    new_elem->next = *top_elem;
    *top_elem = new_elem;
}

void* stack_pop(struct stack_elem** top_elem) {
    if (!(*top_elem)) {
        return 0;
    }
    struct stack_elem* old_elem = *top_elem; // make temp copy of the old top
    *top_elem = old_elem->next;              // switch to the next element as top
    void* data = old_elem->data;
    free(old_elem);                           // release memory for the old top

    return data;
}
