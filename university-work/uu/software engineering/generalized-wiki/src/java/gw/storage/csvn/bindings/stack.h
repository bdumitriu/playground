// A simple stack implementation that stores void*
// Used for SVN RA directory traversal

struct stack_elem {
    void* data;
    struct stack_elem* next;
};

void stack_init(struct stack_elem**);
void stack_free(struct stack_elem**);
void* stack_top(struct stack_elem**);
void stack_push(struct stack_elem**, void*);
void* stack_pop(struct stack_elem**);
