/*
 * Implementation of the Node class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   27.12.2001
 */

#include "mtree.h"

Node::Node(int k, Node *p, Node *l, Node *r, Node *s, Node *pr, Node *sp)
: key(k), parent(p), left(l), right(r), succ(s), pred(pr), spParent(sp)
{}

Node::~Node()
{}