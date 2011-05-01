/*
 * Implementation of the Node class.
 *
 * Author: Bogdan DUMITRIU
 * Date:   19.11.2001
 */

#include "tree.h"

Node::Node(int k, Node *p, Node *l, Node *r)
: key(k), parent(p), left(l), right(r)
{}

Node::~Node()
{}