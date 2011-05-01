/*
 * Implementation of the Red/Black Node class.
 *
 * Author:  Bogdan DUMITRIU
 * Date:    26.11.2001
 */

#include "rbtree.h"

RBNode::RBNode(int k, RBNode *p, RBNode *l, RBNode *r, color c)
: key(k), parent(p), left(l), right(r), col(c)
{}

RBNode::~RBNode()
{}