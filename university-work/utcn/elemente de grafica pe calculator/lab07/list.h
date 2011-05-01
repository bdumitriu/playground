/*
 * Header file for list manipulation of Figures.
 *
 * Author: Bogdan DUMITRIU
 * Date:   01.12.2001
 */

#include "figures.h"

#ifndef __LIST_H_
#define __LIST_H_

/*
 * This class represents the list's nodes.
 */
class Node
{
public:
	/*
	 * Builds a new node containing the specified figure.
	 */
	Node(Figure *data)
	: data(data), next(NULL)
	{}
	~Node()
	{
		delete data;
	}

	/*
	 * Sets the node's next field to next.
	 */
	void setNext(Node *next)
	{
		this->next = next;
	}
	/*
	 * Gets the node following the current one.
	 */
	Node* getNext()
	{
		return next;
	}
	/*
	 * Sets the node's data field to data.
	 */
	void setData(Figure *data)
	{
		this->data = data;
	}
	/*
	 * Gets the node's data.
	 */
	Figure* getData()
	{
    	return data;
	}
private:
	Node *next;
	Figure *data;
};

/*
 *  This class implements a simple linked list.
 */
class List
{
public:
	/*
	 * Builds a new empty list.
	 */
	List()
	: head(NULL), tail(NULL)
	{}
	~List();

	/*
	 * Adds the element at the beginning of the list.
	 * The element should have its data field properly set.
	 */
	void add(Node *element);
	/*
	 * Deletes the element pointed by element from the list.
	 * It does not free the memory occupied by node. This should
	 * be done by the user afterwards if so desired.
	 */
	void remove(Node *element);
	/*
	 * Deletes all elements from the list. It also frees the
	 * memory occupied by all nodes.
	 */
	void removeAll();
	/*
	 * Finds the first element in the list that returns 1
	 * when its contains(x, y) method is called and selects/
	 * deselects it if it is not/it is selected/deselected.
	 * It such an element is found a pointer to it is
	 * returned. If no such element is found NULL is returned.
	 */
	Node* changeStatus(int x, int y);
	/*
	 * Calls move(x, y) for all elements in the list that
	 * return 1 when their getSelected() method is called.
	 */
	void moveAllSelected(int x, int y);
	/*
	 * Deselects all elements from the list.
	 */
	void deselectAll();
	/*
	 * Removes all elements in the list that return 1 when
	 * their getSelected() method is called. It also frees the
	 * memory occupied by these nodes.
	 */
	void removeAllSelected();
	/*
	 * Calls the draw method for all the elements.
	 */
	void drawAll();
	/*
	 * Calls the drawBlack method for all the elements.
	 */
	void drawAllBlack();
	/*
	 * Calls the writeToFile method for all the elements.
	 */
	void writeAll(FILE *f);
private:
	Node *head;
	Node *tail;
};

#endif