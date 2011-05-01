/*
 * Implementation of list manipulation of Figures.
 *
 * Author: Bogdan DUMITRIU
 * Date:   01.12.2001
 */

#include "list.h"

void List::add(Node *element)
{
	element->setNext(NULL);
	if (head == NULL)
		head = element;
	if (tail != NULL)
		tail->setNext(element);
	tail = element;
}

void List::remove(Node *element)
{
	Node *p = head, *q = NULL;
	while ((p != element) && (p != NULL))
	{
		q = p;
		p = p->getNext();
	}
	if (p == NULL)
		return;
	if (p == tail)
		tail = q;
	if (p == head)
		head = p->getNext();
	if (q != NULL)
		q->setNext(p->getNext());
}

void List::removeAll()
{
	Node *p = head, *q = NULL;
	while (p != NULL)
	{
		q = p;
		p = p->getNext();
		delete q;
	}
	head = tail = NULL;
}

List::~List()
{
	removeAll();
}

Node* List::changeStatus(int x, int y)
{
	Node *p = head;
	while (p != NULL)
	{
		Figure *f = p->getData();
		if (f->contains(x, y))
		{
			if (f->getSelected())
				f->deselect();
			else
				f->select();
			return p;
		}
		p = p->getNext();
	}
	return p;
}

void List::moveAllSelected(int x, int y)
{
	Node *p = head;
	while (p != NULL)
	{
		Figure *f = p->getData();
		if (f->getSelected())
			f->move(x, y);
		p = p->getNext();
	}
}

void List::deselectAll()
{
	Node *p = head;
	while (p != NULL)
	{
		p->getData()->deselect();
		p = p->getNext();
	}
}

void List::removeAllSelected()
{
	Node *p = head, *q;
	while (p != NULL)
	{
		if (p->getData()->getSelected())
		{
			remove(p);
			q = p;
			p = p->getNext();
			delete q;
		}
		else
			p = p->getNext();
	}
}

void List::drawAll()
{
	Node *p = head;
	while (p != NULL)
	{
		p->getData()->draw();
		p = p->getNext();
	}
}

void List::drawAllBlack()
{
	Node *p = head;
	while (p != NULL)
	{
		p->getData()->drawBlack();
		p = p->getNext();
	}
}

void List::writeAll(FILE *f)
{
	Node *p = head;
	while (p != NULL)
	{
		p->getData()->writeToFile(f);
		p = p->getNext();
	}
}