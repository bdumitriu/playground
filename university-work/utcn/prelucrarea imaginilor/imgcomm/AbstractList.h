
/**
 * Usefull class performing list manipulations.
 */
 
#ifndef _ABSTRACT_LIST_H_
#define _ABSTRACT_LIST_H_

#include "debug.h"

/**
 * The list element item. <br /> <br />
 *
 * Author Tudor Marian <br />
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 *
 * @author  Tudor Marian
 * created late april
 */
template <class T>
class ListElement
{
public:
	ListElement(T item);
	ListElement* next;
	ListElement* prev;
	T item;
};

/**
* Forward declaration of the <code>ListIterator</code> class.
*/
template <class T>
class ListIterator;

/**
 * The following defines a simple linked list of template
 * elements. It will not care weather two identical elements
 * are inserted. <br /><br />
 *
 * Author  Tudor Marian <br />
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 *
 * @author  Tudor Marian
 * created late april
 */
template <class T>
class List
{
public:
	/**
	* Creates a list object.
	*/
	List();
	/**
	* Destroys the list object.
	*/
	virtual ~List();

	/**
	* Add an element at the begining of the list.
	*
	* @param	item	the item to be added.
	*/
	void addFront(T item);

	/**
	* The method adds the element <code>item</code> at the position
	* indicated by <code>position</code>. It must be >= than 0 and <
	* the size of the list.
	*
	* @param	position	the place in the list where the new item is
	*	to be added.
	* @param	item	the item to be added into the list.
	* @return	false if the bounds of the list are exceeded, true otherwise.
	*/
	bool add(int position, T item);

	/**
	* Add an element at the end of the list.
	*
	* @param	item	the item to be added.
	*/
	void addTail(T item);

	/**
	* Removes the first element from the list.
	*
	* @return the removed element.
	*/
	T removeFront();

	/**
	* Removes the last element from the list.
	*
	* @return	the removed element.
	*/
	T removeTail();

	/**
	* Removes from the list the element found at <code>index</code> in the
	* list.
	*
	* @param	index	the index of the element to be deleted.
	* @return	the deleted element if bounds restrictions are correct, null
	*	otherwise.
	*/
	T removeElementAt(int index);

	/**
	* Removes from the list the first occurance of the element <code>item</code>
	* if any is found.
	*
	* @param	item	the element to be removed from the list.
	*/
	void remove(T item);

	/**
	* Returns the element found at position <code>index</code> provided that the
	* index is valid.
	*
	* @return	the element requsted.
	*/
	T elementAt(int index);

	/**
	* Returns the element found at position <code>index</code> provided that the
	* index is valid.
	*
	* @return	the element requsted.
	*/
	T get(int index);

	/**
	* Returns the index in the list of the first occurance of the <code>element</code>
	* item found in list, -1 otherwise.
	*
	* @param	element	the element seeked.
	* @return	the index of the element, or -1 if no element is found.
	*/
	int indexOf(T element);

	/**
	* Checks to see if the list contains the element <code>item</code>.
	*
	* @param	item	the element searched in the list.
	* @return	true if such an element is found, false otherwise.
	*/
	bool contains(T item);
		
	/**
	* Returns the size of the list.
	*
	* @return	the size of the list.
	*/
	int getSize() {return this->size;};

	/**
	* Checks to see if the list is empty or not.
	*
	* @return	true if the list is empty, false otherwise.
	*/
	bool empty() {return (size == 0);};

private:

	/** The size of the list.
	*/
	int size;

	/** The first element of the list.
	*/
	ListElement<T>* head; 

	/** The last element of the list.
	*/
	ListElement<T>* tail;

	/** Just a friend class.
	*/
	friend class ListIterator<T>;
};

/**
 * The following is the list iterator - much more efficient than the
 * get(index) or indexOf(index) methods. <br /><br />
 *
 * Author  Tudor Marian <br />
 * Technical University Of Cluj-Napoca <br />
 * Computer Science Departament <br />
 * gr. 3241/1 <br />
 *
 * @author  Tudor Marian
 * created late april
 */
template <class T>
class ListIterator
{
public:
	
	/**
	* Creates a <code>ListIterator</code> object.
	*
	* @param	list	the list to iterate through.
	*/
	ListIterator(List<T>* list) {current = list->head;};

	/**
	* Checks to see if the iterator has any more elements to iterate.
	*
	* @return	true if there are any more elements to iterate through,
	*	false otherwise.
	*/
	bool hasNext() {return (current != NULL);};

	/**
	* The method returns the next element from the iteration process.
	*
	* @return	the next iterated element.
	*/
	T next();

private:
	/** The current position of the iterator in the list.
	*/
	ListElement<T>* current;
};


//#include "AbstractListImpl.cpp" // daca faci makefile-ul de mana, altfel...

template <class T>
ListElement<T>::ListElement(T item)
{
	this->item = item;
	next = NULL;
	prev = NULL;
}



template <class T>
List<T>::List()
{
	size = 0;
	head = NULL;
	tail = NULL;
}

template <class T>
List<T>::~List()
{
}

template <class T>
void List<T>::addFront(T item)
{
	ListElement<T>* element = new ListElement<T>(item);

	if (empty()) // if the list is empty
	{
		head = element;
		tail = element;
	}
	else
	{

		element->next = head;
		head->prev = element;

		head = element;
	}
	size ++;
}

template <class T>
void List<T>::addTail(T item)
{
	ListElement<T>* element = new ListElement<T>(item);

	if (empty())
	{
		head = element;
		tail = element;
	}
	else
	{
		tail->next = element;
		element->prev = tail;

		tail = element;
	}

	size ++;
}

template <class T>
bool List<T>::add(int position, T item)
{
	if (position > size || position < 0)
		return false;

	if (position == size)
	{
		addTail(item);
	}
	else if (position == 0)
	{
		addFront(item);
	}
	else
	{
		ListElement<T>* newElement = new ListElement<T>(item);
		int currentPosition = 0;
		ListElement<T>* element = head;

		while (currentPosition < position)
		{
			currentPosition ++;
			element = element->next;
		}

		// now i'm positioned on the one i'm supposed to replace
		element->prev->next = newElement;
		newElement->prev = element->prev;

		newElement->next = element;
		element->prev = newElement;
	}

	size ++;
	return true;
}

template <class T>
T List<T>::removeFront()
{
	ListElement<T>* p = head;

	T item = head->item;
	head = head->next;

	// destroy the references
	head->prev = NULL;
	delete p;

	size --;
	printf("sz = %d\n", this->size);

	return item;
}

template <class T>
T List<T>::removeTail()
{
	ListElement<T>* p = tail;

	T item = tail->item;
	tail = tail->prev;

	// destroy all references
	tail->next = NULL;
	delete p;

	size --;

	return item;
}

template <class T>
T List<T>::removeElementAt(int index)
{
	if (index == 0)
		return removeFront();
	else if (index == size-1)
		return removeTail();
	else
	{
		int position = 0;
		ListElement<T>* element = head;

		while (position < index)
		{
			position ++;
			element = element->next;
		}

		element->prev->next = element->next;
		element->next->prev = element->prev;

		return element->item;
	}
}

// warning, == operator must be overloaded for proper usage
template <class T>
void List<T>::remove(T item)
{
	ListElement<T>* element = head;

	bool found = false;

	while ((! found) && (element != NULL))
	{
		if (element->item == item) // operator == must be overriden
			found = true;
		else
			element = element->next;
	}

	if (!found)
		return;

	if (element == head)
		removeFront();
	else if (element == tail)
		removeTail();
	else
	{
		element->prev->next = element->next;
		element->next->prev = element->prev;
	}
}

template <class T>
T List<T>::elementAt(int index)
{
	ASSERT (index >= 0 && index < size);

	if (index == 0)
		return head->item;

	if (index == size-1)
		return tail->item;

	int currentPosition = 0;
	ListElement<T>* element = head;

	while (currentPosition < index)
	{
		element = element->next;
		currentPosition ++;
	}

	return element->item;
}

template <class T>
T List<T>::get(int index)
{
	return elementAt(index);
}

// returns -1 if no element was found
template <class T>
int List<T>::indexOf(T item)
{
	ListElement<T>* element = head;

	bool found = false;
	int index = 0;

	while (element != NULL && (!found))
	{
		if (element->item == item) // == operator must be overriden
			found = true;
		else
			index ++;
		element = element->next;
	}

	if (!(found))
		return -1;
	else
		return index;
}

template <class T>
bool List<T>::contains(T item)
{
	if (indexOf(item) == -1)
		return false;
	return true;
}

template <class T>
T ListIterator<T>::next()
{
	T element = current->item;
	current = current->next;

	return element;
}

#endif //_ABSTRACT_LIST_H_
