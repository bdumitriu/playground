
/*
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
*/
