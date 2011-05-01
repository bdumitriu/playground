#include <iostream.h>
#include "include/exceptions/ArrayIndexOutOfBoundsException.h"
#include "include/driver/DirectoryEntry.h"

int main(void)
{
	DirectoryEntry *de = new DirectoryEntry();
	
	cout << de->getEntryName();
	cout << endl;
	cout << de->getEntrySize();
	cout << endl;
	cout << de->getEntryType();
	cout << endl;
	
	de->setEntryName("fhrejhetkjghwhe");
	cout << de->getEntryName();
	cout << endl;
}
