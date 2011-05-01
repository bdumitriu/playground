#include <stdlib.h>
#include <string.h>
#include <g++/iostream.h>

struct foo
{
	int x;
	int y;
};

int main()
{
	struct foo *fooPointer;
	fooPointer = (struct foo *) malloc(sizeof(struct foo));
	if (fooPointer == NULL)
	{
		abort();
	}
	fooPointer->x = 10;
	fooPointer->y = 20;
	memset(fooPointer, 0, sizeof(struct foo));
	cout << fooPointer->x << " " << fooPointer->y << "\n";
	cout << fooPointer << "\n";
	free(fooPointer);
	cout << fooPointer->x << " " << fooPointer->y << "\n";
	cout << fooPointer << "\n";
}
