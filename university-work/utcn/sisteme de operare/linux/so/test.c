#include <iostream.h>
#include <string.h>

int main(void)
{
	for (int i = 0; i < 100000; i++)
	{
        	char* c =  new char[100000];
		delete [] c;
	}
}