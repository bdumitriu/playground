#include "str.h"
#include <string.h>
#include <conio.h>

String::String()
{}

String::String(int dim, char c)
{
	str = new char [dim+1];
	for (int i = 0; i < dim; i++)
		str[i] = c;
	str[dim] = '\0';
}

String::String(char* c)
{
	str = new char [strlen(c)];
	strcpy(str, c);
	str[strlen(c)] = '\0';
}

String::String(String& s)
{
	str = new char [strlen(s.str)+1];
	strcpy(str, s.str);
	str[strlen(s.str)] = '\0';
}

String::~String()
{
	delete [] str;
}

void String::Print(int s_idx, int f_idx)
{
	for (int i = s_idx; i <= f_idx; i++)
		cprintf("%c", str[i]);
}

int String::Len()
{
	return strlen(str);
}

void String::SetDim(int dim)
{
	str = new char [255];
	str[255] = '\0';
}

void String::SetStringToChar(char c)
{
	int x = strlen(str);
	char *pc;

  /*	for (int i = 0; i < x; i++)
	{
		pc = str;
		for (int j = 0; j < i; j++)
			pc++;
		*pc = c;
	}*/
/*	for (int i = 0; i < x; i++)
		str[i] = c;*/
	strset(str, c);
	str[x] = '\0';
}