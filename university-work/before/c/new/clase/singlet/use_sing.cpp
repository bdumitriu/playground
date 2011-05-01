#include "singleto.h"
#include <conio.h>

void main()
{
	Singleton *s1, *s2;
   s1 = Singleton::GetInstance();
   s2 = Singleton::GetInstance();

   s1->Afisare();
   s2->Afisare();
   getch();
}