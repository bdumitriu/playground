#include "singleto.h"
#include <stdio.h>

Singleton* Singleton::instance = 0;

Singleton::Singleton(int x)
{
   this->x = x;
}

Singleton* Singleton::GetInstance()
{
   if (instance == NULL)
      instance = new Singleton(5);

   return instance;
}

void Singleton::Afisare()
{
   printf("%d", x);
}