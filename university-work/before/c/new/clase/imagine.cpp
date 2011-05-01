#include "imagine.h"

Imagine::Imagine()
{
   img = (EntitateGrafica**)(new (EntitateGrafica**) [100]);
   idx = 0;
}

Imagine::~Imagine()
{
   for (int i = 0; i < idx; i++)
      delete img[i];
   delete [] img;
}

void Imagine::Adaug(EntitateGrafica* el)
{
   img[idx++] = el;
}

double Imagine::Arie() const
{
   double sum_a = 0;

   for (int i = 0; i < idx; i++)
   {
      sum_a+= img[i]->Arie();
   }

   return sum_a;
}

double Imagine::Perimetru() const
{
   double per_a = 0;

   for (int i = 0; i < idx; i++)
   {
      per_a+= img[i]->Perimetru();
   }

   return per_a;
}

void Imagine::Afisare() const
{
   for (int i = 0; i < idx; i++)
   {
      img[i]->Afisare();
   }
}

int Imagine::Contine(const EntitateGrafica* e) const
{
   for (int i = 0; i < idx; i++)
   {
      if ((*img[i]) == (*e))
	 return 1;
   }

   return 0;
}

int Imagine::operator==(const EntitateGrafica& e) const
{
   return 0;
}