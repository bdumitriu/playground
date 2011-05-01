#if !defined(__CERC_H__)
#define __CERC_H_

#include <iostream.h>
#include "ent_gr.h"

class Cerc : public EntitateGrafica
{
public:
   Cerc(int x, int y, int raza);
   ~Cerc();

   void   Afisare() const;
   double Arie() const;
   double Perimetru() const;
   int    Raza() const;
   int    Cx() const;           // coordonata x a centrului
   int    Cy() const;           // coordonata y a centrului

   Cerc& operator=(const Cerc& c);
   int   operator==(const EntitateGrafica& c) const;
   int   operator!=(const Cerc& c) const;

   friend ostream& operator<<(ostream& os, const Cerc& c);
   friend istream& operator>>(istream& is, Cerc& c);

private:
   int r;
};

#endif