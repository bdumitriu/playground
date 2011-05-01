#if !defined(__PUNCT_H__)
#define __PUNCT_H__

#include "ent_gr.h"
#include <iostream.h>

class Punct : public EntitateGrafica
{
public:
   Punct();
   Punct(int x, int y);
// Punct(const Punct& p);
   ~Punct();

   void   Afisare() const;
   double Arie() const;
   double Perimetru() const;

   int    operator==(const EntitateGrafica& p) const;
   int    operator!=(const Punct& p) const;
   Punct& operator=(const Punct& p);

   friend ostream& operator<<(ostream& os, const Punct& p);
   friend istream& operator>>(istream& is, Punct& p);

};

#endif