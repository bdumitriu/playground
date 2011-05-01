#if !defined(__LINIE_H__)
#define __LINIE_H__

#include "ent_gr.h"
#include <iostream.h>

class Linie : public EntitateGrafica
{
public:
   Linie();
   Linie(int x1, int y1, int x2, int y2);
   ~Linie();

   void   Afisare() const;
   double Perimetru() const;
   double Arie() const;

   int X1() const;
   int Y1() const;
   int X2() const;
   int Y2() const;

   Linie& operator=(const Linie& l);
   int    operator==(const EntitateGrafica& l) const;
   int    operator!=(const Linie& l) const;

   friend ostream& operator<<(ostream& os, const Linie& l);
   friend istream& operator>>(istream& is, Linie& l);

private:
   int x1, y1;

};

#endif