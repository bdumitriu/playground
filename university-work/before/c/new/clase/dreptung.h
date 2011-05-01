#if !defined (__DREPTUNG_H__)
#define __DREPTUNG_H__

#include "punct.h"
#include <iostream.h>

class Dreptunghi : public EntitateGrafica
{
public:
   Dreptunghi(int x1, int y1, int x2, int y2);
   ~Dreptunghi();

   void   Afisare() const;
   double Perimetru() const;
   int    Lung_lat1() const;
   int    Lung_lat2() const;
   double Arie() const;
   int    Sx() const;             // coordonata x a coltului stanga sus
   int    Sy() const;             // coordonata y a coltului stanga sus
   int    Dx() const;             // coordonata x a coltului dreapta jos
   int    Dy() const;             // coordonata y a coltului dreapta jos

   Dreptunghi& operator=(const Dreptunghi& d);
   int         operator==(const EntitateGrafica& d) const;
   int         operator!=(const Dreptunghi& d) const;

   friend ostream& operator<<(ostream& os, const Dreptunghi& d);
   friend istream& operator>>(istream& is, Dreptunghi& d);

private:
   int x1, y1;
};

#endif