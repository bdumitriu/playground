#if !defined(__IMAGINE_H__)
#define __IMAGINE_H__

#include "ent_gr.h"

class Imagine : public EntitateGrafica
{
public:
   Imagine();
   ~Imagine();

   void Adaug(EntitateGrafica* el);
   void Scot(const EntitateGrafica* el);

   double Arie() const;
   double Perimetru() const;
   void   Afisare() const;
   int    Contine(const EntitateGrafica* e) const;

   int    operator==(const EntitateGrafica& e) const;

private:
   EntitateGrafica** img;
   int idx;

};

#endif
