#if !defined(__PERCAR_H__)
#define __PERCAR_H__

#include "perete.h"

class Percar : public Perete
{
public:
   Percar(int nr, int cul);
   void Afisare() const;
   Perete* Clone() const;

private:
   int nr_car;
};

#endif