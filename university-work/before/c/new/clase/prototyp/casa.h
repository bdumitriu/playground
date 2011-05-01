#if !defined(__CASA_H__)
#define __CASA_H__

#include "acoperis.h"
#include "perete.h"

class Casa
{
public:
   Casa(const Acoperis& a, const Perete& p);
   ~Casa();

   void Afisare() const;

private:
   Acoperis* ac;
   Perete*   per;

};

#endif