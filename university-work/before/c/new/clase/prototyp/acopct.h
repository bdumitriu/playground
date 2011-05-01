#if !defined(__ACOPCT_H__)
#define __ACOPCT_H__

#include "acoperis.h"

class Acoperisct : public Acoperis
{
public:
   Acoperisct(int nr, int cul, int ah);
   void Afisare() const;
   Acoperis* Clone() const;

private:
   int nr_tig;
};

#endif