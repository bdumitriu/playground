#if !defined(__COMPUS_H__)
#define __COMPUS_H__

#include "compon.h"
#include "tpl_tabi.h"

class Compus : public Component
{
public:
   Compus(int y);
   ~Compus();

   void Afisare() const;

private:
   TablouI<Component> el;
   int
};

#endif