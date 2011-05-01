#include "elem_men.h"

ElementMeniu::ElementMeniu(const String& s, COLORS BkNorm, COLORS BkInv,
			   COLORS FgNorm, COLORS FgInv)
   : text(s)
{
   this->BkNorm = BkNorm;
   this->BkInv = BkInv;
   this->FgNorm = FgNorm;
   this->FgInv = FgInv;
}

ElementMeniu::~ElementMeniu()
{}

void ElementMeniu::Afisare(int mode) const
{
   if (mode == 0)
   {
      textcolor(FgNorm);
      textbackground(BkNorm);
   }
   else
   {
      textcolor(FgInv);
      textbackground(BkInv);
   }
   cprintf(" %s ", text);
}

int ElementMeniu::isMeniu() const
{
   return 0;
}