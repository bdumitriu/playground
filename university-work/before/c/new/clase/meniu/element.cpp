#include "element.h"

Element::Element(Command* com, const String& s, COLORS BkNorm,
		 COLORS BkInv, COLORS FgNorm, COLORS FgInv)
   : ElementMeniu(s, BkNorm, BkInv, FgNorm, FgInv)
{
   c = com;
}

Element::~Element()
{
   delete c;
}

Ex_codes Element::Execute(int a)
{
   c->DoCommand();
   return ID_EXEC;
}

int Element::isMeniu() const
{
   return 0;
}