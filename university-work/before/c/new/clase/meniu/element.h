#if !defined(__ELEMENT_H__)
#define __ELEMENT_H__

#include "elem_men.h"
#include "command.h"

class Element : public ElementMeniu
{
public:
   Element(Command* com, const String& s, COLORS BkNorm = WHITE,
	   COLORS BkInv = GREEN, COLORS FgNorm = BLACK, COLORS FgInv = BLACK);
   ~Element();

   Ex_codes Execute(int a);
   int      isMeniu() const;
private:
   Command* c;
   void* farg;
};

#endif