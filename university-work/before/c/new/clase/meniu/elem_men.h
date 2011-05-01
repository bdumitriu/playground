#if !defined(__ELEM_MEN_H__)
#define __ELEM_MEN_H__

#include "str.h"
#include <conio.h>

typedef enum {ID_ESC, ID_PREV, ID_NEXT, ID_QUIT, ID_EXEC} Ex_codes;

class ElementMeniu
{
public:                      //int pozitieX, pozitieY;
   ElementMeniu(const String& s, COLORS BkNorm = WHITE, COLORS BkInv = GREEN,
		COLORS FgNorm = BLACK, COLORS FgInv = BLACK);
   virtual ~ElementMeniu();

   virtual Ex_codes Execute(int a) = 0;
   void    Afisare(int mode) const;
   virtual isMeniu() const;
//   const String& GetText() const;

//protected:
   String text;
   COLORS BkNorm, BkInv, FgNorm, FgInv;
};

#endif