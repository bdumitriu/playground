#if !defined(__INTVIEW3_H__)
#define __INTVIEW3_H__

#include "observer.h"
#include "intdoc.h"

class IntView3 : public Observer
{
public:
   IntView3(int x, int y);
   ~IntView3();

   void Notify();
   void SetScreen();
   int  GetKey();

   int operator==(const Observer& ob) const;

private:
   int cx, cy; // coltul x, coltul y
   int x;
};

#endif