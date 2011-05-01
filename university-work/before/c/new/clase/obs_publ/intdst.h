#if !defined(__INTDST_H__)
#define __INTDST_H__

#include "state.h"

class IntDocumentState : public State
{
public:
   IntDocumentState(int i);
   ~IntDocumentState();

   int x;
};

#endif