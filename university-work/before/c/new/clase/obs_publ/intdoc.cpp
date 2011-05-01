#include "intdoc.h"
#include "intdst.h"

IntDocument::IntDocument(int i)
{
   x = i;
}

IntDocument::~IntDocument()
{}

/*
State* IntDocument::GetState() const
{
   return (State*)(new IntDocumentState(x));
}
*/

int IntDocument::GetState() const
{
   return x;
}

void IntDocument::SetVal(int i)
{
   x = i;
   NotifyObservers();
}