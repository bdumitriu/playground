#include "publish.h"
#include "alloc.h"

Publisher::Publisher()
{
   ObsList = (Observer**)(new (Observer*) [25]);
   idx = 0;
}

Publisher::~Publisher()
{
   delete [] (Observer**)ObsList;
}

void Publisher::Attach(Observer& ob)
{
   ObsList[idx++] = &ob;
   ob.pub = this;
   ob.Notify();
}

void Publisher::Detach(Observer& ob)
{
   for (int i = 0; i < idx; i++)
      if ((*ObsList[i]) == ob)
	 for (int j = i; j < idx-1; j++)
	    ObsList[j] = ObsList[j+1];
   idx--;
   ob.pub = NULL;
}

void Publisher::NotifyObservers() const
{
   for (int i = 0; i < idx; i++)
      ObsList[i]->Notify();
}