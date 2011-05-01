#if !defined(__IT_TAB_H__)
#define __IT_TAB_H__

#include "tpl_tab.h"

template <class T>
class Tablou;

template <class T>
class TablouIterator
{
public:
   TablouIterator(const Tablou<T>& t);
   ~TablouIterator();

   operator int();
   const T& operator ()();
   void operator++(int);
   void Restart();
   void Last();
   void operator--(int);

private:
   const Tablou<T> *itp;
   int             idx;

};

template <class T>
TablouIterator<T>::TablouIterator(const Tablou<T>& t)
{
   idx= 0;
   itp= &t;
}

template <class T>
TablouIterator<T>::~TablouIterator()
{}

template <class T>
TablouIterator<T>::operator int()
{
   return (!((idx == itp->ind) || (idx == -1)));
}

template <class T>
const T& TablouIterator<T>::operator ()()
{
   return (*itp)[idx];
}

template <class T>
void  TablouIterator<T>::operator++(int)
{
   idx++;
}

template <class T>
void TablouIterator<T>::operator--(int)
{
   idx--;
}

template <class T>
void TablouIterator<T>::Restart()
{
   idx= 0;
}

template <class T>
void TablouIterator<T>::Last()
{
   idx= itp->ind-1;
}

#endif