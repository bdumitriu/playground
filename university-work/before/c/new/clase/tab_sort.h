#if !defined(__TAB_SORT_H__)
#define __TAB_SORT_H__

#include "tpl_tab.h"

template <class T>
class TablouSortat : public Tablou<T>     
{
public:
   TablouSortat(int (*f)(const T& x1, const T& x2),int dim= 10);

   void Adaug(const T& el);

private:
   int (*OrdFunc)(const T& x1, const T& x2);
};

template <class T>
TablouSortat<T>::TablouSortat(int (*f)(const T& x1, const T& x2), int dim)
   : Tablou<T>(dim)
{
   OrdFunc = f;
}

template <class T>
void TablouSortat<T>::Adaug( const T& el)
{
   int i= 0;

   if (ind == 0)
   {
      tab[ind++]= el;
      return;
   }
   while ((OrdFunc(tab[i], el)) && (i < ind))
      i++;
   for (int j= ind-1; j >= i; j--)
      tab[j+1]= tab[j];
   tab[i]= el;
   ind++;
}

#endif

