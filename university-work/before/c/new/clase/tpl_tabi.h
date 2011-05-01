// tablou indirect - tablou ce contine pointeri.

#if !defined (__TPL_TABI_H__)
#define __TPL_TABI_H__

#include <stdio.h>
#include <conio.h>
#include <iostream.h>

template <class T>
class TablouI
{
public:
   TablouI(int dim = 10, int delta = 5, int bShouldDelete = 0); // bShouldDelete - se distrug
   ~TablouI();                                              // si elementele pointate.

   void Adaug(T *el);
   void Scot(T *el);
   void List() const;
   void List(ostream& os) const;
   int  Exista(const T *el) const;
   int  GetDim() const;

/*
   friend ostream& operator<<(ostream& os, const TablouI<T>& t);
   friend istream& operator>>(istream& is, TablouI<T>& t);
*/

   int       operator==(const TablouI<T>& t) const;
   int       operator!=(const TablouI<T>& t) const;
   const T*  operator[](int idx) const;

   void ForEach(void (*f)(T&, void*) , void *arg);
   const T*   FirstThat(int (*f)(const T&, void*), void *arg);
   const T*   LastThat(int (*f)(const T&, void*), void *arg);

//   friend class TablouIIterator<T>;

protected:
   T** tab;

private:
   int dim;
   int ind, bsd, dlt;
   TablouI<T>& operator=(const TablouI<T>& t);
   TablouI(const TablouI<T>& t);

};

template <class T>
TablouI<T>::TablouI(int dim, int delta, int bShouldDelete)
{
   tab= new (T**) [dim];
   ind= 0;
   this->dim= dim;
   bsd= bShouldDelete;
   dlt = delta;
}

template <class T>
TablouI<T>::~TablouI()
{
   if (bsd)
      for (int i= 0; i < ind; i++)
	 delete tab[i];

//   delete tab;
}

template <class T>
void TablouI<T>::Adaug(T *el)
{
   if (ind < dim)
      tab[ind++]= el;
   else
   {
      T** newTab = new (T**) [dim+dlt];

      for(int i=0; i<ind; i++)
	 newTab[i] = tab[i];

      delete [] tab;
      tab = newTab;
      dim+= dlt;
      tab[ind++]= el;
   }
}

template <class T>
void TablouI<T>::Scot(T *el)
{
   for (int i= 0; i < ind; i++)
      if (*tab[i] == *el)
      {
	 for (int j= i; j < ind-1; j++)
	    tab[j]= tab[j+1];
	 ind--;
	 break;
      }
}

template <class T>
void TablouI<T>::List() const
{
   printf("Tabloul : ");
   for (int i= 0; i < ind; i++)
//      cout<<*tab[i]<<' ';
   getch();
}

template <class T>
const T* TablouI<T>::operator[](int idx) const
{
   return tab[idx];
}

/*
template<class T>
ostream& operator<<(ostream& os, const TablouI<T>& t)
{
   os<<t.dim<<' '<<t.ind<<' ';
   for (int i= 0; i < t.ind; i++)
      os<<*t.tab[i]<<' ';

   return os;
}

template<class T>
istream& operator>>(istream& is, TablouI<T>& t)
{
   int d, i;

   is>>d;
   is.get();
   is>>i;
   is.get();
   for (int j= 0; j < i; j++)
   {
      T* aux= new T;
      is>>*aux;
      t.Adaug(aux);
      is.get();
   }

   return is;
}
*/

template <class T>
int TablouI<T>::Exista(const T *el) const
{
   for (int i= 0; i < ind; i++)
      if (*tab[i] == *el)
	 return i;

   return -1;
}

template <class T>
void TablouI<T>::List(ostream& os) const
{
//   os<<*this;
}

template <class T>
int TablouI<T>::operator==(const TablouI<T>& t) const
{
   if (!(this->ind == t.ind))
      return 0;
   for (int i= 0; i < t.ind; i++)
      if (!(*tab[i] == *t[i]))
	 return 0;

   return 1;
}

template <class T>
int TablouI<T>::operator!=(const TablouI<T>& t) const
{
   return !(*this == t);
}

template <class T>
void TablouI<T>::ForEach(void (*f)(T&, void*), void *arg)
{
   for (int i= 0; i < ind; i++)
      f(*tab[i], arg);
}

template <class T>
const T* TablouI<T>::FirstThat(int (*f)(const T&, void*), void *arg)
{
   for (int i= 0; i < ind; i++)
      if (f(*tab[i], arg))
	 return tab[i];

   return NULL;
}

template <class T>
const T* TablouI<T>::LastThat(int (*f)(const T&, void*), void *arg)
{
   for (int i= ind-1; i > -1; i++)
      if (f(*tab[i], arg))
	 return tab[i];

   return NULL;
}

template <class T>
int TablouI<T>::GetDim() const
{
   return ind;
}

#endif