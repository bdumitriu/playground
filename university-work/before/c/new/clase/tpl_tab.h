#if !defined (__TPL_TAB_H__)
#define __TPL_TAB_H__

#include "it_tab.h"
#include <stdio.h>
#include <conio.h>
#include <iostream.h>

template <class T>
class TablouIterator;

template <class T>
class Tablou
{
public:
   typedef void (*ApplyFunc)(T&, void *);
   typedef int  (*CondFunc)(const T&, void *);

   Tablou(int dim=10);
   ~Tablou();

   virtual void Adaug(const T& el);
   void Scot(const T& el);
   void List() const;
   void List(ostream& os) const;
   int  Exista(const T& el) const;

   friend ostream& operator<<(ostream& os, const Tablou<T>& t);
   friend istream& operator>>(istream& is, Tablou<T>& t);

   int       operator==(const Tablou<T>& t) const;
   int       operator!=(const Tablou<T>& t) const;
   const T&  operator[](int idx) const;

   void ForEach(void (*f)(T&,void*) , void *arg);
   const T*   FirstThat(int (*f)(const T&, void*), void *arg);
   const T*   LastThat(int (*f)(const T&, void*), void *arg);

   friend class TablouIterator<T>;

protected:
   T*  tab;
   int ind;

private:
   int dim;
   Tablou<T>& operator=(const Tablou<T>& t);
   Tablou(const Tablou<T>& t);

};

template <class T>
Tablou<T>::Tablou(int dim)
{
   tab= new T [dim];
   ind= 0;
   this->dim= dim;
}

template <class T>
Tablou<T>::~Tablou()
{
   delete [] tab;
}

template <class T>
void Tablou<T>::Adaug(const T& el)
{
   if (ind < dim)
      tab[ind++]= el;
   else
   {
      Tablou<T> t_aux(dim);

      for (int j= 0; j < dim; j++)
	 t_aux.Adaug(tab[j]);
      delete [] tab;
      tab= new T [dim+10];
      ind= 0;
      dim+= 10;
      for (j= 0; j < dim-10; j++)
	 tab[ind++]= t_aux[j];
      tab[ind++]= el;
   }
}

template <class T>
void Tablou<T>::Scot(const T& el)
{
   for (int i= 0; i < ind; i++)
      if (tab[i] == el)
      {
	 for (int j= i; j < ind-1; j++)
	    tab[j]= tab[j+1];
	 ind--;
	 break;
      }
}

template <class T>
void Tablou<T>::List() const
{
   printf("Tabloul : ");
   for (int i= 0; i < ind; i++)
      cout<<tab[i]<<' ';
   getch();
}

template <class T>
const T& Tablou<T>::operator[](int idx) const
{
   return tab[idx];
}

template<class T>
ostream& operator<<(ostream& os, const Tablou<T>& t)
{
   os<<t.dim<<' '<<t.ind<<' ';
   for (int i= 0; i < t.ind; i++)
      os<<t.tab[i]<<' ';

   return os;
}

template<class T>
istream& operator>>(istream& is, Tablou<T>& t)
{
   int d, i;
   T aux;

   is>>d;
   is.get();
   is>>i;
   is.get();
   for (int j= 0; j < i; j++)
   {
      is>>aux;
      t.Adaug(aux);
      is.get();
   }

   return is;
}

template <class T>
int Tablou<T>::Exista(const T& el) const
{
   for (int i= 0; i < ind; i++)
      if (tab[i] == el)
	 return i;

   return -1;
}

template <class T>
void Tablou<T>::List(ostream& os) const
{
   os<<*this;
}

template <class T>
int Tablou<T>::operator==(const Tablou<T>& t) const
{
   if (!(this->ind == t.ind))
      return 0;
   for (int i= 0; i < t.ind; i++)
      if (!(tab[i] == t[i]))
	 return 0;

   return 1;
}

template <class T>
int Tablou<T>::operator!=(const Tablou<T>& t) const
{
   return !(*this == t);
}

template <class T>
void Tablou<T>::ForEach(void (*f)(T&, void*), void *arg)
{
   TablouIterator<T> it(*this);

   while (it)
   {
      f(it(), arg);
      it++;
   }
}

template <class T>
const T* Tablou<T>::FirstThat(int (*f)(const T&, void*), void *arg)
{
   TablouIterator<T> it(*this);

   while (it)
   {
      if (f(it(), arg))
	 return &it();
      it++;
   }

   return NULL;
}

template <class T>
const T* Tablou<T>::LastThat(int (*f)(const T&, void*), void *arg)
{
   TablouIterator<T> it(*this);

   it.Last();
   while (it)
   {
      if (f(it(), arg))
	 return &it();
      it--;
   }

   return NULL;
}

#endif