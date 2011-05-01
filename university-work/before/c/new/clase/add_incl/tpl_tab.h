#if !defined(__TPL_TAB_H__)
#define __TPL_TAB_H__

#include <stdio.h>
#include <conio.h>
#include <iostream.h>

template <class T>
class Tablou
{
public:
   Tablou(int dim= 10);
   ~Tablou();

   void Adaug(const T& el);
   void Scot(const T& el);
   void List() const;
   void List(ostream& os) const;
   int  Exista(const T& el) const;

   friend ostream& operator<<(ostream& os, const Tablou& t);
   T& operator[](int idx);

private:
   T*  tab;
   int dim;
   int ind;

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
   tab[ind++]= el;
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
      cout<<tab[i];
   getch();
}

template <class T>
T& Tablou<T>::operator[](int idx)
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

#endif