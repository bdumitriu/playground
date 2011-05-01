#if !defined(__TPL_DICT_H__)
#define __TPL_DICT_H__

#include <stdio.h>
#include <conio.h>
#include <iostream.h>
#include "tpl_tab.h"
#include "tpl_asoc.h"
#include "str.h"

template <class T, class V>
class Dict
{
public:
   Dict(int dim= 100);
   ~Dict();

   int   Adauga(const T& ch, const V& val);
   const V& Gaseste(const T& ch);
   void  Scoate(const T& ch);
   void  List();

   friend ostream& operator<<(ostream& os, const Dict<T, V>& d);
   friend istream& operator>>(istream& is, Dict<T, V>& d);

private:
   Tablou< Asoc<T, V> > t;
};

template <class T, class V>
Dict<T, V>::Dict(int dim= 100)
: t(dim)
{}

template <class T, class V>
Dict<T, V>::~Dict()
{}

template <class T, class V>
int Dict<T, V>::Adauga(const T& ch, const V& val)
{
   if (t.Exista(Asoc<T, V> (ch, V())) != -1)
      return 0;
   t.Adaug(Asoc<T,V>(ch, val));
      return 1;
}

template <class T, class V>
const V& Dict<T, V>::Gaseste(const T& ch)
{
   int pos= t.Exista(Asoc<T, V>(ch, V()));

   if(pos == -1)
   {
      cout<<"Cheia nu se afla in sir.\n";
      getch();
      return *new V();
   }
   else
      return t[pos].Valoare();
}

template <class T, class V>
void Dict<T, V>::List()
{
   t.List();
}

template <class T, class V>
ostream& operator<<(ostream& os, const Dict<T, V>& d)
{
   os<<d.t;

   return os;
}

template <class T, class V>
istream& operator>>(istream& is, Dict<T, V>& d)
{
   is>>d.t;

   return is;
}

#endif