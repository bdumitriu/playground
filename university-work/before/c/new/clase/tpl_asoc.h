#if !defined(__TPL_ASOC_H__)
#define __TPL_ASOC_H__

#include <iostream.h>
#include <stdio.h>
#include <conio.h>

template <class T, class V>   // daca fac un template si-l scriu tot intr-un
class Asoc                    // header atunci cum il dau cui vreau sa-l dau
{                             // fara sa vada partea de definire a functiilor
public:                       // membru?
   Asoc();
   Asoc(const T& che, const V& valo);
   ~Asoc();

   const T& Cheie();
   const V& Valoare();
   void  List(ostream& os) const;
   int   operator==(const Asoc<T, V>& x) const;
   friend ostream& operator<<(ostream& os, const Asoc<T, V>& x);
   friend istream& operator>>(istream& is, Asoc<T, V>& x);

private:
   T ch;
   V val;
};

template <class T, class V>
Asoc<T, V>::Asoc()
{}

template <class T, class V>
Asoc<T, V>::Asoc(const T& che, const V& valo)
 :ch(che), val(valo)
{}

template <class T, class V>
Asoc<T, V>::~Asoc()
{}

template <class T, class V>
const T& Asoc<T, V>::Cheie()
{
   return ch;
}

template <class T, class V>
const V& Asoc<T, V>::Valoare()
{
   return val;
}

template <class T, class V>
void Asoc<T, V>::List(ostream& os) const
{
   os<<ch<<" - "<<val;
}

template <class T, class V>
ostream& operator<<(ostream& os, const Asoc<T, V>& x)
{
   os<<x.ch<<'-'<<x.val;
   return os;
}

template <class T, class V>
istream& operator>>(istream& is, Asoc<T, V>& x)
{
   char buf[100];

   is>>x.ch;
   is.get();
   is>>x.val;

   return is;
}

template <class T, class V>
int Asoc<T, V>::operator==(const Asoc<T, V>& x) const
{
   if (x.ch == ch)
      return 1;
   return 0;
}

#endif