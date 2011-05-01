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
{ }



