class Stivat
{
   public:
      Stivat(int x= 50);    // constructor
      ~Stivat();            // destructor
      void ad(int x);       // adaugarea unui element in stiva
      void sc();            // scoaterea unui element din stiva
      void lst();           // listarea continutului stivei
      void caut(int x);     // cautarea unui element in stiva
   private:
      int varf;
      int *t;
};