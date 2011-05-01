class Tablou
{
   public:
      Tablou(int d = 10);
      ~Tablou();
      void adaug(int x);
      void scot(int x);
      void list();
      int& operator[](int idx);

   private:
      int* tab;
      int ind;
      int dim;
};






