class Stiva
{
   public:
      Stiva();
      ~Stiva();
      ad(int); //Adauagrea unui nou element in stiva
      sc(); //Scoaterea elementului din varful stivei
      lst(); //Listarea continutului stivei
      caut(int); //Cautarea unui element in stiva
   private:
      Stiva *varf;
      int n;
      Stiva *leg;
};