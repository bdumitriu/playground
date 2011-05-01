class Graf
{
   public:
      Graf(int x= 50, int y= 0);       // constructor cu:
				       //   x - numarul de noduri;
				       //   y = 1/0 - orientat/neorientat
      ~Graf();                         // destructor
      void adm(int x, int y, int z= 1);// adaugarea arcului/muchiei (x,y)
				       // cu costul z (implicit 1)
      void scm(int x, int y);          // indepartarea arcului/muchiei (x,y)
		 // Pentru cele 2 parcurgeri graful trebuie sa fie conex !!!
      void bf(int x= 0);               // parcurgerea grafului BreadthFirst
				       // unde x e varful de plecare
				       // implicit 1
      void df(int x= 0);               // parcurgerea grafului DepthFirst
				       // unde x e varful de plecare
				       // implicit 1
      void apcm();                     // arbore partial de cost minim
      void drm(int x, int y);          // drum de cost minim intre x si y
   private:
      int **t;
      int tip;
      int n;
};