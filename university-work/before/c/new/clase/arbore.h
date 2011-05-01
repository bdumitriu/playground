class Arbore
{
   public:
      Arbore();
      ~Arbore();
      ad(int); //Adaugarea unui nod
      sc(int); //Scoaterea unui nod
      preord(); // Listarea arborelui in preordine
      inord(); // Listarea arborelui in inordine
      postord(); // Listarea arborelui in postordine
      caut(int); //Cautarea unui element in arbore
   private:
      Arbore *rad;
      int n;
      Arbore *s;
      Arbore *d;
};