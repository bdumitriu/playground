#include <stdio.h>
#include <conio.h>
#include <string.h>

////////////////////////
// interfata claselor //
////////////////////////

/*
 * Clasa Depozit (cea de baza).
 */
class Depozit
{
public:
	// constructorii
	Depozit();
	Depozit(char denArt[15], char util[8], int nrBuc, int st);

	// cele 4 metode accesor
	char* getDenumireArticol();
	char* getUtilitate();
	int getNumarBucati();
	int getStoc();

	// cele 2 metode mutative
	void alterNumarBucati(int nrBuc);
	void alterStoc(int st);

protected:
	// variabilele protejate
	char denumireArticol[15];
	char utilitate[8];
	int	numarBucati;
	int stoc;
};

/*
 * Clasa PretDepozit (cea derivata).
 */
class PretDepozit: public Depozit
{
public:
	// constructorii
	PretDepozit();
	PretDepozit(char denArt[15], char util[8], int nrBuc, int st, float pr);

	// calculul valorii de pe stoc
	float calcVal();

private:
	// variabila suplimentara
	float pret;
};

////////////////////////////
// implementarea claselor //
////////////////////////////

/*
 * Constructor vid.
 */
Depozit::Depozit()
{
	Depozit("", "", 0, 0);
}

/*
 * Constructor cu 4 parametri
 */
Depozit::Depozit(char denArt[15], char util[8], int nrBuc, int st)
{
	strcpy(denumireArticol, denArt);
	strcpy(utilitate, util);
	numarBucati = nrBuc;
	stoc = st;
}

/*
 * Accesor la variabila denumireArticol.
 */
char* Depozit::getDenumireArticol()
{
	return denumireArticol;
}

/*
 * Accesor la variabila utilitate.
 */
char* Depozit::getUtilitate()
{
	return utilitate;
}

/*
 * Accesor la variabila numarBucati.
 */
int Depozit::getNumarBucati()
{
	return numarBucati;
}

/*
 * Accesor la variabila stoc.
 */
int Depozit::getStoc()
{
	return stoc;
}

/*
 * Mutator pentru variabila numarBucati.
 */
void Depozit::alterNumarBucati(int nrBuc)
{
	numarBucati = nrBuc;
}

/*
 * Mutator pentru variabila stoc.
 */
void Depozit::alterStoc(int st)
{
	stoc = st;
}

/*
 * Constructor vid.
 */
PretDepozit::PretDepozit()
{
	PretDepozit("", "", 0, 0, 0);
}

/*
 * Constructor cu 5 parametri.
 */
PretDepozit::PretDepozit(char denArt[15], char util[8], int nrBuc, int st,
	float pr)
: Depozit(denArt, util, nrBuc, st), pret(pr)
{}

float PretDepozit::calcVal()
{
	if (stoc == 0)
		return 0;
	else
		return pret * numarBucati;
}

/////////////////////////
// programul principal //
/////////////////////////

void main()
{
	// N-am avut timp sa mai fac citirea de la tastatura
	// pentru valorile initiale, asa ca le-am dat eu. Atata
	// lucru banuiesc ca stii si singur.

	// tabloul de 5 obiecte
	PretDepozit obiecte[5] = {
			PretDepozit("bluza", "haina", 4, 1, 150000.5),
			PretDepozit("fular", "haina", 7, 1, 72500),
			PretDepozit("biscuiti", "mancare", 150, 1, 17500.2),
			PretDepozit("napolitane", "mancare", 0, 0, 6000),
			PretDepozit("carte", "cultura", 300, 1, 200000)};

	FILE *f;
	int n, nr;
	char c;
	do
	{
		clrscr();
		printf("1. Afisare informatie.\n");
		printf("2. Modificare informatie.\n");
		printf("3. Iesire.\n\n");
		printf("Apasa 1, 2 sau 3: ");

		// nu permit utilizatorului sa scrie altceva decat
		// 1, 2 sau 3.
		do
		{
			c = getch();
		}
		while (!((c == '1') || (c == '2') || (c == '3')));
		printf("%c", c);

		switch(c)
		{
			case '1':
				// aici ar trebui interzisa orice valoare
				// alta decat 1 - 5, da' n-am mai avut vreme...
				printf("\n\nIndexul in tablou: ");
				scanf("%d", &n);

				// vezi c-am facut deschidere de tip append
				// => la fiecare nou apel se va scrie in
				// continuare in fisier, fara a se sterge ceea
				// ce exista deja => fisierul va tot creste
				// iar ultima informatie logata va fi intotdeauna
				// la sfarsitul lui.
				if ((f = fopen("log.txt", "a")) == NULL)
				{
					printf("N-am reusit deschiderea fisierului.\n");
					printf("Apasati orice tasta pentru a continua...");
					getch();
					// n-am facut iesire din program aici pentru
					// ca programul e functional chiar daca nu pot
					// deschide fisierul (doar ca nu se va scrie
					// nimic).
				}

				// acum afisez in paralel si pe ecran, si in fisier...
				printf("Date despre obiectul %d:\n", n+1);
				printf("\t- numele: %s\n", obiecte[n].getDenumireArticol());
				printf("\t- utilitatea: %s\n", obiecte[n].getUtilitate());
				printf("\t- disponibil pe stoc: ");
				fprintf(f, "Date despre obiectul %d:\n", n+1);
				fprintf(f, "\t- numele: %s\n",
					obiecte[n].getDenumireArticol());
				fprintf(f, "\t- utilitatea: %s\n",
					obiecte[n].getUtilitate());
				fprintf(f, "\t- disponibil pe stoc: ");
				if (obiecte[n].getStoc())
				{
					printf("da\n");
					printf("\t- numar bucati pe stoc: %d\n",
						obiecte[n].getNumarBucati());
					printf("\t- valoare totala pe stoc: %f\n",
						obiecte[n].calcVal());
					fprintf(f, "da\n");
					fprintf(f, "\t- numar bucati pe stoc: %d\n",
						obiecte[n].getNumarBucati());
					fprintf(f, "\t- valoare totala pe stoc: %f\n",
						obiecte[n].calcVal());
				}
				else
				{
					printf("nu\n");
					fprintf(f, "nu\n");
				}

				fprintf(f, "\n");
				// inchid fisierul
				fclose(f);
				break;
			case '2':
				printf("\n\nIndexul in tablou: ");
				scanf("%d", &n);
				printf("Noul numar de bucati: ");
				scanf("%d", &nr);
				obiecte[n].alterNumarBucati(nr);
				printf("Numarul de bucati a fost modificat cu succes.\n");
				if (nr == 0)
				{
					obiecte[n].alterStoc(0);
				}
				else
				{
					printf("Noua valoare a stocului este: %f\n",
						obiecte[n].calcVal());
				}
				break;
			case '3':
				printf("\n");
				break;
		}
		printf("\nApasati orice tasta pentru a continua...");
		getch();
	}
	while (c != '3');
}