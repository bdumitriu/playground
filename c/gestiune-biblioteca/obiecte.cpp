/*
 * Implementarea claselor ce contribuie la serviciile
 * de inventariere a obiectelor din biblioteca.
 */

#include "obiecte.h"

ObiectGestiune::ObiectGestiune()
{
	ObiectGestiune(0, 0, 0);
}

ObiectGestiune::ObiectGestiune(int id, int nrPagini, int anAparitie)
{
	this->id = id;
	this->nrPagini = nrPagini;
	this->anAparitie = anAparitie;
	nrImprumuturi = 0;
	imprumutat = 0;
}

void ObiectGestiune::setId(int id)
{
	this->id = id;
}

void ObiectGestiune::setNrPagini(int nrPagini)
{
	this->nrPagini = nrPagini;
}

void ObiectGestiune::setAnAparitie(int anAparitie)
{
	this->anAparitie = anAparitie;
}

int ObiectGestiune::getId()
{
	return id;
}

int ObiectGestiune::getNrPagini()
{
	return nrPagini;
}

int ObiectGestiune::getAnAparitie()
{
	return anAparitie;
}

int ObiectGestiune::getNrImprumuturi()
{
	return nrImprumuturi;
}

/*
 * Intoarce 1 daca imprumutul s-a facut cu succes (cartea
 * nu era deja imprumutata) si 0 altfel.
 */
int ObiectGestiune::imprumuta(int idUtilizator)
{
	if (imprumutat == 1)
		return 0;
	else
	{
		imprumutat = 1;
		this->idUtilizator = idUtilizator;
		nrImprumuturi++;
        return 1;
	}
}

/*
 * Intoarce 1 daca restituirea s-a facut cu succes (cartea
 * era imprumutata) si 0 altfel.
 */
int ObiectGestiune::restituie()
{
	if (imprumutat == 0)
		return 0;
	else
	{
		imprumutat = 0;
        return 1;
	}
}

/*
 * Intoarce 1 daca obiectul din gestiune e imprumutat si 0
 * daca nu e imprumutat.
 */
int ObiectGestiune::isImprumutat()
{
	return imprumutat;
}

void ObiectGestiune::serialize(FILE *f)
{}

void ObiectGestiune::deserialize(FILE *f)
{}

char* ObiectGestiune::toString()
{
	char *str = new char[200];
	char aux[20];

	strcpy(str, "Carte:\n");
	strcat(str, "id: ");
	itoa(id, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "numar pagini: ");
	itoa(nrPagini, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "an aparitie: ");
	itoa(anAparitie, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "numar imprumuturi: ");
	itoa(nrImprumuturi, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "imprumutata: ");
	itoa(imprumutat, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "id utilizator: ");
	itoa(idUtilizator, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");

	return str;
}

// Sfarsit explicitari metode ObiectGestiune

Carte::Carte()
: ObiectGestiune(0, 0, 0)
{
	strcpy(autor, "");
	strcpy(titlu, "");
	strcpy(gen, "");
}

Carte::Carte(int id, int nrPagini, int anAparitie)
: ObiectGestiune(id, nrPagini, anAparitie)
{
	strcpy(autor, "");
	strcpy(titlu, "");
	strcpy(gen, "");
}

Carte::~Carte()
{
	delete [] autor;
	delete [] titlu;
	delete [] gen;
}

void Carte::setAutor(char *autor)
{
	strcpy(this->autor, autor);
}

void Carte::setTitlu(char *titlu)
{
	strcpy(this->titlu, titlu);
}

void Carte::setGen(char *gen)
{
	strcpy(this->gen, gen);
}

char* Carte::getAutor()
{
	return autor;
}

char* Carte::getTitlu()
{
	return titlu;
}

char* Carte::getGen()
{
	return gen;
}

void Carte::serialize(FILE *f)
{
	fprintf(f, "%d %d %d %d %d %d\n%s\n%s\n%s\n", id, nrPagini,
		anAparitie, nrImprumuturi, imprumutat, idUtilizator, autor, titlu,
		gen);
}

void Carte::deserialize(FILE *f)
{
	fscanf(f, "%d %d %d %d %d %d", &id, &nrPagini, &anAparitie,
		&nrImprumuturi, &imprumutat, &idUtilizator);
	fgets(autor, 50, f);
	fgets(autor, 50, f);
	autor[strlen(autor)-1] = '\0';
	fgets(titlu, 50, f);
	titlu[strlen(titlu)-1] = '\0';
	fgets(gen, 20, f);
	gen[strlen(gen)-1] = '\0';
}

char* Carte::toString()
{
	char *str = new char[200];
	char aux[20];

	strcpy(str, "Carte:\n");
	strcat(str, "id: ");
	itoa(id, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "numar pagini: ");
	itoa(nrPagini, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "an aparitie: ");
	itoa(anAparitie, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "numar imprumuturi: ");
	itoa(nrImprumuturi, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "imprumutata: ");
	itoa(imprumutat, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "id utilizator: ");
	itoa(idUtilizator, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "autor: ");
	strcat(str, autor);
	strcat(str, "\n");
	strcat(str, "titlu: ");
	strcat(str, titlu);
	strcat(str, "\n");
	strcat(str, "gen: ");
	strcat(str, gen);
	strcat(str, "\n");

	return str;
}

// Sfarsit explicitari metode Carte

NumarRevista::NumarRevista()
{
	NumarRevista(0, 0, 0);
}

NumarRevista::NumarRevista(int id, int nrPagini, int anAparitie)
: ObiectGestiune(id, nrPagini, anAparitie)
{
	numar = 0;
	idRevista = 0;
}

void NumarRevista::setNumar(int numar)
{
	this->numar = numar;
}

void NumarRevista::setIdRevista(int idRevista)
{
	this->idRevista = idRevista;
}

int NumarRevista::getNumar()
{
	return numar;
}

int NumarRevista::getIdRevista()
{
   return idRevista;
}

void NumarRevista::serialize(FILE *f)
{
	fprintf(f, "%d %d %d %d %d %d %d %d\n", id, nrPagini,
		anAparitie, nrImprumuturi, imprumutat, idUtilizator, idRevista,
		numar);
}

void NumarRevista::deserialize(FILE *f)
{
	fscanf(f, "%d %d %d %d %d %d %d %d", &id, &nrPagini,
		&anAparitie, &nrImprumuturi, &imprumutat, &idUtilizator, &idRevista,
		&numar);
}

char* NumarRevista::toString()
{
	char *str = new char[200];
	char aux[20];

	strcpy(str, "NumarRevista:\n");
	strcat(str, "id: ");
	itoa(id, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "numar pagini: ");
	itoa(nrPagini, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "an aparitie: ");
	itoa(anAparitie, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "numar imprumuturi: ");
	itoa(nrImprumuturi, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "imprumutata: ");
	itoa(imprumutat, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "id utilizator: ");
	itoa(idUtilizator, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "numar: ");
	itoa(numar, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "id revista: ");
	itoa(idRevista, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");

	return str;
}

// Sfarsit explicitari metode NumarRevista

Revista::Revista(char *nume, int nrAparitii, int id)
{
	strcpy(this->nume, nume);
	this->nrAparitii = nrAparitii;
	this->id = id;
}

void Revista::setNume(char *nume)
{
	strcpy(this->nume, nume);
}

void Revista::setNrAparitii(int nrAparitii)
{
	this->nrAparitii = nrAparitii;
}

void Revista::setId(int id)
{
	this->id = id;
}

char* Revista::getNume()
{
	return nume;
}

int Revista::getNrAparitii()
{
	return nrAparitii;
}

int Revista::getId()
{
	return id;
}
