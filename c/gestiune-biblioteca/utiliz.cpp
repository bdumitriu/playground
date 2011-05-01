#include "utiliz.h"

// Implementarea claselor ce contribuie la utilizarea obiectelor

Utilizator::Utilizator()
{
	Utilizator(0, 0, 0);
}

Utilizator::Utilizator(int id, char *nume, char *sex)
{
	this->id = id;
   strcpy(this->nume, nume);
   strcpy(this->sex, sex);
   nrObiecte = 0;
}

Utilizator::~Utilizator()
{
   delete [] sex;
   delete [] nume;
}

void Utilizator::setId(int id)
{
   this->id = id;
}

void Utilizator::setNume(char *nume)
{
	strcpy(this->nume, nume);
}

void Utilizator::setSex(char *sex)
{
   strcpy(this->sex, sex);
}

int Utilizator::getId()
{
	return id;
}

char* Utilizator::getNume()
{
   return nume;
}

char* Utilizator::getSex()
{
   return sex;
}

int Utilizator::getNrObiecte()
{
	return nrObiecte;
}

int Utilizator::imprumutata(int idObiect)
{
	for (int i = 0; i < nrObiecte; i++)
		if (idObiecte[i] == idObiect)
			return 1;
	return 0;
}

/*
 * Intoarce o cifra de la 0 la 5, functie de cate carti
 * mai poate imprumuta utilizatorul si -1 daca acesta
 * are deja 5 carti (maximul posibil) in cont
 */
int Utilizator::imprumuta(int idObiect)
{
	if ( nrObiecte == 5 )
		return -1;        // eroare
	else
	{
		idObiecte[nrObiecte++] = idObiect;
		return ( 5 - nrObiecte );
	}
}

/*
 * Intoarce 1 daca utilizatorul nu avea respectiva carte
 * imprumutata si 0 altfel.
 */
int Utilizator::restituie(int idObiect)
{
	int gasit = -1;
	for (int i = 0; i < nrObiecte; i++)
	{
		if (idObiecte[i] == idObiect)
			gasit = i;
	}
	if (gasit == -1)
	{
		return 1;
	}
	else
	{
		for (i = gasit; i < nrObiecte-1; i++)
		{
			idObiecte[i] = idObiecte[i+1];
		}
		nrObiecte--;
		return 0;
	}
}

void Utilizator::serialize(FILE *f)
{
	fprintf(f, "%d %d\n", id, nrObiecte);
	for (int i = 0; i < nrObiecte; i++)
	{
		fprintf(f, "%d ", idObiecte[i]);
	}
	fprintf(f, "\n%s\n%s\n", nume, sex);
}

void Utilizator::deserialize(FILE *f)
{
	fscanf(f, "%d %d", &id, &nrObiecte);
	for (int i = 0; i < nrObiecte; i++)
	{
		fscanf(f, "%d", &idObiecte[i]);
	}
	fgets(nume, 20, f);
	fgets(nume, 20, f);
	nume[strlen(nume)-1] = '\0';
	fgets(sex, 10, f);
	sex[strlen(sex)-1]  = '\0';
}

// Sfarsit explicitari metode clasa Utilizator

UtilizatorIntern::UtilizatorIntern()
: Utilizator(0, "", "")
{
	strcpy(facultate, "");
}

UtilizatorIntern::UtilizatorIntern(int id, char *nume, char *sex,
	char *facultate)
: Utilizator(id, nume, sex)
{
	strcpy(this->facultate, facultate);
}

UtilizatorIntern::~UtilizatorIntern()
{
	delete [] facultate;
}

void UtilizatorIntern::setFacultate(char *facultate)
{
	strcpy(this->facultate, facultate);
}

char* UtilizatorIntern::getFacultate()
{
	return facultate;
}

void UtilizatorIntern::serialize(FILE *f)
{
	Utilizator::serialize(f);
    fprintf(f, "%s\n", facultate);
}

void UtilizatorIntern::deserialize(FILE *f)
{
	Utilizator::deserialize(f);
	fgets(facultate, 40, f);
	facultate[strlen(facultate)-1] = '\0';
}

// Sfarsit explicitari metode clasa UtilizatorIntern

UtilizatorExtern::UtilizatorExtern()
: Utilizator(0, "", "")
{
	strcpy(domiciliu, "");
}

UtilizatorExtern::UtilizatorExtern(int id, char *nume, char *sex,
	char *domiciliu)
: Utilizator(id, nume, sex)
{
	strcpy(this->domiciliu, domiciliu);
}

UtilizatorExtern::~UtilizatorExtern()
{
	delete [] domiciliu;
}

void UtilizatorExtern::setDomiciliu(char *domiciliu)
{
	strcpy(this->domiciliu, domiciliu);
}

char* UtilizatorExtern::getDomiciliu()
{
	return domiciliu;
}

void UtilizatorExtern::serialize(FILE *f)
{
	Utilizator::serialize(f);
	fprintf(f, "%s\n", domiciliu);
}

void UtilizatorExtern::deserialize(FILE *f)
{
	Utilizator::deserialize(f);
	fgets(domiciliu, 40, f);
	domiciliu[strlen(domiciliu)-1] = '\0';
}

char* UtilizatorExtern::toString()
{
	char *str = new char[200];
	char aux[20];

	strcpy(str, "");
	strcat(str, "UtilizatorExtern cu datele:\n");
	strcat(str, "id: ");
	itoa(id, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "nume: ");
	strcat(str, nume);
	strcat(str, "\n");
	strcat(str, "sex: ");
	strcat(str, sex);
	strcat(str, "\n");
	strcat(str, "nrObiecte: ");
	itoa(nrObiecte, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "obiectele: ");
	for (int i = 0; i < nrObiecte; i++)
	{
		itoa(idObiecte[i], aux, 10);
		strcat(str, aux);
		strcat(str, " ");
	}
	strcat(str, "\n");
	strcat(str, "domiciliu: ");
	strcat(str, domiciliu);

	return str;
}

// Sfarsit explicitari metode clasa UtilizatorExtern

UtilizatorStudent::UtilizatorStudent()
: UtilizatorIntern(0, "", "", "")
{
	an = 1;
	grupa = 0;
	strcpy(sectie, "");
}

UtilizatorStudent::UtilizatorStudent(int id, char *nume, char *sex,
	char *facultate)
: UtilizatorIntern(id, nume, sex, facultate)
{
	an = 1;
	grupa = 0;
	strcpy(sectie, "");
}

UtilizatorStudent::~UtilizatorStudent()
{
	delete [] sectie;
}

void UtilizatorStudent::setAn(int an)
{
	this->an = an;
}

void UtilizatorStudent::setGrupa(int grupa)
{
   this->grupa = grupa;
}

void UtilizatorStudent::setSectie(char *sectie)
{
	strcpy(this->sectie, sectie);
}

int UtilizatorStudent::getAn()
{
	return an;
}

int UtilizatorStudent::getGrupa()
{
	return grupa;
}

char* UtilizatorStudent::getSectie()
{
	return sectie;
}

void UtilizatorStudent::serialize(FILE *f)
{
	UtilizatorIntern::serialize(f);
	fprintf(f, "%d %d\n", an, grupa);
	fprintf(f, "%s\n", sectie);
}

void UtilizatorStudent::deserialize(FILE *f)
{
	UtilizatorIntern::deserialize(f);
	fscanf(f, "%d %d", &an, &grupa);
	fgets(sectie, 20, f);
	fgets(sectie, 20, f);
	sectie[strlen(sectie)-1] = '\0';
}

char* UtilizatorStudent::toString()
{
	char *str = new char[300];
	char aux[20];

	strcpy(str, "");
	strcat(str, "UtilizatorStudent cu datele:\n");
	strcat(str, "id: ");
	itoa(id, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "nume: ");
	strcat(str, nume);
	strcat(str, "\n");
	strcat(str, "sex: ");
	strcat(str, sex);
	strcat(str, "\n");
	strcat(str, "nrObiecte: ");
	itoa(nrObiecte, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "obiectele: ");
	for (int i = 0; i < nrObiecte; i++)
	{
		itoa(idObiecte[i], aux, 10);
		strcat(str, aux);
		strcat(str, " ");
	}
	strcat(str, "\n");
	strcat(str, "facultate: ");
	strcat(str, facultate);
	strcat(str, "\n");
	strcat(str, "an: ");
	itoa(an, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "grupa: ");
	itoa(grupa, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "sectie: ");
	strcat(str, sectie);
	strcat(str, "\n");

	return str;
}

// Sfarsit explicitari metode clasa UtilizatorStudent

UtilizatorCadruDidactic::UtilizatorCadruDidactic()
: UtilizatorIntern(0, "", "", "")
{
	strcpy(titlu, "");
	strcpy(catedra, "");
}

UtilizatorCadruDidactic::UtilizatorCadruDidactic(int id, char *nume, char *sex,
	char *facultate)
: UtilizatorIntern(id, nume, sex, facultate)
{
	strcpy(titlu, "");
	strcpy(catedra, "");
}

UtilizatorCadruDidactic::~UtilizatorCadruDidactic()
{
	delete [] titlu;
	delete [] catedra;
}

void UtilizatorCadruDidactic::setTitlu(char *titlu)
{
	strcpy(this->titlu, titlu);
}

void UtilizatorCadruDidactic::setCatedra(char *catedra)
{
	strcpy(this->catedra, catedra);
}

char* UtilizatorCadruDidactic::getTitlu()
{
	return titlu;
}

char* UtilizatorCadruDidactic::getCatedra()
{
	return catedra;
}

void UtilizatorCadruDidactic::serialize(FILE *f)
{
	UtilizatorIntern::serialize(f);
	fprintf(f, "%s\n", titlu);
	fprintf(f, "%s\n", catedra);
}

void UtilizatorCadruDidactic::deserialize(FILE *f)
{
	UtilizatorIntern::deserialize(f);
	fgets(titlu, 15, f);
	titlu[strlen(titlu)-1] = '\0';
	fgets(catedra, 25, f);
	catedra[strlen(catedra)-1] = '\0';
}

char* UtilizatorCadruDidactic::toString()
{
	char *str = new char[300];
	char aux[20];

	strcpy(str, "");
	strcat(str, "UtilizatorCadruDidactic cu datele:\n");
	strcat(str, "id: ");
	itoa(id, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "nume: ");
	strcat(str, nume);
	strcat(str, "\n");
	strcat(str, "sex: ");
	strcat(str, sex);
	strcat(str, "\n");
	strcat(str, "nrObiecte: ");
	itoa(nrObiecte, aux, 10);
	strcat(str, aux);
	strcat(str, "\n");
	strcat(str, "obiectele: ");
	for (int i = 0; i < nrObiecte; i++)
	{
		itoa(idObiecte[i], aux, 10);
		strcat(str, aux);
		strcat(str, " ");
	}
	strcat(str, "\n");
	strcat(str, "facultate: ");
	strcat(str, facultate);
	strcat(str, "\n");
	strcat(str, "titlu: ");
	strcat(str, titlu);
	strcat(str, "\n");
	strcat(str, "catedra: ");
	strcat(str, catedra);
	strcat(str, "\n");

	return str;
}

// Sfarsit explicitari metode clasa UtilizatorCadruDidactic