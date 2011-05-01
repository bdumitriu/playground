#ifndef __OBIECTE_H_
#define __OBIECTE_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define OBIECT_GESTIUNE_ID 1
#define CARTE_ID 2
#define NUMAR_REVISTA_ID 3

/*
 * Interfetele claselor ce contribuie la serviciile
 * de inventariere a obiectelor din biblioteca.
 */
class ObiectGestiune
{
public:
	// constructori
	ObiectGestiune();
	ObiectGestiune(int id, int nrPagini, int anAparitie);

	// setteri
	void setId(int id);
	void setNrPagini(int nrPagini);
	void setAnAparitie(int anAparitie);

	// getteri
	int getId();
	int getNrPagini();
	int getAnAparitie();
	int getNrImprumuturi();

	int imprumuta(int idUtilizator);
	int restituie();
	int isImprumutat();

	// metode acces fisier
	virtual void serialize(FILE *f);
	virtual void deserialize(FILE *f);

	virtual char* toString();

	virtual int getClassId() { return OBIECT_GESTIUNE_ID; };

protected:
	int id;
	int nrPagini;
	int anAparitie;
	int nrImprumuturi;
	int imprumutat;
	int idUtilizator;
};

//int ObiectGestiune::classID = OBIECT_GESTIUNE_ID;

class Carte: public ObiectGestiune
{
public:
	// constructori
	Carte();
	Carte(int id, int nrPagini, int anAparitie);

	// destructor
	~Carte();

	// setteri
	void setAutor(char *autor);
	void setTitlu(char *titlu);
	void setGen(char *gen);

	// getteri
	char* getAutor();
	char* getTitlu();
	char* getGen();

	// metode acces fisier
	// In cazul ambelor metode, se presupune ca fisierul e
	// deschis pentru scriere si indicatorul de fisier e pozitionat
	// corect.
	void serialize(FILE *f);
	void deserialize(FILE *f);

	char* toString();

	int getClassId() { return CARTE_ID; };

private:
	char autor[50];
	char titlu[50];
	char gen[20];
};

class NumarRevista: public ObiectGestiune
{
public:
	// constructori
	NumarRevista();
	NumarRevista(int id, int nrPagini, int anAparitie);

	// setteri
	void setNumar(int numar);
	void setIdRevista(int idRevista);

	// getteri
	int getNumar();
	int getIdRevista();

	// metode acces fisier
	void serialize(FILE *f);
	void deserialize(FILE *f);

	char* toString();

	int getClassId() { return NUMAR_REVISTA_ID; };

private:
	int numar;
	int idRevista;
};

class Revista
{
public:
	// constructor
	Revista(char *nume, int nrAparitii, int id);

	// destructor
	~Revista();

	// setteri
	void setNume(char *nume);
	void setNrAparitii(int nrAparitii);
	void setId(int id);

	// getteri
	char* getNume();
	int getNrAparitii();
	int getId();

private:
	char nume[50];
	int nrAparitii;
	int id;
};

#endif