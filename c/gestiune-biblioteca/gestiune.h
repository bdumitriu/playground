#ifndef __GESTIUNE_H_
#define __GESTIUNE_H_

#include "obiecte.h"
#include "utiliz.h"

class GestiuneInventar
{
public:
	GestiuneInventar();
	~GestiuneInventar();

	/*
	 * Adauga un nou obiect la inventar.
	 * Intoarce -1 daca s-a atins limita superioara de obiecte din
	 * gestiunea inventarului si indexul la care a fost memorat obiectul
	 * altfel.
	 */
	int adaugaObiect(ObiectGestiune *ob);

	/*
	 * Inlocuieste obiectul de pe pozitia index cu obiectul nou furnizat
	 * ca parametru si intoarce vechiul obiect.
	 * Daca index nu este un index valid, intoarce NULL si nu realizeaza
	 * nici o inlocuire.
	 */
	ObiectGestiune* inlocuireObiect(int index, ObiectGestiune *ob);

	/*
	 * Sterge din inventar obiectul cu id-ul egal cu idObiect daca-l gaseste.
	 * Intoarce obiectul sters sau NULL daca nu gaseste un obiect cu
	 * id-ul egal cu idObiect.
	 */
	ObiectGestiune* stergeObiectId(int idObiect);

	/*
	 * Sterge din inventar obiectul de pe pozitia index si-l intoarce.
	 * Daca index nu este o pozitie valida se intoarce NULL si nu se
	 * modifica inventarul.
	 */
	ObiectGestiune* stergeObiectIndex(int index);

	/*
	 * Intoarce pozitia pe care se afla obiectul identificat prin
	 * idObiect sau -1 daca nu-l gaseste in inventar.
	 */
	int pozitie(int idObiect);

	/*
	 * Intoarce obiectul de pe pozitia index.
	 */
	ObiectGestiune* getObiect(int index);

	/*
	 * Intoarce numarul de obiecte din inventar.
	 */
	int getNrObiecte();

	void serialize(FILE *f);
	void deserialize(FILE *f);

private:
	ObiectGestiune **obiecte;
	// ObiectGestiune *obiecte[200];
	int nrObiecte;
};

class GestiuneUtilizatori
{};

#endif