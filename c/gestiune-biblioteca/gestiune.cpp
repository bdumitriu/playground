#include "gestiune.h"

GestiuneInventar::GestiuneInventar()
{
	*obiecte = new (ObiectGestiune*) [200];
    nrObiecte = 0;
}

GestiuneInventar::~GestiuneInventar()
{
	for (int i = 0; i < nrObiecte; i++)
		delete obiecte[i];
	delete [] obiecte;
}

int GestiuneInventar::adaugaObiect(ObiectGestiune *ob)
{
	if (nrObiecte == 200)
	{
		return -1;
	}
	else
	{
		obiecte[nrObiecte++] = ob;
		return nrObiecte-1;
	}
}

ObiectGestiune* GestiuneInventar::inlocuireObiect(int index, ObiectGestiune *ob)
{
	if ((index < 0) || (index >= nrObiecte))
	{
		return NULL;
	}
	else
	{
		ObiectGestiune *obiect = obiecte[index];
		obiecte[index] = ob;
		return obiect;
	}
}

ObiectGestiune* GestiuneInventar::stergeObiectId(int idObiect)
{
	int poz = -1;
	ObiectGestiune *obiect = NULL;
	for (int i = 0; i < nrObiecte; i++)
	{
		if (obiecte[i]->getId() == idObiect)
		{
			poz = i;
			obiect = obiecte[i];
		}
	}
	if (poz != -1)
	{
		for (i = poz; i < nrObiecte-1; i++)
		{
			obiecte[i] = obiecte[i+1];
		}
		nrObiecte--;
	}

	return obiect;
}

ObiectGestiune* GestiuneInventar::stergeObiectIndex(int index)
{
	ObiectGestiune *obiect;
	if ((index < 0) || (index >= nrObiecte))
	{
		return NULL;
	}
	obiect = obiecte[index];
	for (int i = index; i < nrObiecte-1; i++)
	{
		obiecte[i] = obiecte[i+1];
	}
	nrObiecte--;

    return obiect;
}

int GestiuneInventar::pozitie(int idObiect)
{
	int poz = -1;
	for (int i = 0; i < nrObiecte; i++)
	{
		if (obiecte[i]->getId() == idObiect)
		{
			poz = i;
		}
	}

	return poz;
}

ObiectGestiune* GestiuneInventar::getObiect(int index)
{
	return obiecte[index];
}

int GestiuneInventar::getNrObiecte()
{
	return nrObiecte;
}

void GestiuneInventar::serialize(FILE *f)
{
	fprintf(f, "%d\n", nrObiecte);
	for (int i = 0; i < nrObiecte; i++)
	{
		fprintf(f, "%d\n", obiecte[i]->getClassId());
		obiecte[i]->serialize(f);
	}
}

void GestiuneInventar::deserialize(FILE *f)
{
	int n;
	fscanf(f, "%d", &nrObiecte);
	for (int i = 0; i < nrObiecte; i++)
	{
		fscanf(f, "%d", &n);
		ObiectGestiune *temp;
		switch (n)
		{
			case CARTE_ID:
			{
				temp = new Carte();
				break;
			}
			case NUMAR_REVISTA_ID:
			{
				temp = new NumarRevista();
				break;
			}
			default:
			{
				obiecte[i] = new ObiectGestiune();
				break;
			}
		}
		temp->deserialize(f);
		obiecte[i] = temp;
	}
}