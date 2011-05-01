#include <stdio.h>
#include <conio.h>
#include <string.h>

#include "obiecte.h"
#include "utiliz.h"
#include "gestiune.h"

void main()
{
	clrscr();

/*	Carte *c1 = new Carte(10, 250, 1986);
	Carte *c2 = new Carte(24, 300, 1990);
	NumarRevista *c3 = new NumarRevista(32, 46, 2002);
	NumarRevista *c4 = new NumarRevista(2, 46, 2002);

	GestiuneInventar *gest = new GestiuneInventar();

	c1->setAutor("Ion Creanga");
	c1->setTitlu("Aminitiri din Copilarie");
	c1->setGen("epic");
	c2->setAutor("Mihai Eminescu");
	c2->setTitlu("Poezii");
	c2->setGen("liric");
	c3->setNumar(4);
	c3->setIdRevista(10);
	c4->setNumar(5);
	c4->setIdRevista(10);

//	printf("%s", stud->toString());

	FILE *f;

	if ((f = fopen("c:\\xyz.txt", "w")) == NULL)
	{
		printf("Eroare la deschiderea fisierului.");
		getch();
		exit(1);
	}

	gest->adaugaObiect(c1);
	gest->adaugaObiect(c2);
	gest->adaugaObiect(c3);
	gest->adaugaObiect(c4);

	gest->serialize(f);

	fclose(f);*/

	FILE *f;

	if ((f = fopen("c:\\xyz.txt", "r")) == NULL)
	{
		printf("Eroare la deschiderea fisierului.");
		getch();
		exit(1);
	}

	GestiuneInventar *gest = new GestiuneInventar();
	gest->deserialize(f);

	for (int i = 0; i < gest->getNrObiecte(); i++)
	{
		printf("Obiectul %d:\n%s\n\n", i, gest->getObiect(i)->toString());
		getch();
	}

	if (!getch())
		getch();
}