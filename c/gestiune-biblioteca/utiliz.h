#ifndef __UTILIZ_H_
#define __UTILIZ_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

class Utilizator
{
public:
   // constructori
	Utilizator();
   Utilizator(int id, char *nume, char *sex);

   // destructor
   ~Utilizator();

   // setteri
   void setId(int id);
   void setNume(char *nume);
   void setSex(char *sex);

   // getteri
   int getId();
   char* getNume();
   char* getSex();
   int getNrObiecte();

   // intoarce 1 daca utilizatorul are obiectul identificat prin
   // idObiect imprumutat si 0 altfel.
   int imprumutata(int idObiect);

   int imprumuta(int idObiect);
   int restituie(int idObiect);

   // metode acces fisier
   void serialize(FILE *f);
   void deserialize(FILE *f);

protected:
	char sex[10];
	char nume[20];
	int id;
	int idObiecte[5];
	int nrObiecte;
};

class UtilizatorIntern: public Utilizator
{
public:
   // constructori
	UtilizatorIntern();
   UtilizatorIntern(int id, char *nume, char *sex, char *facultate);

   // destructor
   ~UtilizatorIntern();

   // setter & getter
   void setFacultate(char *facultate);
   char* getFacultate();

   // metode acces fisier
   void serialize(FILE *f);
   void deserialize(FILE *f);

protected:
	char facultate[40];
};

class UtilizatorExtern: public Utilizator
{
public:
	// constructori
   UtilizatorExtern();
   UtilizatorExtern(int id, char *nume, char *sex, char *domiciliu);

   // destructor
   ~UtilizatorExtern();

   // setter & getter
   void setDomiciliu(char *domiciliu);
   char* getDomiciliu();

   // metode acces fisier
   void serialize(FILE *f);
   void deserialize(FILE *f);

	char* toString();

protected:
	char domiciliu[40];
};

class UtilizatorStudent: public UtilizatorIntern
{
public:
	// constructori
   UtilizatorStudent();
   UtilizatorStudent(int id, char *nume, char *sex, char *facultate);

   // destructor
   ~UtilizatorStudent();

   // setteri
   void setAn(int an);
   void setGrupa(int grupa);
   void setSectie(char *sectie);

   // getteri
   int getAn();
   int getGrupa();
   char* getSectie();

   // metode acces fisier
   void serialize(FILE *f);
   void deserialize(FILE *f);

	char* toString();

protected:
	int an;
	int grupa;
	char sectie[20];
};

class UtilizatorCadruDidactic: public UtilizatorIntern
{
public:
   // constructori
   UtilizatorCadruDidactic();
   UtilizatorCadruDidactic(int id, char *nume, char *sex, char *facultate);

   // destructor
   ~UtilizatorCadruDidactic();

   // setteri
   void setTitlu(char *titlu);
   void setCatedra(char *catedra);

   // getteri
   char* getTitlu();
   char* getCatedra();

   // metode aces fisier
   void serialize(FILE *f);
   void deserialize(FILE *f);

	char* toString();

protected:
	char titlu[15];
	char catedra[25];
};

#endif