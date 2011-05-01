#include <conio.h>
#include <stdio.h>
#include <iostream.h>

class Pozitie
{
public:
   Pozitie(int x = 0, int y = 0);
   ~Pozitie();

   virtual void afisare();
   void deplasare(int dx, int dy);

protected:
	int x, y;
};

Pozitie::Pozitie(int x, int y)
{
	this->x = x;
	this->y = y;
}

Pozitie::~Pozitie()
{
	cout << "Fuck you";
}

void Pozitie::afisare()
{
	cout << this;
	cout << "; pozitie: x = " << x;
	cout << ", y = " << y << "\n";
}

void Pozitie::deplasare(int dx, int dy)
{
	cout << "pozitia curenta: ";
	afisare();
	x += dx ; y += dy;
	afisare();
}

//clasa Pozitie cu explicitari

class Punct: public Pozitie
{
public:
   Punct(int x = 0, int y = 0, char culoare = 'A');
   ~Punct();

   void arata()
   { vizibil = 1; }

   void ascunde()
   { vizibil = 0; }

   void coloreaza(char c)
   { culoare = c; }

   void afisare();
   void deplasare(int dx, int dy);

protected:
   int vizibil; // 0 = invizibil
   char culoare;
};

Punct::Punct(int x, int y, char culoare)
: Pozitie(x, y)
{
	this->culoare = culoare;
}

Punct::~Punct()
{
	cout << "Fuck everybody!";
}

void Punct::afisare()
{
	cout << "x = " << x << ", y = " << y;
	cout << "\nculoare = " << culoare;
	if (vizibil)
		cout << "vizibil\n";
	else
		cout << "invizibil\n";
}

void Punct::deplasare(int dx, int dy)
{
	if (vizibil == 1)
	{
		cout << "Deplasare ";
		afisare();
	}
	x += dx; y += dy;
	if (vizibil == 1)
	{
		cout << "la cordonatele:";
		Pozitie::afisare();
	}
}

void main()
{
	clrscr();
	Pozitie *poz = new Pozitie();
	Punct *pct = new Punct();
	poz->afisare();
	pct->afisare();
	poz = pct;
	poz->afisare();
	poz->deplasare(10, 15);
}