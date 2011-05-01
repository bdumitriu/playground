#include "ent_gr.h"

EntitateGrafica::EntitateGrafica()
{
   x = y = 0;
}

EntitateGrafica::EntitateGrafica(int x, int y)
{
   this->x = x;
   this->y = y;
}

EntitateGrafica::~EntitateGrafica()
{}

int EntitateGrafica::X() const
{
   return x;
}

int EntitateGrafica::Y() const
{
   return y;
}

void EntitateGrafica::Pozitionare(int x, int y)
{
   this->x = x;
   this->y = y;
}