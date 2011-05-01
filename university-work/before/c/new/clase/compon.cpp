#include "compon.h"

Component::Component(int x)
{
   i= x;
}

Component::~Component()
{}

int Component::operator==(const Component& c) const
{
   return (i == c.i);
}