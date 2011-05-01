#include <dos.h>
#include <stdio.h>
#include <conio.h>

Do_(int i)
{
   if (i == 1)
      sound(262);
   if (i == 2)
      sound(524);
}

Do1(int i)
{
   if (i == 1)
      sound(278);
   if (i == 2)
      sound(556);
}

Re(int i)
{
   if (i == 1)
      sound(294);
   if (i == 2)
      sound(588);
}

Re1(int i)
{
   if (i == 1)
      sound(312);
   if (i == 2)
      sound(624);
}

Mi(int i)
{
   if (i == 1)
      sound(330);
   if (i == 2)
      sound(660);
}

Fa(int i)
{
   if (i == 1)
      sound(350);
   if (i == 2)
      sound(700);
}

Fa1(int i)
{
   if (i == 1)
      sound(370);
   if (i == 2)
      sound(740);
}

Sol(int i)
{
   if (i == 1)
      sound(392);
   if (i == 2)
      sound(784);
}

Sol1(int i)
{
   if (i == 1)
      sound(416);
   if (i == 2)
      sound(832);
}

La(int i)
{
   if (i == 1)
      sound(440);
   if (i == 2)
      sound(880);
}

La1(int i)
{
   if (i == 1)
      sound(466);
   if (i == 2)
      sound(932);
}

Si(int i)
{
   if (i == 1)
      sound(494);
   if (i == 2)
      sound(988);
}

conquest_of_paradise()
{
   Mi(1); delay(400); nosound();
   Do_(2); delay(400); nosound();
   Si(1); delay(400); nosound();
   La(1); delay(400); nosound();
   Sol1(1); delay(400); nosound();
   La(1); delay(400); nosound();
   Si(1); delay(400); nosound();
   Sol1(1); delay(400); nosound();
   Mi(1); delay(400); nosound();
   Mi(1); delay(400); nosound();
   Do_(2); delay(400); nosound();
   Si(1); delay(400); nosound();
   La(1); delay(400); nosound();
   Sol1(1); delay(400); nosound();
   La(1); delay(400); nosound();
   Si(1); delay(400); nosound();
   Sol(1); delay(400); nosound();
   Mi(2); delay(400); nosound();
   Re(2); delay(400); nosound();
   Do_(2); delay(400); nosound();
   Si(2); delay(400); nosound();
   Do_(2); delay(400); nosound();
   Re(2); delay(400); nosound();
   Si(2); delay(400); nosound();
   Sol(1); delay(400); nosound();
   Sol(1); delay(400); nosound();
   Fa(1); delay(400); nosound();
   Sol(1); delay(400); nosound();
   La(1); delay(400); nosound();
   Sol(1); delay(400); nosound();
   Fa(1); delay(400); nosound();
   Mi(1); delay(400); nosound();
}

baa_baa_black_sheep()
{
   Fa(1); delay(450);
   nosound();
   Fa(1); delay(450);
   Do_(2); delay(450);
   nosound();
   Do_(2); delay(450);
   Re(2); delay(225);
   Mi(2); delay(225);
   Fa(2); delay(225);
   Re(2); delay(225);
   Do_(2); delay(450);
   nosound();
   delay(450);
   La1(1); delay(450);
   nosound();
   La1(1); delay(450);
   La(1); delay(450);
   nosound();
   La(1); delay(450);
   Sol(1); delay(450);
   nosound();
   Sol(1); delay(450);
   Fa(1); delay(675);
   nosound();
   delay(225);
   Do_(2); delay(450);
   nosound();
   Do_(2); delay(225);
   nosound();
   Do_(2); delay(225);
   La1(1); delay(450);
   nosound();
   La1(1); delay(225);
   nosound();
   La1(1); delay(225);
   La(1); delay(450);
   nosound();
   La(1); delay(225);
   nosound();
   La(1); delay(225);
   Sol(1); delay(450);
   nosound();
   delay(225);
   Do_(2); delay(225);
   nosound();
   Do_(2); delay(450);
   nosound();
   Do_(2); delay(225);
   nosound();
   Do_(2); delay(225);
   La1(1); delay(225);
   Do_(2); delay(225);
   Re(2); delay(225);
   La1(1); delay(225);
   La(1); delay(450);
   Sol(1); delay(225);
   nosound();
   Sol(1); delay(225);
   Fa(1); delay(900);
}

int main(void)
{

   conquest_of_paradise();
//   baa_baa_black_sheep();

   nosound();
}
