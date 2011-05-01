int calc_lung(char *s)
{
   int i=0;

   while (s[i])
      i = i+1;

   return i;
}

int calc_lung2(char *s)
{
   char *p = s;

   while(*p)
      p = p+1;

   return p-s;
}

main()
{
   char s[100]="abc";
   printf("\n%i",calc_lung2(s));

}