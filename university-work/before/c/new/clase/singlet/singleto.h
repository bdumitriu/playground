#if !defined(__SINGLETO_H__)
#define __SINGLETO_H__

class Singleton
{
private:
	static Singleton* instance;
	int x;

protected:
   Singleton(int x = 5);

public:
   static Singleton* GetInstance();
   void Afisare();
};

#endif