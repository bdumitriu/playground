#if !defined(__STR_H__)
#define __STR_H__

class String
{
public:
	String();
	String(char *c);
	String(int dim, char c = ' ');
	String(String& s);
	~String();

	int  Len();
	void Print(int s_idx, int f_idx);
	void SetDim(int dim = 255);
	void SetStringToChar(char c = ' ');

	char& operator[](int idx);

private:
	char *str;

};

#endif