#include <g++-2/iostream.h>

int main()
{
	char *s;

	s = new char(20);
	cout << "What do you want to do today?\n";
	cin >> s;
	cout << "So you want to " << s << "\n";

	return 0;
}
