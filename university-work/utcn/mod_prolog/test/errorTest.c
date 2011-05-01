#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <g++/iostream.h>

int main()
{
        FILE *f;
        f = fopen("/dev/fd0", "r");
        cout << strerror(errno) << "\n";
        perror("");
}
