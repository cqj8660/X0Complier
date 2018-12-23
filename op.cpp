
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
int main(int argc, char * argv[])
{
	// freopen("fstack.txt", "r", stdin);
	FILE* fin;
	fin = fopen("fstack.txt", "r");
	char s[200];
	if(argc == 2)
	{
		while(fgets(s, 200, fin))
		{
			// char* p = (char*)s.c_str();
			s[strlen(s) - 1] = 0;
			printf("%s", s);
			putchar('\r');			
			usleep(500000);
			fflush(stdout);
		}
		putchar('\r');
	}
	else
	{
		while(getchar())
		{
			if(fgets(s, 200, fin) == NULL)
				break;
			s[strlen(s) - 1] = 0;
			printf("%s", s);
			putchar('\r');	
			fflush(stdout);
		}
	}
	puts("解释完成!");
	fclose(fin);
	return 0;
}
