#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

void test_pipe();
void test_named_pipe();
void test_environment(char** envp);
void test_sigsegv();
void test_sigkill();
void test_sigalarm();

int main(int argc, char* argv[], char** envp)
{
	test_sigalarm();

	return 0;
}

void test_anon_pipe()
{
	char buf[11];
	int fd[2];
	pipe(fd);

	if (fork() == 0)
	{
		strcpy(buf, "piping...\n");
		write(fd[1], buf, 11);
	}
	else
	{
		read(fd[0], buf, 11);
		printf("%s", buf);
	}
}

void test_named_pipe()
{
	char buf[11];
	int fd;

	if (fork() == 0)
	{
		mkfifo("/tmp/fifo_254", S_IRWXU);
		fd = open("/tmp/fifo_254", O_WRONLY);
		strcpy(buf, "piping...\n");
		write(fd, buf, 11);
	}
	else
	{
		fd = open("/tmp/fifo_254", O_RDONLY);
		read(fd, buf, 11);
		printf("%s", buf);
	}
}

void test_environment(char** envp)
{
	int k = 0;

	while (envp[k] != NULL)
	{
		printf("the %d(st|nd|rd|th) environment variable is %s.\n", k+1, envp[k]);
		k++;
	}
}

void manage_sigsegv(int sig_no)
{
	printf("got SIGSEGV... ending...\n");
	exit(0);
}

void test_sigsegv()
{
	char k[1];
	int i;
	char x;

	if (signal(SIGSEGV, manage_sigsegv))
	{
		perror("signal");
		return;
	}

	for (i = 0; ; i++)
	{
		x = k[i];
	}
}

void manage_sigusr1(int sig_no)
{
	return;
}

void test_sigkill()
{
	int pid;
	if ((pid = fork()) == -1)
	{
		perror("fork");
		return;
	}
	if (pid > 0) /* parent */
	{
		sleep(2);
		kill(pid, SIGUSR1);
		printf("Parent sent signal.\n");
	}
	else
	{
		signal(SIGUSR1, manage_sigusr1);
		pause();
		printf("Child got signal.\n");
	}
}

void manage_sigalarm(int sig_no)
{
	return;
}

/**
 * This is not working as expected because alarm doesn't interrupt
 * read() (as it should?)
 */
void test_sigalarm()
{
	char buf[80];
	int k;

	signal(SIGALRM, manage_sigalarm);
	printf("Print 80 characters in 10 seconds. You've got 5 tries.\n");
	for (k = 5; k > 0; k--)
	{
		printf("You've got %d tries left.\n", k);
		alarm(1);
		if (read(0, buf, 80) == -1)
		{
			if (errno == EINTR)
			{
				printf("Try again.\n");
			}
			else
			{
				perror("read");
				return;
			}
		}
		else
		{
			break;
		}
	}

	if (k > 0)
	{
		printf("You made it!\n");
	}
	else
	{
		printf("You failed!\n");
	}
}
