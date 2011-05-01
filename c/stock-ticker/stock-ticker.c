#include <stdio.h>
#include <string.h>

/* The BVB Statistics site */
const char* SITE = "http://www.bvb.ro/cgi-bin/bse_top.cgi?select_top=66&select_leng=ENG&select_usercode=basic&select_usertype=basic&test=145343";

/* Full path (including program name) to curl utility */
//const char* CURL = "D:\\TEMP\\curl\\curl";
const char* CURL = "curl";

/* The local file to use for saving the html page */
//const char* LOCAL_FILE = "c:\\bvb-stats.html";
const char* LOCAL_FILE = "/home/bdumitriu/bvb-stats.html";

/* The interval at which the ticker should be updated (in seconds) */
const int UPDATE_INTERVAL_IN_SECONDS = 10;

/* The symbol */
const char* SYMBOL = "TLV";

typedef struct
{
  char* symbol;
  long  volume_of_transactions;
  float value_of_transactions;
  int   trades;
  float last_price;
  float reference_price;
  float net_change;
  float percent_change;
} ticker;

/*
 * Allocates <size> characters for <*buffer> and checks if allocation
 * was successful. If not, it aborts. 
 */
void malloc_chars(char** buffer, const int size)
{
  *buffer = (char*) malloc(size * sizeof(char));
  if (*buffer == 0)
  {
    abort();
  }
}

/*
 * Removes the tags from a string of this particular form
 * (you can have anything except a '>' character instead
 * of "..."):
 *
 *     <...><...>some-value</font></td>
 *
 * The <*buffer> is modified such that after execution of
 * this function it only contains the string "some-value"
 * (of course, there will be some (other) value there, not
 * the actual string "some-value").
 */
void remove_tags(char** buffer)
{
  char* start_of_buffer = *buffer;

  *buffer = strchr(*buffer, '>');
  (*buffer)++;
  *buffer = strchr(*buffer, '>');
  (*buffer)++;
  (*buffer)[strlen(*buffer)-13] = '\0';

  strncpy(start_of_buffer, *buffer, strlen(*buffer)+1);
  *buffer = start_of_buffer;
}

/*
 * Removes all the commas from <*buffer>.
 */
void remove_commas(char** buffer)
{
  int i, j, n;
  char* tmp;

  n = strlen(*buffer);
  malloc_chars(&tmp, n+1);
  j = 0;
  for (i = 0; i < n; i++)
  {
    if ((*buffer)[i] != ',')
    {
      tmp[j++] = (*buffer)[i];
    }
  }
  tmp[j++] = '\0';
  strncpy(*buffer, tmp, j);

  free(tmp);
}

/*
 * Takes a stock symbol and a file with a very particular format
 * as paramters and returns a pointer to a ticker structure. The
 * ticker structure will contain the values associated with the
 * given <symbol>, as specified in the file <file_name>. If the
 * symbol doesn't exist in this file, all fields of the ticker
 * structure (except the symbol field) will be 0. The symbol field
 * will always be a copy of the <symbol> input parameter.
 */
ticker* get_ticker(const char* symbol, const char* file_name)
{
  FILE* f;
  char* buffer;
  char* line;
  ticker* result;
  int n = 1000;
  int endloop = 0;

  if ((f = fopen(file_name, "r")) == 0)
  {
    printf("Couldn't open %s.", file_name);
    abort();
  }

  result = (ticker*) malloc(sizeof(ticker));
  malloc_chars(&result->symbol, strlen(SYMBOL)+1);
  malloc_chars(&buffer, n);

  strcpy(result->symbol, SYMBOL);
  result->volume_of_transactions = 0;
  result->value_of_transactions = 0.0;
  result->trades = 0;
  result->last_price = 0.0;
  result->reference_price = 0.0;
  result->net_change = 0.0;
  result->percent_change = 0.0;

  while (!feof(f) && !endloop)
  {
    getline(&buffer, &n, f);
    line = strstr(buffer, symbol);
    if (line != 0)
    {
      // get the volume of transactions
      getline(&buffer, &n, f);
      getline(&buffer, &n, f);
      remove_tags(&buffer);
      remove_commas(&buffer);
      sscanf(buffer, "%ld", &result->volume_of_transactions);

      // get the value of transactions
      getline(&buffer, &n, f);
      getline(&buffer, &n, f);
      remove_tags(&buffer);
      remove_commas(&buffer);
      sscanf(buffer, "%f", &result->value_of_transactions);

      // get the number of trades
      getline(&buffer, &n, f);
      getline(&buffer, &n, f);
      remove_tags(&buffer);
      remove_commas(&buffer);
      sscanf(buffer, "%d", &result->trades);

      // get the last price
      getline(&buffer, &n, f);
      getline(&buffer, &n, f);
      remove_tags(&buffer);
      remove_commas(&buffer);
      sscanf(buffer, "%f", &result->last_price);

      // get the reference price
      getline(&buffer, &n, f);
      getline(&buffer, &n, f);
      remove_tags(&buffer);
      remove_commas(&buffer);
      sscanf(buffer, "%f", &result->reference_price);

      // get the net change
      getline(&buffer, &n, f);
      getline(&buffer, &n, f);
      remove_tags(&buffer);
      remove_commas(&buffer);
      sscanf(buffer, "%f", &result->net_change);

      // get the percent change
      getline(&buffer, &n, f);
      getline(&buffer, &n, f);
      remove_tags(&buffer);
      remove_commas(&buffer);
      sscanf(buffer, "%f", &result->percent_change);

      endloop = 1;
    }
  }

  fclose(f);
  free(buffer);

  return result;
}

void do_something_with_ticker(ticker* ticker)
{
  printf("Symbol:                 %s\n", ticker->symbol);
  printf("Volume of transactions: %ld\n", ticker->volume_of_transactions);
  printf("Value of transactions:  %.2f\n", ticker->value_of_transactions);
  printf("Number of trades:       %d\n", ticker->trades);
  printf("Last price:             %.4f\n", ticker->last_price);
  printf("Reference price:        %.4f\n", ticker->reference_price);
  printf("Net change:             %.4f\n", ticker->net_change);
  printf("Percent change:         %.2f\n\n", ticker->percent_change);
}

int main()
{
  char* command;
  ticker* ticker;

  malloc_chars(&command, 1000);
  
  // create command string (something like "curl -o file http://site")
  strcat(command, CURL);
  strcat(command, " -o ");
  strcat(command, LOCAL_FILE);
  strcat(command, " ");
  strcat(command, SITE);

  while (1)
  {
    system(command);

    // wait for file to get downloaded
    sleep(10);

    ticker = get_ticker(SYMBOL, LOCAL_FILE);
    do_something_with_ticker(ticker);
    free(ticker->symbol);
    free(ticker);

    sleep(UPDATE_INTERVAL_IN_SECONDS);
  }

  // although never reached, who cares?
  free(command);
}
