#include <svn_client.h>
#include <svn_error.h>
#include <svn_diff.h>
#include <svn_pools.h>
#include <svn_path.h>
#include <svn_sorts.h>
#include <assert.h>

/* The metadata associated with a particular revision. */
struct rev
{
  svn_revnum_t revision; /* the revision number */
  const char *author;    /* the author of the revision */
  const char *date;      /* the date of the revision */
  /* Only used by the pre-1.1 code. */
  const char *path;      /* the absolute repository path */
  struct rev *next;      /* the next revision */
};


/* One chunk of blame */
struct blame
{
  struct rev *rev;    /* the responsible revision */
  apr_off_t start;    /* the starting diff-token (line) */
  struct blame *next; /* the next chunk */
};


/* The baton used for a file revision.  Also used for the diff output
   routine. */
struct file_rev_baton {
  svn_revnum_t start_rev, end_rev;
  const char *target;
  const char *last_filename;
  struct rev *rev;     /* the rev for which blame is being assigned
                          during a diff */
  struct blame *blame; /* linked list of blame chunks */
  struct blame *avail; /* linked list of free blame chunks */
  apr_pool_t *mainpool;  /* lives during the whole sequence of calls */
  apr_pool_t *lastpool;  /* pool used during previous call */
  apr_pool_t *currpool;  /* pool used during this call */
};


/* The baton used for RA->get_log */
struct log_message_baton {
  const char *path;        /* The path to be processed */
  struct rev *eldest;      /* The eldest revision processed */
  char action;             /* The action associated with the eldest */ 
  svn_revnum_t copyrev;    /* The revision the eldest was copied from */
  apr_pool_t *pool; 
};


svn_error_t *
svn_ra_blame (svn_ra_plugin_t *ra_lib, void *ra_session,
                  const char *target,
                  svn_revnum_t start_revnum,
                  svn_revnum_t end_revnum,
                  svn_client_blame_receiver_t receiver,
                  void *receiver_baton,
                  apr_pool_t *pool);
