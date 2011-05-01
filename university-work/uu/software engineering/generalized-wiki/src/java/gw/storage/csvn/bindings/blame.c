#include "blame.h"

/* This file needs some refactoring!!!

   This blame function is compatible with all subversion servers, because i do 
   not use the the ra_lib->get_file_revs (which is supported since version
   1.1). I think this version is slower!!!
*/


/* Remove a temporary file F, which is of type apr_file_t *.  First, try
   to close the file, ignoring any errors.  Return an error if the remove
   fails. */
static apr_status_t
cleanup_tempfile (void *f)
{
  apr_file_t *file = f;
  apr_status_t apr_err;
  const char *fname;

  /* the file may or may not have been closed; try it */
  apr_file_close (file);

  apr_err = apr_file_name_get (&fname, file);
  if (apr_err == APR_SUCCESS)
    apr_err = apr_file_remove (fname, apr_file_pool_get (file));

  return apr_err;
}


/* Create a blame chunk associated with REV for a change starting
   at token START. */
static struct blame *
blame_create (struct file_rev_baton *baton, struct rev *rev, apr_off_t start)
{
  struct blame *blame;
  if (baton->avail)
    {
      blame = baton->avail;
      baton->avail = blame->next;
    }
  else
    blame = apr_palloc (baton->mainpool, sizeof (*blame));
  blame->rev = rev;
  blame->start = start;       
  blame->next = NULL;
  return blame;
}

/* Destroy a blame chunk. */
static void
blame_destroy (struct file_rev_baton *baton, struct blame *blame)
{
  blame->next = baton->avail;
  baton->avail = blame;
}

/* Return the blame chunk that contains token OFF, starting the search at
   BLAME. */
static struct blame *
blame_find (struct blame *blame, apr_off_t off)
{
  struct blame *prev = NULL;
  while (blame)
    {
      if (blame->start > off) break;
      prev = blame;
      blame = blame->next;
    }
  return prev;
}

/* Shift the start-point of BLAME and all subsequence blame-chunks
   by ADJUST tokens */
static void
blame_adjust (struct blame *blame, apr_off_t adjust)
{
  while (blame)
    {
      blame->start += adjust;
      blame = blame->next;
    }
}

/* Delete the blame associated with the region from token START to
   START + LENGTH */
static svn_error_t *
blame_delete_range (struct file_rev_baton *db, apr_off_t start,
                    apr_off_t length)
{
  struct blame *first = blame_find (db->blame, start);
  struct blame *last = blame_find (db->blame, start + length);
  struct blame *tail = last->next;

  if (first != last)
    {
      struct blame *walk = first->next;
      while (walk != last)
        {
          struct blame *next = walk->next;
          blame_destroy (db, walk);
          walk = next;
        }
      first->next = last;
      last->start = start;
      if (first->start == start)
        {
          *first = *last;
          blame_destroy (db, last);
          last = first;
        }
    }

  if (tail && tail->start == last->start + length)
    {
      *last = *tail;
      blame_destroy (db, tail);
      tail = last->next;
    }

  blame_adjust (tail, -length);

  return SVN_NO_ERROR;
}

/* Insert a chunk of blame associated with DB->REV starting
   at token START and continuing for LENGTH tokens */
static svn_error_t *
blame_insert_range (struct file_rev_baton *db, apr_off_t start,
                    apr_off_t length)
{
  struct blame *head = db->blame;
  struct blame *point = blame_find (head, start);
  struct blame *insert;

  if (point->start == start)
    {
      insert = blame_create (db, point->rev, point->start + length);
      point->rev = db->rev;
      insert->next = point->next;
      point->next = insert;
    }
  else if (!point->next || point->next->start > start + length)
    {
      struct blame *middle;
      middle = blame_create (db, db->rev, start);
      insert = blame_create (db, point->rev, start + length);
      middle->next = insert;
      insert->next = point->next;
      point->next = middle;
    }
  else
    {
      insert = blame_create (db, db->rev, start);
      insert->next = point->next;
      point->next = insert;
    }
  blame_adjust (insert->next, length);

  return SVN_NO_ERROR;
}

/* Callback for diff between subsequent revisions */
static svn_error_t *
output_diff_modified (void *baton,
                      apr_off_t original_start,
                      apr_off_t original_length,
                      apr_off_t modified_start,
                      apr_off_t modified_length,
                      apr_off_t latest_start,
                      apr_off_t latest_length)
{
  struct file_rev_baton *db = baton;

  if (original_length)
    SVN_ERR (blame_delete_range (db, modified_start, original_length));

  if (modified_length)
    SVN_ERR (blame_insert_range (db, modified_start, modified_length));

  return SVN_NO_ERROR;
}

static const svn_diff_output_fns_t output_fns = {
        NULL,
        output_diff_modified
};


svn_error_t *
svn_client__prev_log_path (const char **prev_path_p,
                           char *action_p,
                           svn_revnum_t *copyfrom_rev_p,
                           apr_hash_t *changed_paths,
                           const char *path,
                           svn_node_kind_t kind,
                           svn_revnum_t revision,
                           apr_pool_t *pool)
{
  svn_log_changed_path_t *change;
  const char *prev_path = NULL;

  /* It's impossible to find the predecessor path of a NULL path. */
  assert(path);

  /* Initialize our return values for the action and copyfrom_rev in
     case we have an unhandled case later on. */
  if (action_p)
    *action_p = 'M';
  if (copyfrom_rev_p)
    *copyfrom_rev_p = SVN_INVALID_REVNUM;

  /* See if PATH was explicitly changed in this revision. */
  change = apr_hash_get (changed_paths, path, APR_HASH_KEY_STRING);
  if (change)
    {
      /* If PATH was not newly added in this revision, then it may or may
         not have also been part of a moved subtree.  In this case, set a
         default previous path, but still look through the parents of this
         path for a possible copy event. */
      if (change->action != 'A' && change->action != 'R')
        {
          prev_path = path;
        }
      else
        {
          /* PATH is new in this revision.  This means it cannot have been
             part of a copied subtree. */
          if (change->copyfrom_path)
            prev_path = apr_pstrdup (pool, change->copyfrom_path);
          else
            prev_path = NULL;
          
          *prev_path_p = prev_path;
          if (action_p)
            *action_p = change->action;
          if (copyfrom_rev_p)
            *copyfrom_rev_p = change->copyfrom_rev;
          return SVN_NO_ERROR;
        }
    }
  
  if (apr_hash_count (changed_paths))
    {
      /* The path was not explicitly changed in this revision.  The
         fact that we're hearing about this revision implies, then,
         that the path was a child of some copied directory.  We need
         to find that directory, and effectively "re-base" our path on
         that directory's copyfrom_path. */
      int i;
      apr_array_header_t *paths;

      /* Build a sorted list of the changed paths. */
      paths = svn_sort__hash (changed_paths,
                              svn_sort_compare_items_as_paths, pool);

      /* Now, walk the list of paths backwards, looking a parent of
         our path that has copyfrom information. */
      for (i = paths->nelts; i > 0; i--)
        {
          svn_sort__item_t item = APR_ARRAY_IDX (paths,
                                                 i - 1, svn_sort__item_t);
          const char *ch_path = item.key;
          int len = strlen (ch_path);

          /* See if our path is the child of this change path.  If
             not, keep looking.  */
          if (! ((strncmp (ch_path, path, len) == 0) && (path[len] == '/')))
            continue;

          /* Okay, our path *is* a child of this change path.  If
             this change was copied, we just need to apply the
             portion of our path that is relative to this change's
             path, to the change's copyfrom path.  Otherwise, this
             change isn't really interesting to us, and our search
             continues. */
          change = apr_hash_get (changed_paths, ch_path, len);
          if (change->copyfrom_path)
            {
              if (action_p)
                *action_p = change->action;
              if (copyfrom_rev_p)
                *copyfrom_rev_p = change->copyfrom_rev;
              prev_path = svn_path_join (change->copyfrom_path, 
                                         path + len + 1, pool);
              break;
            }
        }
    }

  /* If we didn't find what we expected to find, return an error.
     (Because directories bubble-up, we get a bunch of logs we might
     not want.  Be forgiving in that case.)  */
  if (! prev_path)
    {
      if (kind == svn_node_dir)
        prev_path = apr_pstrdup (pool, path);
      else
        return svn_error_createf (SVN_ERR_CLIENT_UNRELATED_RESOURCES, NULL,
                                  ("Missing changed-path information for "
                                    "'%s' in revision %ld"),
                                  path, revision);
    }
  
  *prev_path_p = prev_path;
  return SVN_NO_ERROR;
}


/* Callback for log messages: accumulates revision metadata into
   a chronologically ordered list stored in the baton. */
static svn_error_t *
log_message_receiver (void *baton,
                      apr_hash_t *changed_paths,
                      svn_revnum_t revision,
                      const char *author,
                      const char *date,
                      const char *message,
                      apr_pool_t *pool)
{
  struct log_message_baton *lmb = baton;
  struct rev *rev;

  rev = apr_palloc (lmb->pool, sizeof (*rev));
  rev->revision = revision;
  rev->author = apr_pstrdup (lmb->pool, author);
  rev->date = apr_pstrdup (lmb->pool, date);
  rev->path = lmb->path;
  rev->next = lmb->eldest;
  lmb->eldest = rev;

  SVN_ERR (svn_client__prev_log_path (&lmb->path, &lmb->action,
                                      &lmb->copyrev, changed_paths,
                                      lmb->path, svn_node_file, revision,
                                      lmb->pool));

  return SVN_NO_ERROR;
}

/* Add the blame for the diffs between LAST_FILE and CUR_FILE with the rev
   specified in FRB.  LAST_FILE may be NULL in which
   case blame is added for every line of CUR_FILE. */
static svn_error_t *
add_file_blame (const char *last_file, const char *cur_file,
                struct file_rev_baton *frb)
{
  if (!last_file)
    {
      assert (frb->blame == NULL);
      frb->blame = blame_create (frb, frb->rev, 0);
    }
  else
    {
      svn_diff_t *diff;

      /* We have a previous file.  Get the diff and adjust blame info. */
      SVN_ERR (svn_diff_file_diff (&diff, last_file, cur_file,
                                   frb->currpool));
      SVN_ERR (svn_diff_output (diff, frb, &output_fns));
    }

  return SVN_NO_ERROR;
}


static svn_error_t *
do_blame (const char *target, 
           svn_ra_plugin_t *ra_lib,
           void *ra_session,
           struct file_rev_baton *frb)
{
  struct log_message_baton lmb;
  apr_array_header_t *condensed_targets;
  apr_file_t *file;
  svn_stream_t *stream;
  struct rev *rev;
  svn_node_kind_t kind;
  apr_pool_t *pool = frb->mainpool;

  SVN_ERR (ra_lib->check_path (ra_session, target, frb->end_rev, &kind, pool));

  if (kind == svn_node_dir) 
    return svn_error_createf (SVN_ERR_CLIENT_IS_DIRECTORY, NULL,
                              ("URL '%s' refers to a directory"), target); 

  condensed_targets = apr_array_make (pool, 1, sizeof (const char *));
  (*((const char **)apr_array_push (condensed_targets))) = "";

  lmb.path = apr_pstrcat(pool, "/", target, NULL);
  lmb.eldest = NULL;
  lmb.pool = pool;

  /* Accumulate revision metadata by walking the revisions
     backwards; this allows us to follow moves/copies
     correctly. */
  SVN_ERR (ra_lib->get_log (ra_session,
                            condensed_targets,
                            frb->end_rev,
                            frb->start_rev,
                            TRUE,
                            FALSE,
                            log_message_receiver,
                            &lmb,
                            pool));

  /* Inspect the first revision's change metadata; if there are any
     prior revisions, compute a new starting revision/path.  If no
     revisions were selected, no blame is assigned.  A modified
     item certainly has a prior revision.  It is reasonable for an
     added item to have none, but anything else is unexpected.  */
  if (!lmb.eldest)
    {
      lmb.eldest = apr_palloc (pool, sizeof (*rev));
      lmb.eldest->revision = frb->end_rev;
      lmb.eldest->path = lmb.path;
      lmb.eldest->next = NULL;
      rev = apr_palloc (pool, sizeof (*rev));
      rev->revision = SVN_INVALID_REVNUM;
      rev->author = NULL;
      rev->date = NULL;
      frb->blame = blame_create (frb, rev, 0);
    }
  else if (lmb.action == 'M' || SVN_IS_VALID_REVNUM (lmb.copyrev))
    {
      rev = apr_palloc (pool, sizeof (*rev));
      if (SVN_IS_VALID_REVNUM (lmb.copyrev))
        rev->revision = lmb.copyrev;
      else
        rev->revision = lmb.eldest->revision - 1;
      rev->path = lmb.path;
      rev->next = lmb.eldest;
      lmb.eldest = rev;
      rev = apr_palloc (pool, sizeof (*rev));
      rev->revision = SVN_INVALID_REVNUM;
      rev->author = NULL;
      rev->date = NULL;
      frb->blame = blame_create (frb, rev, 0);
    }
  else if (lmb.action == 'A')
    {
      frb->blame = blame_create (frb, lmb.eldest, 0);
    }
  else
    return svn_error_createf (APR_EGENERAL, NULL,
                              ("Revision action '%c' for "
                                "revision %ld of '%s' "
                                "lacks a prior revision"),
                              lmb.action, lmb.eldest->revision,
                              lmb.eldest->path);

  /* Walk the revision list in chronological order, downloading
     each fulltext, diffing it with its predecessor, and accumulating
     the blame information into db.blame.  Use two iteration pools
     rather than one, because the diff routines need to look at a
     sliding window of revisions.  Two pools gives us a ring buffer
     of sorts. */
  for (rev = lmb.eldest; rev; rev = rev->next)
    {
      const char *tmp;
      const char *temp_dir;
      apr_hash_t *props;
      svn_string_t *mimetype;
      
      apr_pool_clear (frb->currpool);
      SVN_ERR (svn_io_temp_dir (&temp_dir, frb->currpool));
      SVN_ERR (svn_io_open_unique_file (&file, &tmp,
                 svn_path_join (temp_dir, "tmp", frb->currpool), ".tmp",
                                        FALSE, frb->currpool));

      apr_pool_cleanup_register (frb->currpool, file, cleanup_tempfile,
                                 apr_pool_cleanup_null);

      stream = svn_stream_from_aprfile (file, frb->currpool);
      SVN_ERR (ra_lib->get_file(ra_session, rev->path + 1, rev->revision,
                                stream, NULL, &props, frb->currpool));
      SVN_ERR (svn_stream_close (stream));
      SVN_ERR (svn_io_file_close (file, frb->currpool));

      /* If this file has a non-textual mime-type, bail out. */
      if (props && 
          ((mimetype = apr_hash_get (props, SVN_PROP_MIME_TYPE, 
                                     sizeof (SVN_PROP_MIME_TYPE) - 1))))
        {
          if (svn_mime_type_is_binary (mimetype->data))
            return svn_error_createf (SVN_ERR_CLIENT_IS_BINARY_FILE, 0,
               ("Cannot calculate blame information for binary file '%s'"),target);
        }

      if (frb->last_filename)
        {
          frb->rev = rev;
          SVN_ERR (add_file_blame (frb->last_filename, tmp, frb));
        }

      frb->last_filename = tmp;
      {
        apr_pool_t *tmppool = frb->currpool;
        frb->currpool = frb->lastpool;
        frb->lastpool = tmppool;
      }
    }

  return SVN_NO_ERROR;
}


svn_error_t *
svn_ra_blame (svn_ra_plugin_t *ra_lib, void *ra_session,
              const char *target,
              svn_revnum_t start_revnum,
              svn_revnum_t end_revnum,
              svn_client_blame_receiver_t receiver,
              void *receiver_baton,
              apr_pool_t *pool)
{
    struct file_rev_baton frb;
    struct blame *walk;
    apr_file_t *tempfile;
    apr_pool_t *iterpool;
    svn_stream_t *stream;
    svn_error_t *err;

    // SVN_INVALID_REVNUM (=head revision)
    if ((end_revnum < start_revnum) && (end_revnum != SVN_INVALID_REVNUM))
      return svn_error_create(SVN_ERR_CLIENT_BAD_REVISION, NULL,
                              ("Start revision must precede end revision"));

    frb.start_rev = start_revnum;
    frb.end_rev = end_revnum;
    frb.target = target;
    frb.last_filename = NULL;
    frb.blame = NULL;
    frb.avail = NULL;
    frb.mainpool = pool;

  /* The callback will flip the following two pools, because it needs
     information from the previous call.  Obviously, it can't rely on
     the lifetime of the pool provided by get_file_revs. */
    frb.lastpool = svn_pool_create (pool);
    frb.currpool = svn_pool_create (pool);

    // do the actual blame
    err = do_blame (target, ra_lib, ra_session, &frb);
    if (err) return err;

    /* Report the blame to the caller. */

    /* The callback has to have been called at least once. */
    assert (frb.last_filename != NULL);

    /* Create a pool for the iteration below. */
    iterpool = svn_pool_create (pool);

    /* Open the last file and get a stream. */
    err = svn_io_file_open (&tempfile, frb.last_filename, APR_READ, APR_OS_DEFAULT, pool);
    if (err) return err;
    stream = svn_stream_from_aprfile(tempfile, pool);

    /* Process each blame item. */
    for (walk = frb.blame; walk; walk = walk->next)
    {
        apr_off_t line_no;
        for (line_no = walk->start; !walk->next || line_no < walk->next->start; ++line_no)
        {
            svn_boolean_t eof;
            svn_stringbuf_t *sb;
            apr_pool_clear (iterpool);
            err = svn_stream_readline (stream, &sb, "\n", &eof, iterpool);
            if (err) return err;

            if (!eof || sb->len)
                SVN_ERR (receiver (receiver_baton, line_no, walk->rev->revision,
                         walk->rev->author, walk->rev->date,
                         sb->data, iterpool));
            if (eof) break;
        }
    }

    err = svn_stream_close (stream);
    err = svn_io_file_close(tempfile, pool);
    svn_pool_destroy(frb.lastpool);
    svn_pool_destroy(frb.currpool);
    svn_pool_destroy(iterpool);
    return SVN_NO_ERROR;
}


