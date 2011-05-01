package gw;

import java.util.ListResourceBundle;

/**
 * Container for all text resources (default version).
 * 
 * @author Patrick Camphuijsen
 * @author Jeroen Zuijderwijk
 */ 
public class TextResources extends ListResourceBundle {

   public Object[][] getContents() {
      return contents;
   }

   /**
   * Contains all text related resources for the wiki system
   */
   static final Object[][] contents = {
      {"ShowFile.SVNStatus", "svn status:"},

      {"ServletUtilities.NoFileModified","No file has been modified."},

      {"OkKey", "Ok!"},
      {"CancelKey", "Cancel"},
      {"YesKey", "Yes!"},
      {"NoKey", "No"},

      {"AttachFile.SelectFile","Select the file(s) you want to upload to"},
      {"AttachFile.FilesBiggerThan","Files bigger than "},
      {"AttachFile.AreIgnored","kB are ignored!"},
      {"AttachFile.MultipleFiles","If you want to upload multiple files, fill in the number (max 20)"},

	  {"ACLFile.submit", "Update these permissions please."},
	  {"ACLFile.title", "Change the ACL for the file here"},
	  
      {"Commit.changedProperties", "The following files have changed properties."},
      
	  {"HandleFile.MethodError", "Method error"},
      {"HandleFile.MethodNotImplemented", "This method is not implemented in this servlet"},

      {"HistoryFile.Of", "View the content of this file at revision: "},
      {"HistoryFile.Submit", "View"},
      
      {"CopyFileMoveFile.PathNotExists", "Path does not exist!"},
      {"CopyFileMoveFile.ExistingDirs", "Existing dirs"},
      {"CopyFileMoveFile.NewDirs", "New dir(s)"},
      {"CopyFileMoveFile.FileName", "Filename"},
      {"CopyFileMoveFile.ForceCreation", "Force creation if target path doesn't exists"},
      
      {"MoveFile.MoveFile", "Move this file"},
      {"MoveFile.Submit", "Move this file!"},
      {"MoveFile.ForceOverwrite", "Overwrite if file already exists"},

      {"CopyFile.CopyFile", "Copy this File"},
      {"CopyFile.Submit", "Copy the file"},
          
      {"RevertFile.Submit", "Revert to revision!"},
      {"RevertFile.To", "To which revision do you wish to revert?"},

      {"PraiseFile.Submit", "Praise between these revisions!"},

      {"PreviewFile.CommitAllChanges", "I want to commit all my changes"},
      {"PreviewFile.Submit", "Save Changes!"},

      {"EditFile.CreatedDirectory", "Created directory"},
      {"EditFile.PreviewChanges", "Preview Changes"},
      {"EditFile.NewTopic", "This is a new topic"},

      {"DiffFile.ChangeBetweenRevisions", "Changes between revision"},
      {"DiffFile.AndRevision", "and revision"},
      {"DiffFile.OfFile", "of file"},
      {"DiffFile.Submit", "Difference between these revisions!"},

      {"CommitFileSet.Submit", "Commit!"},
      {"CommitFileSet.MergeSubmit", "Please enter a message for this commit"},
      {"CommitFileSet.Resolve", "Resolve!"},
      {"CommitFileSet.NoConflicts", "There were no conflicts."},
      {"CommitFileSet.CommitMessage", "Please enter a commit message"},

      {"RevisionInfo.Information", "Revision Information"},
      {"RevisionInfo.NoLogs", "No log messages found"},
      {"RevisionInfo.AllLogs", "View all log messages from this revision to the last revision: "},
      {"RevisionInfo.Submit", "View"},

      {"Error.DontDeleteRoot", "Error: You can't delete the root."},
      {"Error.ActionNotAllowedOnRoot", "Error: This action is not allowed on the root."},
      {"Error.RevisionNotFound", "Error: The revision you referred to could not be found!"},
      {"Error.IntervalTooSmall", "Error: The requested interval is too small!"},
      {"Error.NoNumber", "Error: Please specify a number!"},
      {"Error.CouldNotGetFileList", "Error: Couldn't get the file list!"},
      {"Error.NoCommitMessage", "Error: You will have to fill in a commit message!"},
      {"Error.FileNotFound", "Error: The requested file could not be found!"},
      {"Error.CanNotEditDirectory", "Error: Can not edit directory"},
      {"Error.SameDirectory", "Error: The target directory is the same as the source directory. This move is impossible!"},
      {"Error.CanNotEditBinary", "Error: Binaries can not be edited!"},
      {"Error.CouldNotBeSaved", "Error: Could not be saved"},
      {"Error.NotDirectory","Error: Target is not a directory"},
      {"Error.CommitError","Commit error"},
      {"Error.CommitOnNonExistingFile","Commit can't be done on a non-existing file or directory !"},
      {"Error.StatusError","Status error"},
      {"Error.StatusOfNonExistingFile","Can't request the status of a non-existing file or directory !"},
      {"Error.AttachError","Couldn't attach file"},
      {"Error.AttachOnFile","Files can't be attached to files, only to directories !"},
      {"Error.AttachOnNonExisting","Files can't be attached to unexisting directories !"},
      
      {"Warning.MergeConflict", "Warning: These files have merge conflicts. Try editing the files!"},
      {"Warning.DeleteFile", "Warning: Are you sure you want to delete this file?"},
      {"Warning.DeleteDirectory", "Warning: Are you sure you want to delete this directory and all files in it?"},
      {"Warning.DirectoryNotFound", "Warning: The file could not be moved, because the directory does not exist. If you want to move the file to a non-existing directory, select force-creation"},
      {"Warning.FileExists", "Warning: the file could not be moved, because a file with the same name already exists."},

	  {"Text.Commit", "Commit"},
	  {"Text.History", "History"},
	  {"Text.Praise", "Praise"},
	  {"Text.Revert", "Revert"},
      {"Text.Delete", "Delete"},
      {"Text.Edit", "Edit"},
      {"Text.File", "File"},
      {"Text.Move", "Move"},
      {"Text.To", "to"},
      {"Text.Attach", "Attach"},
      {"Text.Upload", "Upload"},
      {"Text.Author", "Author"},
      {"Text.Changed", "Changed"},
      {"Text.Revision", "Revision"},
      {"Text.Content", "Content"},
      {"Text.Preview", "Preview"},
      {"Text.Line", "Line"},
      {"Text.NoPaths","No paths"},
      {"Text.Status", "Status"},
      {"Text.Directory", "Directory"},
      {"Text.LastRevision", "Last Revision"},
      {"Text.ContentType", "Content-type"},
      {"Text.View", "View"},

      {"Status.Added", "Added"},
      {"Status.Copied", "Copied"},
      {"Status.Deleted", "Deleted"},
      {"Status.Modified", "Modified"},
      {"Status.Normal", "Normal"}
   };
}