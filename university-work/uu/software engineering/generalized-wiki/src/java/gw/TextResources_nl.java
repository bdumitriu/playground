package gw;

import java.util.ListResourceBundle;

/**
 * Container for all text resources (Dutch version).
 * 
 * @author Patrick Camphuijsen
 * @author Jeroen Zuijderwijk
 */  
public class TextResources_nl extends ListResourceBundle {

   public Object[][] getContents() {
      return contents;
   }

   /**
   * Contains all text related resources for the wiki system (dutch version)
   */
   static final Object[][] contents = {      
      {"ShowFile.SVNStatus", "Svn status:"},

      {"ServletUtilities.NoFileModified","Er zijn geen bestanden gewijzigd."},

      {"OkKey", "Ok!"},
      {"CancelKey", "Annuleren"},
      {"YesKey", "Ja!"},
      {"NoKey", "Nee"},

      {"AttachFile.SelectFile","Selecteer het bestand of de bestanden die u wilt uploaden naar"},
      {"AttachFile.FilesBiggerThan","Bestanden groter dan"},
      {"AttachFile.AreIgnored","kB worden genegeerd!"},
      {"AttachFile.MultipleFiles","Als u meerdere bestanden wilt versturen vul dan hier het aantal in (max 20)"},
      
      {"ACLFile.submit", "Vernieuw deze rechten alstublieft."},
	  {"ACLFile.title", "Verander de ACL voor de file hier"},
	  
	  {"Commit.changedProperties", "De volgende bestanden hebben veranderde eigenschappen."},
	  
      {"HandleFile.MethodError", "Fout in methode"},
      {"HandleFile.MethodNotImplemented", "Deze methode is niet geimplementeerd in deze servlet"},

      {"HistoryFile.Of", "Bekijk the inhoud van dit bestand op revisie: "},
      {"HistoryFile.Submit", "Bekijk"},
      
      {"CopyFileMoveFile.PathNotExists", "Pad bestaat niet!"},
      {"CopyFileMoveFile.ExistingDirs", "Bestaande directories"},
      {"CopyFileMoveFile.NewDirs", "Nieuwe directories"},
      {"CopyFileMoveFile.FileName", "Bestandsnaam"},
      {"CopyFileMoveFile.ForceCreation", "Pad geforceerd aanmaken indien niet aanwezig"},
      
      {"MoveFile.MoveFile", "Verplaats dit bestand"},
      {"MoveFile.Submit", "Verplaats dit bestand"},
      {"MoveFile.ForceOverwrite", "Overwrite if file already exists"},

      {"CopyFile.CopyFile", "Kopieer dit bestand"},
      {"CopyFile.Submit", "Kopieer dit bestand"},      
     
      {"RevertFile.Submit", "Draai terug naar revisie!"},
      {"RevertFile.To", "Naar welke revisie wil je terugdraaien?"},

      {"PraiseFile.Submit", "Verantwoordelijken tussen deze twee revisies!"},

      {"PreviewFile.CommitAllChanges", "Al mijn wijzigingen opslaan"},
      {"PreviewFile.Submit", "Wijzigingen opslaan!"},

      {"EditFile.CreatedDirectory", "Map aangemaakt"},
      {"EditFile.PreviewChanges", "Voorbeeldweergave"},
      {"EditFile.NewTopic", "Dit is een nieuw onderwerp"},

      {"DiffFile.ChangeBetweenRevisions", "Veranderingen tussen revisies"},
      {"DiffFile.AndRevision", "en revisie"},
      {"DiffFile.OfFile", "van bestand"},
      {"DiffFile.Submit", "Wijzigingen tussen deze revisies!"},

      {"CommitFileSet.Submit", "Sla op!"},
      {"CommitFileSet.MergeSubmit", "Voer een bericht in bij deze opslag"},
      {"CommitFileSet.Resolve", "Los op!"},
      {"CommitFileSet.NoConflicts", "Geen conflicten gevonden."},
      {"CommitFileSet.CommitMessage", "Voer een bericht in bij deze opslag"},

      {"RevisionInfo.Information", "Revisie informatie"},
      {"RevisionInfo.NoLogs", "Geen log berichten gevonden"},
      {"RevisionInfo.AllLogs", "Bekijk alle log berichten zijn vanaf deze tot aan de laatste revisie: "},
      {"RevisionInfo.Submit", "Bekijk"},
      
      {"Error.DontDeleteRoot", "FOUT:De hoofdmap kan niet gewist worden."},
      {"Error.ActionNotAllowedOnRoot", "FOUT: Deze actie is niet toegestaan op de hoofdmap."},
      {"Error.RevisionNotFound", "FOUT: De opgevraagde revisie kon niet gevonden worden!"},
      {"Error.IntervalTooSmall", "FOUT: Het opgegeven interval is te klein!"},
      {"Error.NoNumber", "FOUT: Specificeer een getal!"},
      {"Error.CouldNotGetFileList", "FOUT: Kon geen bestandslijst verkrijgen!"},
      {"Error.NoCommitMessage", "FOUT: Voer een opslag bericht in!"},
      {"Error.FileNotFound", "FOUT: Het opgevraagde bestand kon niet gevonden worden!"},
      {"Error.CanNotEditDirectory", "FOUT: Kan geen map bewerken!"},
      {"Error.SameDirectory", "FOUT: De doelmap is dezelfde als de bronmap. Deze verplaatsing is onmogelijk!"},
      {"Error.CanNotEditBinary", "FOUT: Binaire bestanden kunnen niet veranderd worden!"},
      {"Error.CouldNotBeSaved", "FOUT: Kon niet opgeslagen worden!"},
      {"Error.NotDirectory","FOUT: Doel is geen map"},
      {"Error.CommitError","Commit fout"},
      {"Error.CommitOnNonExistingFile","Commit kan niet gedaan worden op niet bestaande bestanden !"},
      {"Error.StatusError","Status fout"},
      {"Error.StatusOfNonExistingFile","Er kan geen status worden opgevraagd over niet bestaande bestanden !"},
      {"Error.AttachError","Kon bestand niet bijsluiten"},
      {"Error.AttachOnFile","Bestanden kunnen niet aan bestanden gekoppeld worden, enkel aan directories !"},
      {"Error.AttachOnNonExisting","Bestanden kunnen niet aan niet-bestaande directories gekoppeld worden !"},
      
      {"Warning.MergeConflict", "Waarschuwing: Deze bestanden hebben samenvoegconflicten. Los deze op!"},
      {"Warning.DeleteFile", "Waarschuwing: Weet u zeker dat u dit bestand wilt verwijderen?"},
      {"Warning.DeleteDirectory", "Waarschuwing: Weet u het zeker dat u deze map inclusief alle bestanden en submappen wilt wissen?"},
      {"Warning.DirectoryNotFound", "Waarschuwing: Het bestand kon niet worden verplaatst, omdat de map niet bestaat. Als u wilt verplaatsen naar een niet bestaande map kies dan voor geforceerd aanmaken!"},
      {"Warning.FileExists", "Waarschuwing: Het bestand kon niet verplaats worden, omdat er al een bestand met die naam bestaat."},
	  
	  {"Text.Commit", "Opslaan"},
	  {"Text.History", "Geschiedenis"},
	  {"Text.Praise", "Praise"}, // ??????
	  {"Text.Revert", "Terugdraaien"},
      {"Text.Delete", "Verwijderen"},
      {"Text.Edit", "Bewerken"},
      {"Text.File", "Bestand"},
      {"Text.Move", "Verplaatsen"},
      {"Text.To", "Naar"},
      {"Text.Attach", "Vastmaken"},
      {"Text.Upload", "Verstuur"},
      {"Text.Author", "Auteur"},
      {"Text.Changed", "Gewijzigd"},
      {"Text.Revision", "Revisie"},
      {"Text.Content", "Inhoud"},
      {"Text.Preview", "Voorbeeldweergave"},
      {"Text.Line", "Regel"},
      {"Text.NoPaths","Geen paden"},
      {"Text.Status", "Status"},
      {"Text.Directory", "Map"},
      {"Text.LastRevision", "Laatste Revisie"},
      {"Text.ContentType", "Inhouds-soort"},
      {"Text.View", "Bekijken"},

      {"Status.Added", "Toegevoegd"},
      {"Status.Copied", "Gekopieerd"},
      {"Status.Deleted", "Verwijderd"},
      {"Status.Modified", "Gewijzigd"},
      {"Status.Normal", "Normaal"}
   };
}