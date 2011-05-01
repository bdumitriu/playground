<?

// this is the number of fields which is expected to be contained
// by every row (including the header row) in the csv file. If any
// row in the file contains either less or more than this number of
// fileds, an error will be signaled and the import procedure will
// be aborted
$nrFields = 76;

// this is the maximum number of characters which can appear on any
// single line of the .csv file
$maxRowLength = 1024;

// the maximum number of rows you want to preview in the preview
// screen
$preview = 10;

// ip address or hostname of the mysql server
$mysqlServer = "localhost";

// user name to use for connecting to the mysql server
$mysqlUser = "bdumitriu";

// password to use for connecting to the mysql server
$mysqlPassword = "bogdan";

// database name to use
$mysqlDBName = "traffic";

// table name to use
$mysqlTableName = "traffic";

// premier accounts
$premier = array(62489020, 62489021, 6248922, 1588);

// budget accounts
$budget = array(62489023, 62489024, 1599);

// start of peak time
$peakStart = "7:00AM";

// end of peak time
$peakEnd = "7:00PM";

// start of off-peak time
$offPeakStart = "7:00PM";

// end of off-peak time
$offPeakEnd="7:00AM";

?>
