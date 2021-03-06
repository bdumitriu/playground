# 
CREATE TABLE `church_chu` (
  `chu_Church_ID` int(11) NOT NULL auto_increment,
  `chu_Church_Name` varchar(100) NOT NULL default '',
  `chu_Church_Address1` varchar(100) NOT NULL default '',
  `chu_Church_Address2` varchar(100) NOT NULL default '',
  `chu_Church_City` varchar(100) NOT NULL default '',
  `chu_Church_State` varchar(100) NOT NULL default '',
  `chu_Church_postalcode` varchar(100) NOT NULL default '',
  `chu_Church_Country` varchar(100) NOT NULL default '',
  `chu_Church_URL` varchar(255) NOT NULL default '',
  `chu_Church_DateEntered` date NOT NULL default '0000-00-00',
  `chu_Church_email` varchar(100) NOT NULL default '',
  `chu_Church_phone` varchar(100) NOT NULL default '',
  PRIMARY KEY  (`chu_Church_ID`)
) TYPE=MyISAM; 

insert into church_chu(chu_Church_ID, chu_Church_Name) values(1,'My Church');

# Host: localhost
# Database: osc
# Table: 'donationamounts_dna'
# 
CREATE TABLE `donationamounts_dna` (
  `dna_don_ID` mediumint(9) unsigned NOT NULL default '0',
  `chu_Church_ID` int(11) NOT NULL default '0',
  `dna_Amount` decimal(10,2) default NULL,
  `dna_fun_ID` tinyint(3) unsigned default NULL,
  KEY `dna_don_ID` (`dna_don_ID`)
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'donationfund_fun'
# 
CREATE TABLE `donationfund_fun` (
  `fun_ID` tinyint(3) NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `fun_Active` enum('true','false') NOT NULL default 'true',
  `fun_Name` varchar(30) default NULL,
  `fun_Description` varchar(100) default NULL,
  PRIMARY KEY  (`fun_ID`),
  UNIQUE KEY `fun_ID` (`fun_ID`)
) TYPE=MyISAM; 

# Sample data for table `donationfund_fun`
INSERT INTO donationfund_fun VALUES (1,1, 'true', 'General Donation', 'Default fund: General operating expenses.');
INSERT INTO donationfund_fun VALUES (2,1, 'true', 'Missions', 'Support for missions.');
INSERT INTO donationfund_fun VALUES (3,1, 'true', 'Building', 'New building fund.');



# Host: localhost
# Database: osc
# Table: 'donations_don'
# 
CREATE TABLE `donations_don` (
  `don_ID` mediumint(9) unsigned NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `don_DonorID` mediumint(9) unsigned default NULL,
  `don_PaymentType` tinyint(3) default NULL,
  `don_CheckNumber` mediumint(9) unsigned NOT NULL default '0',
  `don_Date` date NOT NULL default '0000-00-00',
  `don_Envelope` smallint(5) unsigned default NULL,
  PRIMARY KEY  (`don_ID`),
  KEY `don_DonorID` (`don_DonorID`),
  KEY `don_Date` (`don_Date`)
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'family_fam'
# 
CREATE TABLE `family_fam` (
  `fam_ID` mediumint(9) unsigned NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `fam_Name` varchar(50) default NULL,
  `fam_Address1` varchar(255) default NULL,
  `fam_Address2` varchar(255) default NULL,
  `fam_City` varchar(50) default NULL,
  `fam_State` varchar(50) default NULL,
  `fam_Zip` varchar(50) default NULL,
  `fam_Country` varchar(50) default NULL,
  `fam_HomePhone` varchar(30) default NULL,
  `fam_WorkPhone` varchar(30) default NULL,
  `fam_CellPhone` varchar(30) default NULL,
  `fam_Email` varchar(100) default NULL,
  `fam_WeddingDate` date default NULL,
  `fam_DateEntered` datetime NOT NULL default '0000-00-00 00:00:00',
  `fam_DateLastEdited` datetime default NULL,
  `fam_EnteredBy` smallint(5) unsigned NOT NULL default '0',
  `fam_EditedBy` smallint(5) unsigned default '0',
  PRIMARY KEY  (`fam_ID`),
  KEY `fam_ID` (`fam_ID`)
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'group_grp'
# 
CREATE TABLE `group_grp` (
  `grp_ID` mediumint(8) unsigned NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `grp_Type` tinyint(4) NOT NULL default '0',
  `grp_RoleListID` mediumint(8) unsigned NOT NULL default '0',
  `grp_DefaultRole` mediumint(9) NOT NULL default '0',
  `grp_Name` varchar(50) NOT NULL default '',
  `grp_Description` text,
  `grp_hasSpecialProps` enum('true','false') NOT NULL default 'false',
  PRIMARY KEY  (`grp_ID`),
  UNIQUE KEY `grp_ID` (`grp_ID`),
  KEY `grp_ID_2` (`grp_ID`)
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'groupprop_1'
# 
CREATE TABLE `groupprop_1` (
  `per_ID` mediumint(8) unsigned NOT NULL default '0',
  `c1` enum('false','true') default NULL,
  PRIMARY KEY  (`per_ID`),
  UNIQUE KEY `per_ID` (`per_ID`)
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'groupprop_master'
# 
CREATE TABLE `groupprop_master` (
  `grp_ID` mediumint(9) unsigned NOT NULL default '0',
  `prop_ID` tinyint(3) unsigned NOT NULL default '0',
  `chu_Church_ID` int(11) NOT NULL default '0',
  `prop_Field` varchar(5) NOT NULL default '0',
  `prop_Name` varchar(40) default NULL,
  `prop_Description` varchar(60) default NULL,
  `type_ID` smallint(5) unsigned NOT NULL default '0',
  `prop_Special` mediumint(9) unsigned default NULL,
  `prop_PersonDisplay` enum('false','true') NOT NULL default 'false'
) TYPE=MyISAM COMMENT='Group-specific properties order, name, description, type'; 

# Host: localhost
# Database: osc
# Table: 'list_lst'
# 
CREATE TABLE `list_lst` (
  `lst_ID` mediumint(8) unsigned NOT NULL default '0',
  `chu_Church_ID` int(11) NOT NULL default '0',
  `lst_OptionID` mediumint(8) unsigned NOT NULL default '0',
  `lst_OptionSequence` tinyint(3) unsigned NOT NULL default '0',
  `lst_OptionName` varchar(50) NOT NULL default ''
) TYPE=MyISAM; 

# Sample data for member classifications
INSERT INTO list_lst VALUES (1,1, 1, 1, 'Member');
INSERT INTO list_lst VALUES (1,1, 2, 2, 'Regular Attender');
INSERT INTO list_lst VALUES (1,1, 3, 3, 'Guest');
INSERT INTO list_lst VALUES (1,1, 5, 4, 'Non-Attender');
INSERT INTO list_lst VALUES (1,1, 4, 5, 'Non-Attender (staff)');

# Sample data for family roles
INSERT INTO list_lst VALUES (2,1, 1, 1, 'Head of Household');
INSERT INTO list_lst VALUES (2,1, 2, 2, 'Spouse');
INSERT INTO list_lst VALUES (2,1, 3, 3, 'Child');
INSERT INTO list_lst VALUES (2,1, 4, 4, 'Other Relative');
INSERT INTO list_lst VALUES (2,1, 5, 5, 'Non Relative');

# Sample data for group types
INSERT INTO list_lst VALUES (3,1, 1, 1, 'Ministry');
INSERT INTO list_lst VALUES (3,1, 2, 2, 'Team');
INSERT INTO list_lst VALUES (3,1, 3, 3, 'Bible Study');
INSERT INTO list_lst VALUES (3,1, 4, 4, 'Sunday School Class');

# Insert the custom-field / group-property types
INSERT INTO list_lst VALUES (4,1, 1, 1, 'True / False');
INSERT INTO list_lst VALUES (4,1, 2, 2, 'Date');
INSERT INTO list_lst VALUES (4,1, 3, 3, 'Text Field (50 char)');
INSERT INTO list_lst VALUES (4,1, 4, 4, 'Text Field (100 char)');
INSERT INTO list_lst VALUES (4,1, 5, 5, 'Text Field (Long)');
INSERT INTO list_lst VALUES (4,1, 6, 6, 'Year');
INSERT INTO list_lst VALUES (4,1, 7, 7, 'Season');
INSERT INTO list_lst VALUES (4,1, 8, 8, 'Number');
INSERT INTO list_lst VALUES (4,1, 9, 9, 'Person from Group');
INSERT INTO list_lst VALUES (4,1, 10, 10, 'Money');
INSERT INTO list_lst VALUES (4,1, 11, 11, 'Phone Number');
INSERT INTO list_lst VALUES (4,1, 12, 12, 'Custom Drop-Down List');


# Host: localhost
# Database: osc
# Table: 'note_nte'
# 
CREATE TABLE `note_nte` (
  `nte_ID` mediumint(8) unsigned NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `nte_per_ID` mediumint(8) unsigned NOT NULL default '0',
  `nte_fam_ID` mediumint(8) unsigned NOT NULL default '0',
  `nte_Private` mediumint(8) unsigned NOT NULL default '0',
  `nte_Text` text,
  `nte_DateEntered` datetime NOT NULL default '0000-00-00 00:00:00',
  `nte_DateLastEdited` datetime default NULL,
  `nte_EnteredBy` mediumint(8) unsigned NOT NULL default '0',
  `nte_EditedBy` mediumint(8) unsigned NOT NULL default '0',
  PRIMARY KEY  (`nte_ID`)
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'old_donationamounts_dna'
# 
CREATE TABLE `old_donationamounts_dna` (
  `dna_don_ID` mediumint(9) unsigned NOT NULL default '0',
  `dna_Amount` decimal(10,2) default NULL,
  `dna_fun_ID` tinyint(3) unsigned default NULL,
  KEY `dna_don_ID` (`dna_don_ID`)
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'person2group2role_p2g2r'
# 
CREATE TABLE `person2group2role_p2g2r` (
  `p2g2r_per_ID` mediumint(8) unsigned NOT NULL default '0',
  `p2g2r_grp_ID` mediumint(8) unsigned NOT NULL default '0',
  `chu_Church_ID` int(11) NOT NULL default '0',
  `p2g2r_rle_ID` mediumint(8) unsigned NOT NULL default '0',
  PRIMARY KEY  (`p2g2r_per_ID`,`p2g2r_grp_ID`),
  KEY `p2g2r_per_ID` (`p2g2r_per_ID`,`p2g2r_grp_ID`,`p2g2r_rle_ID`)
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'person_custom'
# 
CREATE TABLE `person_custom` (
  `per_ID` mediumint(9) NOT NULL default '0',
  `chu_Church_ID` int(11) NOT NULL default '0',
  `c1` date default NULL,
  `c2` enum('false','true') default NULL,
  `c3` enum('winter','spring','summer','fall') default NULL,
  PRIMARY KEY  (`per_ID`)
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'person_custom_master'
# 
CREATE TABLE `person_custom_master` (
  `custom_Order` smallint(6) NOT NULL default '0',
  `chu_Church_ID` int(11) NOT NULL default '0',
  `custom_Field` varchar(5) NOT NULL default '',
  `custom_Name` varchar(40) NOT NULL default '',
  `custom_Special` mediumint(8) unsigned default NULL,
  `custom_Side` enum('left','right') NOT NULL default 'left',
  `type_ID` tinyint(4) NOT NULL default '0'
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'person_per'
# 
CREATE TABLE `person_per` (
  `per_ID` mediumint(9) unsigned NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `per_Title` varchar(50) default NULL,
  `per_FirstName` varchar(50) default NULL,
  `per_MiddleName` varchar(50) default NULL,
  `per_LastName` varchar(50) default NULL,
  `per_Suffix` varchar(50) default NULL,
  `per_Address1` varchar(50) default NULL,
  `per_Address2` varchar(50) default NULL,
  `per_City` varchar(50) default NULL,
  `per_State` varchar(50) default NULL,
  `per_Zip` varchar(50) default NULL,
  `per_Country` varchar(50) default NULL,
  `per_HomePhone` varchar(30) default NULL,
  `per_WorkPhone` varchar(30) default NULL,
  `per_CellPhone` varchar(30) default NULL,
  `per_Email` varchar(50) default NULL,
  `per_WorkEmail` varchar(50) default NULL,
  `per_BirthMonth` tinyint(3) unsigned NOT NULL default '0',
  `per_BirthDay` tinyint(3) unsigned NOT NULL default '0',
  `per_BirthYear` year(4) default NULL,
  `per_MembershipDate` date default NULL,
  `per_Gender` tinyint(1) unsigned NOT NULL default '0',
  `per_fmr_ID` tinyint(3) unsigned NOT NULL default '0',
  `per_cls_ID` tinyint(3) unsigned NOT NULL default '0',
  `per_fam_ID` smallint(5) unsigned NOT NULL default '0',
  `per_Envelope` smallint(5) unsigned default NULL,
  `per_DateLastEdited` datetime default NULL,
  `per_DateEntered` datetime NOT NULL default '0000-00-00 00:00:00',
  `per_EnteredBy` smallint(5) unsigned NOT NULL default '0',
  `per_EditedBy` smallint(5) unsigned default '0',
  PRIMARY KEY  (`per_ID`),
  KEY `per_ID` (`per_ID`)
) TYPE=MyISAM; 


INSERT INTO person_per VALUES (1,1,NULL,'OSC',NULL,'Admin',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,0,0000,NULL,0,0,0,0,NULL,NULL,'2001-02-05 18:00:00',0,0);


# Host: localhost
# Database: osc
# Table: 'property_pro'
# 
CREATE TABLE `property_pro` (
  `pro_ID` mediumint(8) unsigned NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `pro_Class` varchar(10) NOT NULL default '',
  `pro_prt_ID` mediumint(8) unsigned NOT NULL default '0',
  `pro_Name` varchar(200) NOT NULL default '0',
  `pro_Description` text NOT NULL,
  `pro_Prompt` varchar(255) default NULL,
  PRIMARY KEY  (`pro_ID`),
  UNIQUE KEY `pro_ID` (`pro_ID`),
  KEY `pro_ID_2` (`pro_ID`)
) TYPE=MyISAM; 

INSERT INTO property_pro VALUES (1,1,'p',1,'Disabled','has a disability.','What is the nature of the disability?');
INSERT INTO property_pro VALUES (2,1,'f',2,'Single Parent','is a single-parent household.','');
INSERT INTO property_pro VALUES (3,1,'g',3,'Youth','is youth-oriented.','');

# Host: localhost
# Database: osc
# Table: 'propertytype_prt'
# 
CREATE TABLE `propertytype_prt` (
  `prt_ID` mediumint(9) NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `prt_Class` varchar(10) NOT NULL default '',
  `prt_Name` varchar(50) NOT NULL default '',
  `prt_Description` text NOT NULL,
  PRIMARY KEY  (`prt_ID`),
  UNIQUE KEY `prt_ID` (`prt_ID`),
  KEY `prt_ID_2` (`prt_ID`)
) TYPE=MyISAM; 

INSERT INTO propertytype_prt VALUES (1,1,'p','General','General Person Properties');
INSERT INTO propertytype_prt VALUES (2,1,'f','General','General Family Properties');
INSERT INTO propertytype_prt VALUES (3,1,'g','General','General Group Properties');


# Host: localhost
# Database: osc
# Table: 'query_qry'
# 
CREATE TABLE `query_qry` (
  `qry_ID` mediumint(8) unsigned NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `qry_SQL` text NOT NULL,
  `qry_Name` varchar(255) NOT NULL default '',
  `qry_Description` text NOT NULL,
  `qry_Count` tinyint(1) unsigned NOT NULL default '0',
  PRIMARY KEY  (`qry_ID`),
  UNIQUE KEY `qry_ID` (`qry_ID`),
  KEY `qry_ID_2` (`qry_ID`)
) TYPE=MyISAM; 

INSERT INTO query_qry VALUES (2,1,'SELECT COUNT(per_ID)\nAS \'Count\'\nFROM person_per','Person Count','Returns the total number of people in the database.',0);
INSERT INTO query_qry VALUES (3,1,'SELECT CONCAT(\'<a href=FamilyView.php?FamilyID=\',fam_ID,\'>\',fam_Name,\'</a>\') AS \'Family Name\', COUNT(*) AS \'No.\'\nFROM person_per\nINNER JOIN family_fam\nON fam_ID = per_fam_ID\nGROUP BY per_fam_ID\nORDER BY \'No.\' DESC','Family Member Count','Returns each family and the total number of people assigned to them.',0);
INSERT INTO query_qry VALUES (4,1,'SELECT per_ID as AddToCart,CONCAT(\'<a href=PersonView.php?PersonID=\',per_ID,\'>\',per_FirstName,\' \',per_LastName,\'</a>\') AS Name, CONCAT(per_BirthMonth,\'/\',per_BirthDay,\'/\',per_BirthYear) AS \'Birth Date\', \nYEAR(CURRENT_DATE) - per_BirthYear AS \'Age\'\nFROM person_per\nWHERE\nDATE_ADD(CONCAT(per_BirthYear,\'-\',per_BirthMonth,\'-\',per_BirthDay),INTERVAL ~min~ YEAR) <= CURDATE()\nAND\nDATE_ADD(CONCAT(per_BirthYear,\'-\',per_BirthMonth,\'-\',per_BirthDay),INTERVAL ~max~ YEAR) >= CURDATE()','Person by Age','Returns any person records with ages between two given ages.',1);
INSERT INTO query_qry VALUES (6,1,'SELECT COUNT(per_ID) AS Total FROM person_per WHERE per_Gender = ~gender~','Total By Gender','Total of records matching a given gender.',0);
INSERT INTO query_qry VALUES (7,1,'SELECT per_ID as AddToCart, CONCAT(per_FirstName,\' \',per_LastName) AS Name FROM person_per WHERE per_fmr_ID = ~role~ AND per_Gender = ~gender~','Person by Role and Gender','Selects person records with the family role and gender specified.',1);
INSERT INTO query_qry VALUES (9,1,'SELECT \r\nper_ID as AddToCart, \r\nCONCAT(per_FirstName,\' \',per_LastName) AS Name, \r\nCONCAT(r2p_Value,\' \') AS Value\r\nFROM person_per,record2property_r2p\r\nWHERE per_ID = r2p_record_ID\r\nAND r2p_pro_ID = ~PropertyID~\r\nORDER BY per_LastName','Person by Property','Returns person records which are assigned the given property.',1);
INSERT INTO query_qry VALUES (10,1, 'SELECT CONCAT(\'<a href=PersonView.php?PersonID=\',per_ID,\' target=view>\', per_FirstName,\' \', per_MiddleName,\' \', per_LastName,\'</a>\') AS Name, CONCAT(\'<a href=DonationView.php?PersonID=\',per_ID,\' target=view>\', \'$\',sum(round(dna_amount,2)),\'</a>\') as Amount\r\nFROM donations_don, person_per\r\nLEFT JOIN donationamounts_dna ON don_ID = dna_don_ID\r\nWHERE don_DonorID = per_ID AND don_date >= \'~startdate~\'\r\nAND don_date <= \'~enddate~\'\r\nGROUP BY don_DonorID\r\nORDER BY per_LastName ASC', 'Total Donations by Member', 'Sum of donations by member for a specific period of time between two dates.', 1);
INSERT INTO query_qry VALUES (11,1,'SELECT fun_name as Fund, CONCAT(\'$\',sum(round(dna_amount,2))) as Total\r\nFROM donations_don\r\nLEFT JOIN donationamounts_dna ON donations_don.don_ID = donationamounts_dna.dna_don_ID LEFT JOIN donationfund_fun ON donationamounts_dna.dna_fun_ID = donationfund_fun.fun_ID\r\nWHERE don_date >= \'~startdate~\'\r\nAND don_date <= \'~enddate~\'\r\nGROUP BY fun_id\r\nORDER BY fun_name', 'Total Donations by Fund', 'Sum of donations by FUND for a specific period of time between two dates.', 1);
INSERT INTO query_qry VALUES (15,1, 'SELECT per_ID as AddToCart, CONCAT(\'<a href=PersonView.php?PersonID=\',per_ID,\'>\',per_FirstName,\' \',per_MiddleName,\' \',per_LastName,\'</a>\') AS Name, \r\nper_City as City, per_State as State,\r\nper_Zip as ZIP, per_HomePhone as HomePhone\r\nFROM person_per \r\nWHERE ~searchwhat~ LIKE \'%~searchstring~%\'', 'Advanced Search', 'Search by any part of Name, City, State, Zip, or Home Phone.', 1);


# Host: localhost
# Database: osc
# Table: 'queryparameteroptions_qpo'
# 
CREATE TABLE `queryparameteroptions_qpo` (
  `qpo_ID` smallint(5) unsigned NOT NULL auto_increment,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `qpo_qrp_ID` mediumint(8) unsigned NOT NULL default '0',
  `qpo_Display` varchar(50) NOT NULL default '',
  `qpo_Value` varchar(50) NOT NULL default '',
  PRIMARY KEY  (`qpo_ID`),
  UNIQUE KEY `qpo_ID` (`qpo_ID`)
) TYPE=MyISAM; 

INSERT INTO queryparameteroptions_qpo VALUES (1,1, 4,'Male','1');
INSERT INTO queryparameteroptions_qpo VALUES (2,1, 4,'Female','2');
INSERT INTO queryparameteroptions_qpo VALUES (3,1, 6,'Male','1');
INSERT INTO queryparameteroptions_qpo VALUES (4,1, 6,'Female','2');
INSERT INTO queryparameteroptions_qpo VALUES (5,1,  15, 'Name', 'CONCAT(per_FirstName,per_MiddleName,per_LastName)');
INSERT INTO queryparameteroptions_qpo VALUES (6,1,  15, 'Zip Code', 'per_Zip');
INSERT INTO queryparameteroptions_qpo VALUES (7,1, 15, 'State', 'per_State');
INSERT INTO queryparameteroptions_qpo VALUES (8, 1,15, 'City', 'per_City');
INSERT INTO queryparameteroptions_qpo VALUES (9,1, 15, 'Home Phone', 'per_HomePhone');

# Host: localhost
# Database: osc
# Table: 'queryparameters_qrp'
# 
CREATE TABLE `queryparameters_qrp` (
  `qrp_ID` mediumint(8) unsigned NOT NULL auto_increment,
  `qrp_qry_ID` mediumint(8) unsigned NOT NULL default '0',
  `chu_Church_ID` int(11) NOT NULL default '0',
  `qrp_Type` tinyint(3) unsigned NOT NULL default '0',
  `qrp_OptionSQL` text,
  `qrp_Name` varchar(25) default NULL,
  `qrp_Description` text,
  `qrp_Alias` varchar(25) default NULL,
  `qrp_Default` varchar(25) default NULL,
  `qrp_Required` tinyint(3) unsigned NOT NULL default '0',
  `qrp_InputBoxSize` tinyint(3) unsigned NOT NULL default '0',
  `qrp_Validation` varchar(5) NOT NULL default '',
  `qrp_NumericMax` int(11) default NULL,
  `qrp_NumericMin` int(11) default NULL,
  `qrp_AlphaMinLength` int(11) default NULL,
  `qrp_AlphaMaxLength` int(11) default NULL,
  PRIMARY KEY  (`qrp_ID`),
  UNIQUE KEY `qrp_ID` (`qrp_ID`),
  KEY `qrp_qry_ID` (`qrp_qry_ID`),
  KEY `qrp_ID_2` (`qrp_ID`)
) TYPE=MyISAM; 

INSERT INTO queryparameters_qrp VALUES (1,4,1,0,NULL,'Minimum Age','The minimum age for which you want records returned.','min','0',0,5,'n',120,0,NULL,NULL);
INSERT INTO queryparameters_qrp VALUES (2,4,1,0,NULL,'Maximum Age','The maximum age for which you want records returned.','max','120',1,5,'n',120,0,NULL,NULL);
INSERT INTO queryparameters_qrp VALUES (4,6,1,1,'','Gender','The desired gender to search the database for.','gender','1',1,0,'',0,0,0,0);
INSERT INTO queryparameters_qrp VALUES (5,7,1,2,'SELECT lst_OptionID as Value, lst_OptionName as Display FROM list_lst WHERE lst_ID=2 ORDER BY lst_OptionSequence','Family Role','Select the desired family role.','role','1',0,0,'',0,0,0,0);
INSERT INTO queryparameters_qrp VALUES (6,7,1,1,'','Gender','The gender for which you would like records returned.','gender','1',1,0,'',0,0,0,0);
INSERT INTO queryparameters_qrp VALUES (8,9,1,2,'SELECT pro_ID AS Value, pro_Name as Display \r\nFROM property_pro\r\nWHERE pro_Class= \'p\' \r\nORDER BY pro_Name ','Property','The property for which you would like person records returned.','PropertyID','0',1,0,'',0,0,0,0);
INSERT INTO queryparameters_qrp VALUES (9, 10,1, 2, 'SELECT distinct don_date as Value, don_date as Display FROM donations_don ORDER BY don_date ASC', 'Beginning Date', 'Please select the beginning date to calculate total contributions for each member (i.e. YYYY-MM-DD). NOTE: You can only choose dates that conatain donations.', 'startdate', '1', 1, 0, '0', 0, 0, 0, 0);
INSERT INTO queryparameters_qrp VALUES (10, 10,1, 2, 'SELECT distinct don_date as Value, don_date as Display FROM donations_don\r\nORDER BY don_date DESC', 'Ending Date', 'Please enter the last date to calculate total contributions for each member (i.e. YYYY-MM-DD).', 'enddate', '1', 1, 0, '', 0, 0, 0, 0);
INSERT INTO queryparameters_qrp VALUES (14, 15,1, 0, '', 'Search', 'Enter any part of the following: Name, City, State, Zip, or Home Phone.', 'searchstring', '', 1, 0, '', 0, 0, 0, 0);
INSERT INTO queryparameters_qrp VALUES (15, 15,1, 1, '', 'Field', 'Select field to search for.', 'searchwhat', '1', 1, 0, '', 0, 0, 0, 0);
INSERT INTO queryparameters_qrp VALUES (16, 11,1, 2, 'SELECT distinct don_date as Value, don_date as Display FROM donations_don ORDER BY don_date ASC', 'Beginning Date', 'Please select the beginning date to calculate total contributions for each member (i.e. YYYY-MM-DD). NOTE: You can only choose dates that conatain donations.', 'startdate', '1', 1, 0, '0', 0, 0, 0, 0);
INSERT INTO queryparameters_qrp VALUES (17, 11,1, 2, 'SELECT distinct don_date as Value, don_date as Display FROM donations_don\r\nORDER BY don_date DESC', 'Ending Date', 'Please enter the last date to calculate total contributions for each member (i.e. YYYY-MM-DD).', 'enddate', '1', 1, 0, '', 0, 0, 0, 0);


# Host: localhost
# Database: osc
# Table: 'record2property_r2p'
# 
CREATE TABLE `record2property_r2p` (
  `r2p_pro_ID` mediumint(8) unsigned NOT NULL default '0',
  `r2p_record_ID` mediumint(8) unsigned NOT NULL default '0',
  `chu_Church_ID` int(11) NOT NULL default '0',
  `r2p_Value` text NOT NULL
) TYPE=MyISAM; 

# Host: localhost
# Database: osc
# Table: 'user_usr'
# 
CREATE TABLE `user_usr` (
  `usr_per_ID` mediumint(9) unsigned NOT NULL default '0',
  `chu_Church_ID` int(11) default '0',
  `usr_Password` varchar(50) NOT NULL default '',
  `usr_NeedPasswordChange` tinyint(3) unsigned NOT NULL default '0',
  `usr_LastLogin` datetime NOT NULL default '0000-00-00 00:00:00',
  `usr_LoginCount` smallint(5) unsigned NOT NULL default '0',
  `usr_FailedLogins` tinyint(3) unsigned NOT NULL default '0',
  `usr_AddRecords` tinyint(3) unsigned NOT NULL default '0',
  `usr_EditRecords` tinyint(3) unsigned NOT NULL default '0',
  `usr_DeleteRecords` tinyint(3) unsigned NOT NULL default '0',
  `usr_MenuOptions` tinyint(3) unsigned NOT NULL default '0',
  `usr_ManageGroups` tinyint(3) unsigned NOT NULL default '0',
  `usr_Finance` tinyint(3) unsigned NOT NULL default '0',
  `usr_Communication` tinyint(3) unsigned NOT NULL default '0',
  `usr_Notes` tinyint(3) unsigned NOT NULL default '0',
  `usr_Admin` tinyint(3) unsigned NOT NULL default '0',
  `usr_Workspacewidth` smallint(6) default NULL,
  `usr_BaseFontSize` tinyint(4) default NULL,
  `usr_SearchLimit` tinyint(4) default '10',
  `usr_Login` varchar(100) NOT NULL default '',
  `usr_Style` varchar(50) default 'Style.css',
  PRIMARY KEY  (`usr_per_ID`),
  KEY `usr_per_ID` (`usr_per_ID`)
) TYPE=MyISAM; 

INSERT INTO user_usr VALUES (1,1,1,'3af0e3bbc3d4c00ac8294887eef91280',1,'0000-00-00 00:00:00',0,0,1,1,1,1,1,1,1,1,1,580,9,10,'Style.css');

# Host: localhost
# Database: osc
# Table: 'tithe'
#
CREATE TABLE `tithe_tth` (
  `tth_Tithe_ID` mediumint(9) unsigned NOT NULL auto_increment,
  `per_person_ID` mediumint(9) unsigned default NULL,
  `chu_Church_ID` int(11) NOT NULL default '0',
  `tth_Amount` decimal(10,2) default NULL,
  `tth_Year` smallint unsigned default NULL,
  `tth_Created` date NOT NULL default '0000-00-00',
  `tth_LastEdited` date NOT NULL default '0000-00-00',
  `tth_LastEditedBy` mediumint(9) unsigned default NULL,
  PRIMARY KEY (`tth_Tithe_ID`)
) TYPE=MyISAM;
