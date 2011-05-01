-- run with 'psql -f <name-of-file> -U <user> -d template1 --password' and
-- make sure that the user you use has enough rights for creating new
-- users and creating new databases, new tables, etc.

-- use 'dospassword' as password the second time you are asked for a password

CREATE USER dosuser
	WITH ENCRYPTED PASSWORD 'dospassword'
	NOCREATEDB NOCREATEUSER;

CREATE DATABASE dosdb
	WITH OWNER = dosuser
	ENCODING = 'SQL_ASCII';

\connect dosdb dosuser

CREATE TABLE Users (
	loginName	varchar(20) PRIMARY KEY,
	password	varchar(20)
);

CREATE TABLE UserDetails (
	loginName	varchar(20) PRIMARY KEY NOT NULL REFERENCES Users(loginName) ON DELETE CASCADE ON UPDATE CASCADE,
	fullName	varchar(45),
	title		varchar(20),
	phone		varchar(20),
	granularity	int DEFAULT 15
);

CREATE TABLE Appointments (
	id		serial PRIMARY KEY,
	startTime	timestamp,
	endTime		timestamp,
	description	varchar(200),
	location	varchar(100),
	isGroupApp	boolean
);

CREATE TABLE Calendars (
	userName	varchar(20) NOT NULL REFERENCES Users(loginName) ON DELETE CASCADE ON UPDATE CASCADE,
	appointmentId	serial NOT NULL REFERENCES Appointments(id) ON DELETE CASCADE ON UPDATE CASCADE,
	PRIMARY KEY (userName, appointmentId)
);
