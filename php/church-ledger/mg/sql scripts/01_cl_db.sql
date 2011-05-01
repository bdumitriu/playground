-- run with 'psql -f 01_cl_db.sql -U <user> -d testdb --password' and
-- make sure that the user you use has enough rights for creating new
-- users and creating new databases, new tables, etc.

-- use 'chledger' as password the second time you are asked for a password

CREATE USER chledger
	WITH ENCRYPTED PASSWORD 'chledger'
	NOCREATEDB NOCREATEUSER;

CREATE DATABASE chledger
	WITH OWNER = chledger
	ENCODING = 'SQL_ASCII';

\connect chledger chledger

CREATE TABLE Member (
	memberId	serial PRIMARY KEY,
	familyId	int DEFAULT 1,
	lastName	varchar(40),
	firstName	varchar(40),
	dateOfBirth	varchar(40),
	sex		varchar(40),
	middleI		varchar(40),
	email		varchar(40),
	churchId	int DEFAULT 1,
	memberStatus	int DEFAULT 0
);

CREATE TABLE MembershipGroup (
	id		serial PRIMARY KEY,
	name		varchar(50),
	description	varchar(100)
);

CREATE TABLE MGMembers (
	idMG		int NOT NULL REFERENCES MembershipGroup(id) ON DELETE CASCADE,
	idMember	int NOT NULL REFERENCES Member(memberId) ON DELETE CASCADE,
	PRIMARY KEY (idMG, idMember)
);
