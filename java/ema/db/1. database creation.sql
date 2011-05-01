CREATE USER ema
	WITH ENCRYPTED PASSWORD 'ema'
	NOCREATEDB NOCREATEUSER;

CREATE DATABASE ema
	WITH OWNER = ema
	ENCODING = 'SQL_ASCII';

-- you need to reconnect, selecting ema as default database
-- at this point before running the rest of the commands

CREATE DOMAIN numePJ AS varchar(50);	-- nume persoana fizica
CREATE DOMAIN numePF AS varchar(50);	-- nume persoana juridica
CREATE DOMAIN adresa AS varchar(150);	-- adresa postala
CREATE DOMAIN telefon AS varchar(20);	-- numar de telefon

CREATE TABLE DN (
	id		serial PRIMARY KEY,
	numar		smallint NOT NULL UNIQUE
);

INSERT INTO DN VALUES (DEFAULT, 15);
INSERT INTO DN VALUES (DEFAULT, 20);
INSERT INTO DN VALUES (DEFAULT, 25);
INSERT INTO DN VALUES (DEFAULT, 30);
INSERT INTO DN VALUES (DEFAULT, 50);
INSERT INTO DN VALUES (DEFAULT, 80);

CREATE TABLE Domeniu (
	id		serial PRIMARY KEY,
	nume		varchar(20) NOT NULL UNIQUE
);

INSERT INTO Domeniu VALUES (DEFAULT, 'public');
INSERT INTO Domeniu VALUES (DEFAULT, 'privat');

CREATE TABLE FabricantApometre (
	id		serial PRIMARY KEY,
	nume		numePJ NOT NULL UNIQUE
);

CREATE TABLE Apometru (
	id		serial PRIMARY KEY,
	id_fabricant	int NOT NULL REFERENCES FabricantApometre(id),
	serie		varchar(10) NOT NULL CHECK (char_length(serie) >= 8),
	id_dn		int NOT NULL REFERENCES DN(id),
	clasa		char(1) NOT NULL,
	id_domeniu	int NOT NULL REFERENCES Domeniu(id),
	observatii	text,
	UNIQUE (id_fabricant, serie)
);

CREATE TABLE PF (
	id		serial PRIMARY KEY,
	nume		numePF NOT NULL,
	adresa		adresa,
	telefon		telefon
);

CREATE TABLE Banca (
	id		serial PRIMARY KEY,
	nume		numePJ NOT NULL,
	adresa		adresa,
	telefon		telefon,
	fax		telefon
);

CREATE TABLE PJ (
	id		serial PRIMARY KEY,
	nume		numePJ NOT NULL UNIQUE,
	adresa_sediu	adresa,
	adresa_lucru	adresa,
	cui		char(8) UNIQUE,
	orc1		char(3),
	orc2		char(5),
	orc3		char(4),
	cont		varchar(25),
	id_banca	int REFERENCES Banca(id),
	nume_contact	numePF,
	tel_contact	telefon,
	telefon		telefon,
	fax		telefon,
	UNIQUE (cont, id_banca),
	UNIQUE (orc1, orc2, orc3)
);

CREATE TABLE TipCerere (
	id		serial PRIMARY KEY,
	nume		varchar(20) NOT NULL UNIQUE,
	valabilitate	smallint NOT NULL
);

INSERT INTO TipCerere VALUES (DEFAULT, 'initiala', 3);
INSERT INTO TipCerere VALUES (DEFAULT, 'verificare', 2);
INSERT INTO TipCerere VALUES (DEFAULT, 'reverificare', 2);

CREATE TABLE Calitate (
	id		serial PRIMARY KEY,
	nume		varchar(20) NOT NULL UNIQUE
);

INSERT INTO Calitate VALUES (DEFAULT, 'detinator');
INSERT INTO Calitate VALUES (DEFAULT, 'utilizator');
INSERT INTO Calitate VALUES (DEFAULT, 'montator');
INSERT INTO Calitate VALUES (DEFAULT, 'reparator');

CREATE TABLE Solicitant (
	id		serial PRIMARY KEY,
	nume		varchar(20) NOT NULL UNIQUE
);

INSERT INTO Solicitant VALUES (DEFAULT, 'persoana fizica');
INSERT INTO Solicitant VALUES (DEFAULT, 'persoana juridica');

CREATE TABLE Cerere (
	id		serial PRIMARY KEY,
	cod		int NOT NULL CHECK (cod < 100000),
	an		smallint NOT NULL CHECK (an > 1999),
	id_tip		int NOT NULL REFERENCES TipCerere(id),
	id_calitate	int REFERENCES Calitate(id),
	id_solicitant	int NOT NULL REFERENCES Solicitant(id),
	data_cerere	date,
	data_scadenta	date,
	UNIQUE (cod, an)
);

CREATE TABLE CerereApometru (
	id_cerere	int NOT NULL REFERENCES Cerere(id),
	id_apometru	int NOT NULL REFERENCES Apometru(id),
	PRIMARY KEY (id_cerere, id_apometru)
);

CREATE TABLE CererePF (
	id		int NOT NULL REFERENCES Cerere(id) PRIMARY KEY,
	id_pf		int NOT NULL REFERENCES PF(id)
);

CREATE TABLE Contract (
	id		serial PRIMARY KEY,
	cod		int NOT NULL CHECK (cod < 100000),
	an		smallint NOT NULL CHECK (an > 1999),
	id_firma	int NOT NULL REFERENCES PJ(id),
	data_incheiere	date,
	UNIQUE (cod, an)
);

CREATE TABLE CererePJ (
	id		int NOT NULL REFERENCES Cerere(id) PRIMARY KEY,
	id_contract	int NOT NULL REFERENCES Contract(id)
);
