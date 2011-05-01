CREATE OR REPLACE FUNCTION getNamesLike(varchar)
	RETURNS setof PF AS
'
	SELECT *
	FROM PF
	WHERE nume LIKE $1
'
LANGUAGE 'sql' VOLATILE;

CREATE OR REPLACE FUNCTION checkPF(varchar, varchar, varchar)
	RETURNS boolean AS
'
	DECLARE
		result integer;
		p_nume ALIAS FOR $1;
		p_adresa ALIAS FOR $2;
		p_telefon ALIAS FOR $3;
	BEGIN
		SELECT INTO result COUNT(*)
		FROM PF
		WHERE nume = p_nume AND adresa = p_adresa AND telefon = p_telefon;

		IF result > 0 THEN
			RETURN TRUE;
		ELSE
			RETURN FALSE;
		END IF;
	END
'
LANGUAGE 'plpgsql' VOLATILE;

-- Returns the id_pf for the person identified by the three parameters
-- The parameters are (nume,adresa,telefon). If no such person exists,
-- -1 is returned
CREATE OR REPLACE FUNCTION getPFFor(varchar,varchar,varchar)
	RETURNS integer AS
'
	DECLARE
		result integer;
		p_nume ALIAS FOR $1;
		p_adresa ALIAS FOR $2;
		p_telefon ALIAS FOR $3;
	BEGIN
		SELECT INTO result id
		FROM PF
		WHERE nume = p_nume AND adresa = p_adresa AND telefon = p_telefon;

		IF result IS NULL THEN
			RETURN -1;
		ELSE
			RETURN result;
		END IF;
	END
'
LANGUAGE 'plpgsql' VOLATILE;

CREATE OR REPLACE FUNCTION checkCodCerere(int, int)
	RETURNS boolean AS
'
	DECLARE
		result integer;
		p_cod ALIAS FOR $1;
		p_an ALIAS FOR $2;
	BEGIN
		SELECT INTO result COUNT(*)
		FROM Cerere
		WHERE cod = p_cod AND an = p_an;

		IF result > 0 THEN
			RETURN TRUE;
		ELSE
			RETURN FALSE;
		END IF;
	END
'
LANGUAGE 'plpgsql' VOLATILE;

CREATE OR REPLACE FUNCTION getFabricanti()
	RETURNS setof Fabricant AS
'
	SELECT *
	FROM Fabricant
'
LANGUAGE 'sql' VOLATILE;

CREATE OR REPLACE FUNCTION insertFabricant(varchar)
	RETURNS void AS
'
	INSERT INTO FabricantApometre VALUES (DEFAULT, $1)
'
LANGUAGE 'sql' VOLATILE;

CREATE OR REPLACE FUNCTION getDNs()
	RETURNS setof DN AS
'
	SELECT *
	FROM DN
'
LANGUAGE 'sql' VOLATILE;

CREATE OR REPLACE FUNCTION getTipuriCerere()
	RETURNS setof TipCerere AS
'
	SELECT *
	FROM TipCerere
'
LANGUAGE 'sql' VOLATILE;

CREATE OR REPLACE FUNCTION getSolicitanti()
	RETURNS setof Solicitant AS
'
	SELECT *
	FROM Solicitant
'
LANGUAGE 'sql' VOLATILE;

CREATE OR REPLACE FUNCTION getCalitati()
	RETURNS setof Calitate AS
'
	SELECT *
	FROM Calitate
'
LANGUAGE 'sql' VOLATILE;

CREATE OR REPLACE FUNCTION getIdForDomeniuPublic()
	RETURNS int AS
'
	DECLARE
		result integer;
	BEGIN
		SELECT INTO result id
		FROM Domeniu
		WHERE nume = ''public'';

		IF result is null THEN
			RETURN -1;
		ELSE
			RETURN result;
		END IF;
	END
'
LANGUAGE 'plpgsql' VOLATILE;

CREATE OR REPLACE FUNCTION getIdForDomeniuPrivat()
	RETURNS int AS
'
	DECLARE
		result integer;
	BEGIN
		SELECT INTO result id
		FROM Domeniu
		WHERE nume = ''privat'';

		IF result is null THEN
			RETURN -1;
		ELSE
			RETURN result;
		END IF;
	END
'
LANGUAGE 'plpgsql' VOLATILE;

-- Inserts a new PF into the PF database and returns its id.
-- If an entry with the exact same values already exists, it
-- doesn't insert anything and returns this entry's id instead.
CREATE OR REPLACE FUNCTION insertPF(varchar,varchar,varchar)
	RETURNS int AS
'
	DECLARE
		id int;
		p_nume ALIAS FOR $1;
		p_adresa ALIAS FOR $2;
		p_telefon ALIAS FOR $3;
	BEGIN
		SELECT INTO id PF.id
		FROM PF
		WHERE nume = p_nume AND adresa = p_adresa AND telefon = p_telefon;

		IF id IS NULL THEN
			INSERT INTO PF VALUES (DEFAULT, p_nume, p_adresa, p_telefon);
			RETURN currval(''PF_id_seq'');
		ELSE
			RETURN id;
		END IF;
	END
'
LANGUAGE 'plpgsql' VOLATILE;

-- Inserts a new CererePF into the database and returns its id.
-- The parameters are (cod,an,id_tip,id_calitate,id_solicitant,data_cerere,data_scadenta,id_pf).
-- id_pf should be a valid id from the PF table identifying the PF this CererePF belongs to.
CREATE OR REPLACE FUNCTION insertCererePF(int,int,int,int,int,date,date,int)
	RETURNS int AS
'
	DECLARE
		id int;
	BEGIN
		INSERT INTO Cerere VALUES (DEFAULT,$1,$2,$3,$4,$5,$6,$7);
		id := currval(''Cerere_id_seq'');
		INSERT INTO CererePF VALUES (id,$8);
		RETURN id;
	END
'
LANGUAGE 'plpgsql' VOLATILE;

-- Inserts a new Apometru into the Apometru database and returns its id.
-- If an entry with the exact same values for id_fabricant and serie already
-- exists, it doesn't insert anything and returns this entry's id instead.
-- The parameters are (id_fabricant,serie,id_dn,clasa,id_domeniu,observatii)
CREATE OR REPLACE FUNCTION insertApometru(int,varchar,int,char(1),int,varchar)
	RETURNS int AS
'
	DECLARE
		id int;
		p_id_fabricant ALIAS FOR $1;
		p_serie ALIAS FOR $2;
		p_id_dn ALIAS FOR $3;
		p_clasa ALIAS FOR $4;
		p_id_domeniu ALIAS FOR $5;
		p_observatii ALIAS FOR $6;
	BEGIN
		SELECT INTO id Apometru.id
		FROM Apometru
		WHERE id_fabricant = p_id_fabricant AND serie = p_serie;

		IF id IS NULL THEN
			INSERT INTO Apometru VALUES (DEFAULT,p_id_fabricant,p_serie,p_id_dn,p_clasa,p_id_domeniu,p_observatii);
			RETURN currval(''Apometru_id_seq'');
		ELSE
			RETURN id;
		END IF;
	END
'
LANGUAGE 'plpgsql' VOLATILE;

-- Inserts a new CerereApometru into the database.
-- The parameters are (id_cerere,id_apometru).
CREATE OR REPLACE FUNCTION insertCerereApometru(int,int)
	RETURNS void AS
'
	INSERT INTO CerereApometru VALUES ($1,$2);
'
LANGUAGE 'sql' VOLATILE;
