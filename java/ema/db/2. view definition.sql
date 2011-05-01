CREATE VIEW ApometruView (id, fabricant, serie, dn, clasa, domeniu, observatii) AS
	SELECT a.id, f.nume, a.serie, dn.numar, a.clasa, d.nume, a.observatii
	FROM Apometru a, FabricantApometre f, DN dn, Domeniu d
	WHERE a.id_fabricant = f.id AND a.id_dn = dn.id AND a.id_domeniu = d.id;

CREATE VIEW ContractView (id, cod, an, id_firma, nume, adresa_sediu, adresa_lucru, cui, orc, cont, banca, nume_contact, tel_contact, telefon, fax) AS
	SELECT c.id, c.cod, c.an, f.id, f.nume, f.adresa_sediu, f.adresa_lucru, f.cui, f.orc1 || '/' || f.orc2 || '/' || f.orc3, f.cont, b.nume, f.nume_contact, f.tel_contact, f.telefon, f.fax
	FROM Contract c, PJ f, Banca b
	WHERE c.id_firma = f.id AND f.id_banca = b.id;

CREATE VIEW CererePFView (id, cod, an, id_pf, nume, adresa, telefon, tip, calitate, solicitant, data_cerere, data_scadenta) AS
	SELECT c.id, c.cod, c.an, pf.id, pf.nume, pf.adresa, pf.telefon, t.nume, cal.nume, s.nume, c.data_cerere, c.data_scadenta
	FROM Cerere c, CererePF cpf, PF pf, Solicitant s, TipCerere t, Calitate cal
	WHERE c.id = cpf.id AND c.id_calitate = cal.id AND c.id_solicitant = s.id AND c.id_tip = t.id AND cpf.id_pf = pf.id;

CREATE VIEW CererePJView (id, cod, an, id_firma, cod_contract, an_contract, nume, adresa_sediu, adresa_lucru, cui, orc, cont, banca, nume_contact, tel_contact, telefon, fax) AS
	SELECT c.id, c.cod, c.an, cv.id_firma, cv.cod, cv.an, cv.nume, cv.adresa_sediu, cv.adresa_lucru, cv.cui, cv.orc, cv.cont, cv.banca, cv.nume_contact, cv.tel_contact, cv.telefon, cv.fax
	FROM Cerere c, CererePJ cpj, ContractView cv
	WHERE c.id = cpj.id AND cpj.id_contract = cv.id;