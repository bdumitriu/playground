CREATE DATABASE traffic;

USE traffic;

CREATE TABLE traffic (
	ACARRIER		VARCHAR(25),
	ACARRIER_TARIFF		VARCHAR(25),
	ACARRIER_TCLASS		VARCHAR(25),
	ACARRIER_TOTCHG		DECIMAL(10,5),
	ACCOUNT			VARCHAR(10),
	ACONDATE		DATETIME,
	ADA			VARCHAR(15),
	ADETDATE		DATETIME,
	ADUR			INT,
	AENDDATE		DATETIME,
	AGENT_ID		INT,
	AMEMO			VARCHAR(100),
	AOA			VARCHAR(15),
	APORT			INT,
	APROT			VARCHAR(25),
	ARAW			INT,
	AREASON			VARCHAR(100),
	AREF			VARCHAR(15) NOT NULL PRIMARY KEY,
	ATARIFF			VARCHAR(25),
	ATOTCHG			DECIMAL(10,5),
	ATS			INT,
	BATCH			INT,
	BCARRIER		VARCHAR(25),
	BCARRIER_TARIFF		VARCHAR(25),
	BCARRIER_TCLASS		VARCHAR(25),
	BCARRIER_TOTCHG		DECIMAL(10,5),
	BCONDATE		DATETIME,
	BDA			VARCHAR(15),
	BDUR			INT,
	BENDDATE		DATETIME,
	BILL_CODE		VARCHAR(100),
	BILL_ID			INT,
	BMACHINE		VARCHAR(25),
	BMEMO			VARCHAR(100),
	BOA			VARCHAR(15),
	BPORT			INT,
	BPROT			VARCHAR(25),
	BRAW			INT,
	BREASON			VARCHAR(100),
	BREF			VARCHAR(15),
	BSTDATE			DATETIME,
	BTARIFF			VARCHAR(25),
	BTOTCHG			DECIMAL(10,5),
	BTS			INT,
	BUILD			VARCHAR(25),
	BZONE			VARCHAR(25),
	CALL_TYPE		INT,
	CCARRIER		VARCHAR(25),
	CCARRIER_TARIFF		VARCHAR(25),
	CCARRIER_TCLASS		VARCHAR(25),
	CCARRIER_TOTCHG		DECIMAL(10,5),
	CCONDATE		DATETIME,
	CDA			VARCHAR(15),
	CDUR			INT,
	CENDDATE		DATETIME,
	CMACHINE		VARCHAR(25),
	CMEMO			VARCHAR(100),
	COA			VARCHAR(15),
	CPORT			INT,
	CPROT			VARCHAR(25),
	CRAW			INT,
	CREASON			VARCHAR(100),
	CREF			VARCHAR(15),
	CSTDATE			DATETIME,
	CTARIFF			VARCHAR(25),
	CTOTCHG			DECIMAL(10,5),
	CTS			INT,
	CZONE			VARCHAR(25),
	END_BAL			DECIMAL(10,5),
	MACHINE			VARCHAR(25),
	OA_CAT			INT,
	PIN			VARCHAR(15),
	SCRIPT_RES		VARCHAR(25),
	SERIAL			INT,
	START_BAL		DECIMAL(10,5),
	WRITETIME		DATETIME
);