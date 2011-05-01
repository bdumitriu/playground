create database lt_web;
use lt_web;
create table clienti
	(id mediumint unsigned not null auto_increment primary key,
	 nume varchar(30) not null,
 	 email varchar(35) not null,
	 telefon varchar(40) default "N/A",
	 firma varchar(30) default "N/A",
	 telfirma varchar(40) default "N/A",
	 emailfirma varchar(35) default "N/A",
	 web varchar(50) default "N/A",
	 venit varchar(20) default "N/A",
	 cifraaf varchar(25) default "N/A",
	 domeniu varchar(30) default "N/A",
	 oras varchar(20) default "N/A");
create table calcule
	(id mediumint unsigned not null,
	 tipprodus varchar(20) not null,
	 durata varchar(10) default "N/A",
	 suma varchar(20) default "N/A",
	 tva varchar(3) default "N/A",
	 avans varchar(20) default "N/A",
	 data date);
