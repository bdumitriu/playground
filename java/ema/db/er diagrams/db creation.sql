
CREATE RULE exact_4_cifre
 AS @col BETWEEN 1000 AND 9999
go

CREATE RULE exact_cinci_cifre
 AS @col BETWEEN 10000 AND 99999
go

CREATE RULE serie_peste_8_caracter
 AS char_length(serie) >= 8
go

CREATE RULE tipuri_calitate
 AS @col IN ('d', 'D', 'u', 'U', 'm', 'M', 'r', 'R')
go

CREATE RULE tipuri_cerere
 AS @col IN ('i', 'I', 'v', 'V', 'r', 'R')
go

CREATE RULE tipuri_dn
 AS @col IN (15, 20, 25, 30, 50, 80)
go

CREATE RULE tipuri_solicitant
 AS @col IN ('pf', 'PF', 'Pf', 'pF', 'pj', 'PJ', 'Pj', 'pJ')
go

exec sp_addtype adresa, "varchar(150)", "NULL"
go

exec sp_addtype identificator_unic, "int", "NULL"
go

exec sp_addtype numar_telefon, "varchar(20)", "NULL"
go

exec sp_addtype nume_firma, "varchar(50)", "NULL"
go

exec sp_addtype nume_persoana, "varchar(50)", "NULL"
go

CREATE TABLE Apometru (
       id                   identificator_unic NOT NULL,
       id_fabricant         identificator_unic NOT NULL,
       serie                varchar(10) NOT NULL,
       dn                   bit NOT NULL,
       clasa                char(1) NULL,
       domeniu              bit NOT NULL,
       observatii           text NULL
)
go

CREATE UNIQUE INDEX XAK1Apometru ON Apometru
(
       serie                          ,
       id_fabricant                   ASC
)
go


ALTER TABLE Apometru
       ADD PRIMARY KEY (id)
go


exec sp_bindrule serie_peste_8_caracter, 'Apometru.serie'
exec sp_bindrule tipuri_dn, 'Apometru.dn'
go

CREATE TABLE Banca (
       id                   identificator_unic NOT NULL,
       nume                 nume_firma NOT NULL,
       adresa               adresa,
       telefon              numar_telefon,
       fax                  numar_telefon
)
go


ALTER TABLE Banca
       ADD PRIMARY KEY (id)
go


CREATE TABLE Cerere (
       id                   identificator_unic NOT NULL,
       cod                  int NOT NULL,
       an                   bit NOT NULL,
       tip                  char(1) NOT NULL,
       calitate             char(1) NULL,
       data_cerere          datetime NULL,
       data_scadenta        datetime NULL,
       solicitant           char(2) NOT NULL
)
go

CREATE UNIQUE INDEX XAK1Cerere ON Cerere
(
       cod                            ,
       an                             
)
go


ALTER TABLE Cerere
       ADD PRIMARY KEY (id)
go


exec sp_bindrule exact_4_cifre, 'Cerere.an'
exec sp_bindrule tipuri_cerere, 'Cerere.tip'
exec sp_bindrule tipuri_calitate, 'Cerere.calitate'
exec sp_bindrule tipuri_solicitant, 'Cerere.solicitant'
go

CREATE TABLE CerereApometru (
       id_cerere            identificator_unic NOT NULL,
       id_apometru          identificator_unic NOT NULL
)
go


ALTER TABLE CerereApometru
       ADD PRIMARY KEY (id_cerere, id_apometru)
go


CREATE TABLE CererePF (
       id                   identificator_unic NOT NULL,
       id_pf                identificator_unic NOT NULL
)
go


ALTER TABLE CererePF
       ADD PRIMARY KEY (id)
go


CREATE TABLE CererePJ (
       id                   identificator_unic NOT NULL,
       id_contract          identificator_unic NOT NULL
)
go


ALTER TABLE CererePJ
       ADD PRIMARY KEY (id)
go


CREATE TABLE Contract (
       id                   identificator_unic NOT NULL,
       cod                  int NOT NULL,
       an                   bit NOT NULL,
       id_firma             identificator_unic NOT NULL,
       data_incheiere       datetime NULL
)
go

CREATE UNIQUE INDEX XAK1Contract ON Contract
(
       cod                            ,
       an                             
)
go


ALTER TABLE Contract
       ADD PRIMARY KEY (id)
go


exec sp_bindrule exact_4_cifre, 'Contract.an'
go

CREATE TABLE Fabricant (
       id                   identificator_unic NOT NULL,
       nume                 nume_firma NOT NULL
)
go


ALTER TABLE Fabricant
       ADD PRIMARY KEY (id)
go


CREATE TABLE PF (
       id                   identificator_unic NOT NULL,
       nume                 nume_persoana NOT NULL,
       adresa               adresa,
       telefon              numar_telefon
)
go


ALTER TABLE PF
       ADD PRIMARY KEY (id)
go


CREATE TABLE PJ (
       id                   identificator_unic NOT NULL,
       nume                 adresa NOT NULL,
       adresa_sediu         adresa,
       adresa_lucru         adresa,
       cui                  varchar(8) NOT NULL,
       orc1                 varchar(3) NULL,
       orc2                 varchar(5) NULL,
       orc3                 varchar(4) NULL,
       cont                 varchar(25) NULL,
       id_banca             identificator_unic,
       nume_contact         nume_persoana,
       tel_contact          numar_telefon,
       telefon              numar_telefon,
       fax                  numar_telefon
)
go

CREATE UNIQUE INDEX XAK1PJ ON PJ
(
       cui                            
)
go


ALTER TABLE PJ
       ADD PRIMARY KEY (id)
go


ALTER TABLE Apometru
       ADD FOREIGN KEY (id_fabricant)
                             REFERENCES Fabricant
go


ALTER TABLE CerereApometru
       ADD FOREIGN KEY (id_cerere)
                             REFERENCES Cerere
go


ALTER TABLE CerereApometru
       ADD FOREIGN KEY (id_apometru)
                             REFERENCES Apometru
go


ALTER TABLE CererePF
       ADD FOREIGN KEY (id)
                             REFERENCES Cerere
go


ALTER TABLE CererePF
       ADD FOREIGN KEY (id_pf)
                             REFERENCES PF
go


ALTER TABLE CererePJ
       ADD FOREIGN KEY (id)
                             REFERENCES Cerere
go


ALTER TABLE CererePJ
       ADD FOREIGN KEY (id_contract)
                             REFERENCES Contract
go


ALTER TABLE Contract
       ADD FOREIGN KEY (id_firma)
                             REFERENCES PJ
go


ALTER TABLE PJ
       ADD FOREIGN KEY (id_banca)
                             REFERENCES Banca
go




create trigger tD_Apometru on Apometru for DELETE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* DELETE trigger on Apometru */
begin
  declare  @errno   int,
           @errmsg  varchar(255)
    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    /* Apometru R/3 CerereApometru ON PARENT DELETE RESTRICT */
    if exists (
      select * from deleted,CerereApometru
      where
        /*  CerereApometru.id_apometru = deleted.id */
        CerereApometru.id_apometru = deleted.id
    )
    begin
      select @errno  = 30001,
             @errmsg = 'Cannot DELETE Apometru because CerereApometru exists.'
      goto error
    end


    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tI_Apometru on Apometru for INSERT as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* INSERT trigger on Apometru */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Fabricant R/5 Apometru ON CHILD INSERT RESTRICT */
  if
    /* update(id_fabricant) */
    update(id_fabricant)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Fabricant
        where
          /* inserted.id_fabricant = Fabricant.id */
          inserted.id_fabricant = Fabricant.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30002,
             @errmsg = 'Cannot INSERT Apometru because Fabricant does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_Apometru on Apometru for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on Apometru */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Apometru R/3 CerereApometru ON PARENT UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    if exists (
      select * from deleted,CerereApometru
      where
        /*  CerereApometru.id_apometru = deleted.id */
        CerereApometru.id_apometru = deleted.id
    )
    begin
      select @errno  = 30005,
             @errmsg = 'Cannot UPDATE Apometru because CerereApometru exists.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Fabricant R/5 Apometru ON CHILD UPDATE RESTRICT */
  if
    /* update(id_fabricant) */
    update(id_fabricant)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Fabricant
        where
          /* inserted.id_fabricant = Fabricant.id */
          inserted.id_fabricant = Fabricant.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30007,
             @errmsg = 'Cannot UPDATE Apometru because Fabricant does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tD_Banca on Banca for DELETE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* DELETE trigger on Banca */
begin
  declare  @errno   int,
           @errmsg  varchar(255)
    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    /* Banca R/9 PJ ON PARENT DELETE SET NULL */
    update PJ
      set
        /* PJ.id_banca = NULL */
        PJ.id_banca = NULL
      from PJ,deleted
      where
        /* PJ.id_banca = deleted.id */
        PJ.id_banca = deleted.id


    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_Banca on Banca for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on Banca */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Banca R/9 PJ ON PARENT UPDATE SET NULL */
  if
    /* update(id) */
    update(id)
  begin
    update PJ
      set
        /* PJ.id_banca = NULL */
        PJ.id_banca = NULL
      from PJ,deleted
      where
        /* PJ.id_banca = deleted.id */
        PJ.id_banca = deleted.id
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tD_Cerere on Cerere for DELETE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* DELETE trigger on Cerere */
begin
  declare  @errno   int,
           @errmsg  varchar(255)
    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    /* Cerere R/8 CererePJ ON PARENT DELETE RESTRICT */
    if exists (
      select * from deleted,CererePJ
      where
        /*  CererePJ.id = deleted.id */
        CererePJ.id = deleted.id
    )
    begin
      select @errno  = 30001,
             @errmsg = 'Cannot DELETE Cerere because CererePJ exists.'
      goto error
    end

    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    /* Cerere R/7 CererePF ON PARENT DELETE RESTRICT */
    if exists (
      select * from deleted,CererePF
      where
        /*  CererePF.id = deleted.id */
        CererePF.id = deleted.id
    )
    begin
      select @errno  = 30001,
             @errmsg = 'Cannot DELETE Cerere because CererePF exists.'
      goto error
    end

    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    /* Cerere R/4 CerereApometru ON PARENT DELETE RESTRICT */
    if exists (
      select * from deleted,CerereApometru
      where
        /*  CerereApometru.id_cerere = deleted.id */
        CerereApometru.id_cerere = deleted.id
    )
    begin
      select @errno  = 30001,
             @errmsg = 'Cannot DELETE Cerere because CerereApometru exists.'
      goto error
    end


    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_Cerere on Cerere for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on Cerere */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Cerere R/8 CererePJ ON PARENT UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    if exists (
      select * from deleted,CererePJ
      where
        /*  CererePJ.id = deleted.id */
        CererePJ.id = deleted.id
    )
    begin
      select @errno  = 30005,
             @errmsg = 'Cannot UPDATE Cerere because CererePJ exists.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Cerere R/7 CererePF ON PARENT UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    if exists (
      select * from deleted,CererePF
      where
        /*  CererePF.id = deleted.id */
        CererePF.id = deleted.id
    )
    begin
      select @errno  = 30005,
             @errmsg = 'Cannot UPDATE Cerere because CererePF exists.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Cerere R/4 CerereApometru ON PARENT UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    if exists (
      select * from deleted,CerereApometru
      where
        /*  CerereApometru.id_cerere = deleted.id */
        CerereApometru.id_cerere = deleted.id
    )
    begin
      select @errno  = 30005,
             @errmsg = 'Cannot UPDATE Cerere because CerereApometru exists.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tI_CerereApometru on CerereApometru for INSERT as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* INSERT trigger on CerereApometru */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Cerere R/4 CerereApometru ON CHILD INSERT RESTRICT */
  if
    /* update(id_cerere) */
    update(id_cerere)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Cerere
        where
          /* inserted.id_cerere = Cerere.id */
          inserted.id_cerere = Cerere.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30002,
             @errmsg = 'Cannot INSERT CerereApometru because Cerere does not exist.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Apometru R/3 CerereApometru ON CHILD INSERT RESTRICT */
  if
    /* update(id_apometru) */
    update(id_apometru)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Apometru
        where
          /* inserted.id_apometru = Apometru.id */
          inserted.id_apometru = Apometru.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30002,
             @errmsg = 'Cannot INSERT CerereApometru because Apometru does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_CerereApometru on CerereApometru for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on CerereApometru */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid_cerere identificator_unic, 
           @insid_apometru identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Cerere R/4 CerereApometru ON CHILD UPDATE RESTRICT */
  if
    /* update(id_cerere) */
    update(id_cerere)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Cerere
        where
          /* inserted.id_cerere = Cerere.id */
          inserted.id_cerere = Cerere.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30007,
             @errmsg = 'Cannot UPDATE CerereApometru because Cerere does not exist.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Apometru R/3 CerereApometru ON CHILD UPDATE RESTRICT */
  if
    /* update(id_apometru) */
    update(id_apometru)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Apometru
        where
          /* inserted.id_apometru = Apometru.id */
          inserted.id_apometru = Apometru.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30007,
             @errmsg = 'Cannot UPDATE CerereApometru because Apometru does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tI_CererePF on CererePF for INSERT as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* INSERT trigger on CererePF */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Cerere R/7 CererePF ON CHILD INSERT RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Cerere
        where
          /* inserted.id = Cerere.id */
          inserted.id = Cerere.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30002,
             @errmsg = 'Cannot INSERT CererePF because Cerere does not exist.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* PF R/6 CererePF ON CHILD INSERT RESTRICT */
  if
    /* update(id_pf) */
    update(id_pf)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,PF
        where
          /* inserted.id_pf = PF.id */
          inserted.id_pf = PF.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30002,
             @errmsg = 'Cannot INSERT CererePF because PF does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_CererePF on CererePF for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on CererePF */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Cerere R/7 CererePF ON CHILD UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Cerere
        where
          /* inserted.id = Cerere.id */
          inserted.id = Cerere.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30007,
             @errmsg = 'Cannot UPDATE CererePF because Cerere does not exist.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* PF R/6 CererePF ON CHILD UPDATE RESTRICT */
  if
    /* update(id_pf) */
    update(id_pf)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,PF
        where
          /* inserted.id_pf = PF.id */
          inserted.id_pf = PF.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30007,
             @errmsg = 'Cannot UPDATE CererePF because PF does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tI_CererePJ on CererePJ for INSERT as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* INSERT trigger on CererePJ */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Cerere R/8 CererePJ ON CHILD INSERT RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Cerere
        where
          /* inserted.id = Cerere.id */
          inserted.id = Cerere.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30002,
             @errmsg = 'Cannot INSERT CererePJ because Cerere does not exist.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Contract R/1 CererePJ ON CHILD INSERT RESTRICT */
  if
    /* update(id_contract) */
    update(id_contract)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Contract
        where
          /* inserted.id_contract = Contract.id */
          inserted.id_contract = Contract.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30002,
             @errmsg = 'Cannot INSERT CererePJ because Contract does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_CererePJ on CererePJ for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on CererePJ */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Cerere R/8 CererePJ ON CHILD UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Cerere
        where
          /* inserted.id = Cerere.id */
          inserted.id = Cerere.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30007,
             @errmsg = 'Cannot UPDATE CererePJ because Cerere does not exist.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Contract R/1 CererePJ ON CHILD UPDATE RESTRICT */
  if
    /* update(id_contract) */
    update(id_contract)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,Contract
        where
          /* inserted.id_contract = Contract.id */
          inserted.id_contract = Contract.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30007,
             @errmsg = 'Cannot UPDATE CererePJ because Contract does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tD_Contract on Contract for DELETE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* DELETE trigger on Contract */
begin
  declare  @errno   int,
           @errmsg  varchar(255)
    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    /* Contract R/1 CererePJ ON PARENT DELETE RESTRICT */
    if exists (
      select * from deleted,CererePJ
      where
        /*  CererePJ.id_contract = deleted.id */
        CererePJ.id_contract = deleted.id
    )
    begin
      select @errno  = 30001,
             @errmsg = 'Cannot DELETE Contract because CererePJ exists.'
      goto error
    end


    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tI_Contract on Contract for INSERT as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* INSERT trigger on Contract */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* PJ R/2 Contract ON CHILD INSERT RESTRICT */
  if
    /* update(id_firma) */
    update(id_firma)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,PJ
        where
          /* inserted.id_firma = PJ.id */
          inserted.id_firma = PJ.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30002,
             @errmsg = 'Cannot INSERT Contract because PJ does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_Contract on Contract for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on Contract */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Contract R/1 CererePJ ON PARENT UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    if exists (
      select * from deleted,CererePJ
      where
        /*  CererePJ.id_contract = deleted.id */
        CererePJ.id_contract = deleted.id
    )
    begin
      select @errno  = 30005,
             @errmsg = 'Cannot UPDATE Contract because CererePJ exists.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* PJ R/2 Contract ON CHILD UPDATE RESTRICT */
  if
    /* update(id_firma) */
    update(id_firma)
  begin
    select @nullcnt = 0
    select @validcnt = count(*)
      from inserted,PJ
        where
          /* inserted.id_firma = PJ.id */
          inserted.id_firma = PJ.id
    /*  */
    
    if @validcnt + @nullcnt != @numrows
    begin
      select @errno  = 30007,
             @errmsg = 'Cannot UPDATE Contract because PJ does not exist.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tD_Fabricant on Fabricant for DELETE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* DELETE trigger on Fabricant */
begin
  declare  @errno   int,
           @errmsg  varchar(255)
    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    /* Fabricant R/5 Apometru ON PARENT DELETE RESTRICT */
    if exists (
      select * from deleted,Apometru
      where
        /*  Apometru.id_fabricant = deleted.id */
        Apometru.id_fabricant = deleted.id
    )
    begin
      select @errno  = 30001,
             @errmsg = 'Cannot DELETE Fabricant because Apometru exists.'
      goto error
    end


    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_Fabricant on Fabricant for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on Fabricant */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Fabricant R/5 Apometru ON PARENT UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    if exists (
      select * from deleted,Apometru
      where
        /*  Apometru.id_fabricant = deleted.id */
        Apometru.id_fabricant = deleted.id
    )
    begin
      select @errno  = 30005,
             @errmsg = 'Cannot UPDATE Fabricant because Apometru exists.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tD_PF on PF for DELETE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* DELETE trigger on PF */
begin
  declare  @errno   int,
           @errmsg  varchar(255)
    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    /* PF R/6 CererePF ON PARENT DELETE RESTRICT */
    if exists (
      select * from deleted,CererePF
      where
        /*  CererePF.id_pf = deleted.id */
        CererePF.id_pf = deleted.id
    )
    begin
      select @errno  = 30001,
             @errmsg = 'Cannot DELETE PF because CererePF exists.'
      goto error
    end


    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_PF on PF for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on PF */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* PF R/6 CererePF ON PARENT UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    if exists (
      select * from deleted,CererePF
      where
        /*  CererePF.id_pf = deleted.id */
        CererePF.id_pf = deleted.id
    )
    begin
      select @errno  = 30005,
             @errmsg = 'Cannot UPDATE PF because CererePF exists.'
      goto error
    end
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tD_PJ on PJ for DELETE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* DELETE trigger on PJ */
begin
  declare  @errno   int,
           @errmsg  varchar(255)
    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    /* PJ R/2 Contract ON PARENT DELETE RESTRICT */
    if exists (
      select * from deleted,Contract
      where
        /*  Contract.id_firma = deleted.id */
        Contract.id_firma = deleted.id
    )
    begin
      select @errno  = 30001,
             @errmsg = 'Cannot DELETE PJ because Contract exists.'
      goto error
    end


    /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
    return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tI_PJ on PJ for INSERT as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* INSERT trigger on PJ */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Banca R/9 PJ ON CHILD INSERT SET NULL */
  if
    /* update(id_banca) */
    update(id_banca)
  begin
    update PJ
      set
        /* PJ.id_banca = NULL */
        PJ.id_banca = NULL
      from PJ,inserted
      where
        /* PJ.id = inserted.id */
  PJ.id = inserted.id and
   
        not exists (
          select * from Banca
          where
            /* inserted.id_banca = Banca.id */
            inserted.id_banca = Banca.id
        )
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

create trigger tU_PJ on PJ for UPDATE as
/* ERwin Builtin Sun Nov 30 21:44:33 2003 */
/* UPDATE trigger on PJ */
begin
  declare  @numrows int,
           @nullcnt int,
           @validcnt int,
           @insid identificator_unic,
           @errno   int,
           @errmsg  varchar(255)

  select @numrows = @@rowcount
  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* PJ R/2 Contract ON PARENT UPDATE RESTRICT */
  if
    /* update(id) */
    update(id)
  begin
    if exists (
      select * from deleted,Contract
      where
        /*  Contract.id_firma = deleted.id */
        Contract.id_firma = deleted.id
    )
    begin
      select @errno  = 30005,
             @errmsg = 'Cannot UPDATE PJ because Contract exists.'
      goto error
    end
  end

  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  /* Banca R/9 PJ ON CHILD UPDATE SET NULL */
  if
    /* update(id_banca) */
    update(id_banca)
  begin
    update PJ
      set
        /* PJ.id_banca = NULL */
        PJ.id_banca = NULL
      from PJ,inserted
      where
        /* PJ.id = inserted.id */
  PJ.id = inserted.id and
   
        not exists (
          select * from Banca
          where
            /* inserted.id_banca = Banca.id */
            inserted.id_banca = Banca.id
        )
  end


  /* ERwin Builtin Sun Nov 30 21:44:33 2003 */
  return
error:
    raiserror @errno @errmsg
    rollback transaction
end
go

