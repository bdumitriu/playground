-- run with 'psql -f <name-of-file> -U dosuser -d dosdb --password' and
-- use 'dospassword' as password

INSERT INTO Users values ('bdumitriu', 'bdumitriu''s pass');

INSERT INTO UserDetails values ('bdumitriu', 'Bogdan Dumitriu', 'eng.', '030-2641620', 30);

INSERT INTO Appointments values (DEFAULT, '2005-03-10 13:15', '2005-03-10 15:00', 'PT class', 'BBL-???', 'false');
INSERT INTO Appointments values (DEFAULT, '2005-03-10 15:15', '2005-03-10 17:00', 'PT lab', 'BBL-???', 'false');
INSERT INTO Appointments values (DEFAULT, '2005-03-11 11:00', '2005-03-10 14:50', 'DOS class', 'BBL-???', 'false');

INSERT INTO Calendars SELECT 'bdumitriu', id FROM Appointments;
