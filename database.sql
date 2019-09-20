DELETE FROM userinput;
DELETE FROM stocktable_1;
SELECT * FROM stocktable_1;

GRANT INSERT ON stocktable TO 'financeuser';

ALTER USER 'user'@'localhost' IDENTIFIED WITH mysql_native_password BY 'user';

SHOW VARIABLES LIKE 'local_infile';
SET GLOBAL local_infile = 1;