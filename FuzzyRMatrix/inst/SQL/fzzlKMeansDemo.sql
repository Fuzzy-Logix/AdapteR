CREATE MULTISET TABLE FL_R_WRAP.tblKMeansDemo ,NO FALLBACK ,
     NO BEFORE JOURNAL,
     NO AFTER JOURNAL,
     CHECKSUM = DEFAULT,
     DEFAULT MERGEBLOCKRATIO
     (
      ID BIGINT,
      X FLOAT,
      Y FLOAT)
PRIMARY INDEX ( ID );

INSERT tblKMeansDemo 
SELECT  a.SerialVal AS ID,
    FLSimNormal(a.SerialVal, 0.0, 1.0) AS X,
    FLSimNormal(a.SerialVal+1000, 0.0, 1.0) AS Y
FROM fzzlSerial a
WHERE a.SerialVal <= 100;

INSERT tblKMeansDemo 
SELECT  a.SerialVal AS ID,
    FLSimNormal(a.SerialVal, 10.0, 1.0) AS X,
    FLSimNormal(a.SerialVal+1500, 10.0, 1.0) AS Y
FROM fzzlSerial a
WHERE a.SerialVal > 100 AND a.SerialVal <= 200;

SELECT * FROM tblKMeansDemo;

-- DELETE FROM fzzlKMeansDemo WHERE 1 = 1;