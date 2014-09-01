CREATE MULTISET TABLE FL_TRAIN.fzzlKMeansDemo ,NO FALLBACK ,
     NO BEFORE JOURNAL,
     NO AFTER JOURNAL,
     CHECKSUM = DEFAULT,
     DEFAULT MERGEBLOCKRATIO
     (
      ID BIGINT,
      X FLOAT,
      Y FLOAT)
PRIMARY INDEX ( ID );

INSERT fzzlKMeansDemo 
SELECT  a.SerialVal AS ID,
    FLSimNormal(a.SerialVal, 0.0, 1.0) AS X,
    FLSimNormal(a.SerialVal+1000, 0.0, 1.0) AS Y
FROM fzzlSerial a
WHERE a.SerialVal <= 20;

INSERT fzzlKMeansDemo 
SELECT  a.SerialVal AS ID,
    FLSimNormal(a.SerialVal, 5.0, 1.0) AS X,
    FLSimNormal(a.SerialVal+1000, 5.0, 1.0) AS Y
FROM fzzlSerial a
WHERE a.SerialVal > 20 AND a.SerialVal <= 40;

SELECT * FROM fzzlKMeansDemo;

-- DELETE FROM fzzlKMeansDemo WHERE 1 = 1;