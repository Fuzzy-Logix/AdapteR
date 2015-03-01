DROP TABLE tblTrainIDs;
DROP TABLE tblTestIds;

CREATE TABLE tblTrainIDs
AS  ( 
    SELECT	a.ObsID
    FROM	tblCancerData a
    WHERE	FLSimUniform(RANDOM(1, 10000), 0, 1) < 0.8
    )
WITH DATA
PRIMARY INDEX (ObsID);

CREATE TABLE tblTestIds
AS  ( 
    SELECT	a.ObsID
    FROM	tblCancerData a
    WHERE	NOT EXISTS (SELECT  1
                        FROM    tblTrainIDs b
                        WHERE   b.ObsID  = a.ObsID)
    )
WITH DATA
PRIMARY INDEX (ObsID);

--Creating Test and Train Sets
DROP TABLE tblCancerData_Train;
DROP TABLE tblCancerData_Test;

CREATE TABLE tblCancerData_Train AS
(
	SELECT 	a.*
	FROM	tblCancerData a,
			tblTrainIDs b
	WHERE	a.ObsID = b.ObsID
) WITH DATA;

CREATE TABLE tblCancerData_Test AS
(
	SELECT 	a.*
	FROM	tblCancerData a,
			tblTestIDs b
	WHERE	a.ObsID = b.ObsID
) WITH DATA;
