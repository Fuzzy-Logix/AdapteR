CREATE TABLE FL_R_WRAP.tblCancerData(
 	ObsID BIGINT PRIMARY KEY NOT NULL,
 	SampleCode BIGINT,
 	ClumpThickness INTEGER,
 	CellSizeUniformity INTEGER,
 	CellShapeUniformity INTEGER,
 	MarginalAdhesion INTEGER,
 	SingleEpithelialCellSize INTEGER,
 	BareNuclei INTEGER,
    BlandChromatin INTEGER,
    NormalNucleoli INTEGER,
    Mitoses INTEGER,
    BenignOrMalignant BYTEINT
 );
 
 
 
 INSERT INTO FL_R_WRAP.tblCancerData (ObsID, SampleCode, ClumpThickness, CellSizeUniformity, CellShapeUniformity, MarginalAdhesion, 
 SingleEpithelialCellSize, BareNuclei, BlandChromatin, NormalNucleoli, Mitoses, BenignOrMalignant)
 VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
 
 SELECT * FROM FL_R_WRAP.tblCancerData
 
 UPDATE FL_R_WRAP.tblCancerData
 SET BenignOrMalignant = 0 
 WHERE BenignOrMalignant = 2
 
 UPDATE FL_R_WRAP.tblCancerData
 SET BenignOrMalignant = 1 
 WHERE BenignOrMalignant = 4