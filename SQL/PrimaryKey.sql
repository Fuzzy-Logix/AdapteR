SELECT	DatabaseName, TableName, IndexNumber, IndexType, ColumnName
FROM	DBC.Indices 
WHERE TableName = '%s' AND DatabaseName = '%s'