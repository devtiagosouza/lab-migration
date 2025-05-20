SELECT RC.rdb$constraint_name AS NAME, SG.rdb$index_name AS INDEX_NAME,
       MAX(iif(coalesce(i.rdb$index_type,0) = 1,'DESCENDING','ASCENDING' )) AS SORTING,
       LIST(TRIM(sg.RDB$FIELD_NAME)) AS FIELDS
       FROM RDB$RELATION_CONSTRAINTS rc
       JOIN RDB$INDEX_SEGMENTS sg ON rc.RDB$INDEX_NAME = sg.RDB$INDEX_NAME
       join rdb$indices i on sg.RDB$INDEX_NAME = i.rdb$index_name
       WHERE rc.RDB$RELATION_NAME = :TABLE_NAME
       AND rc.RDB$CONSTRAINT_TYPE = 'PRIMARY KEY'
       GROUP BY RC.rdb$constraint_name, SG.rdb$index_name