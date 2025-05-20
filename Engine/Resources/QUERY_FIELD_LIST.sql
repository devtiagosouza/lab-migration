SELECT R.rdb$field_name as field_name FROM RDB$RELATION_FIELDS R
WHERE R.rdb$relation_name = :VIEW_NAME
order by r.rdb$field_position;