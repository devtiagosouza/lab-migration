select  trim(t.rdb$relation_name) as name, t.rdb$view_source as source from rdb$relations t
where t.rdb$system_flag = 0