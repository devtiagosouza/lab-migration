select g.rdb$generator_name as generator_name
from rdb$generators g
left join rdb$dependencies d on d.rdb$depended_on_name = g.rdb$generator_name
where rdb$system_flag = 0
and d.rdb$dependent_name is null