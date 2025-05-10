select
trim(g.rdb$generator_name) as name,
g.rdb$initial_value as initial_value,
g.rdb$generator_increment as increment,
IIF(d.rdb$dependent_name IS NOT NULL, TRIM(d.rdb$dependent_name), '') AS TRIGGER_NAME
from rdb$generators g
LEFT join RDB$DEPENDENCIES d ON D.rdb$depended_on_name = g.rdb$generator_name AND D.RDB$DEPENDED_ON_TYPE = 14
and D.RDB$DEPENDENT_TYPE = 2
where g.rdb$system_flag = 0
