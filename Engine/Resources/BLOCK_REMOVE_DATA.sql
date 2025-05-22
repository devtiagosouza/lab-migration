execute block

returns (
  table_name varchar(31),
  qtd  integer
)
as
begin


for select trim(r.rdb$relation_name)  from rdb$relations r
  where r.rdb$system_flag = 0 into :table_name do begin

  execute statement 'select count(*) from '||:table_name into :qtd;
  if (:qtd > 0) then
     execute statement 'delete from '||:table_name;

  suspend;
end

end;
