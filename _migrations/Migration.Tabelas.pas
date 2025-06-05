unit Migration.Tabelas;

interface

uses System.SysUtils, System.Classes, Model.DBTable, Migration, DCollections;

type
  TMigrationTabelas = class(TMigration)
  public
      procedure CreateMigrations; override;


      procedure Tabela_AJUSTE_ICMS;
  end;

implementation

{ TMigrationTabelas }

procedure TMigrationTabelas.CreateMigrations;
begin
   Tabela_AJUSTE_ICMS;
end;

procedure TMigrationTabelas.Tabela_AJUSTE_ICMS;
begin
  Table('CLIENTES')
     .Column('MEU_CAMPO','VARCHAR(10) CHARACTER SET WIN1252 DEFAULT ''TESTANDO'' NOT NULL COLLATE WIN_PTBR')
     .Trigger('CREATE OR ALTER TRIGGER CLIENTES_BI FOR CLIENTES '+sLineBreak+
          'ACTIVE BEFORE INSERT POSITION 0    '+slinebreak+
          'as                                 '+slinebreak+
          'begin                              '+slinebreak+
          '  if (new.id is null) then         '+slinebreak+
          '    new.id = gen_id(gen_clientes_id,1);  '+slinebreak+
          'end  ^');

end;

end.
