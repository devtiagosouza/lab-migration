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
var
  tabela : TDBTable;
  tabelas : TList<TDBTable>;
begin


 // tabela :=
    Table('AJUSTE_ICMS')
     .Column('MEU_CAMPO','VARCHAR(10) CHARACTER SET WIN1252 DEFAULT ''TESTANDO'' NOT NULL COLLATE WIN_PTBR');


     tabelas := FTables;
end;

end.
