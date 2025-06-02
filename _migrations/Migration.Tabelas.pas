unit Migration.Tabelas;

interface

uses System.SysUtils, System.Classes, Model.DBTable, Migration;

type
  TMigrationTabelas = class(TMigration)
  public
      procedure Tabela_AJUSTE_ICMS;
      constructor Create;
      procedure CreateMigrations; override;
  end;

implementation


{ TMigrationTabelas }

constructor TMigrationTabelas.Create;
begin

end;

procedure TMigrationTabelas.CreateMigrations;
begin
   Tabela_AJUSTE_ICMS;
end;

procedure TMigrationTabelas.Tabela_AJUSTE_ICMS;
begin
    Table('AJUSTE_ICMS')
     .Column('MEU_CAMPO','VARCHAR(10)');
     // CHARACTER SET WIN1252 DEFAULT ''TESTANDO'' NOT NULL COLLATE WIN_PTBR');
end;

end.
