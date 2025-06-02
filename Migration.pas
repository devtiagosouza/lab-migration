unit Migration;

interface

uses  System.Classes,System.SysUtils, Model.DBObject,Splitters, Sql.Script.Builder,DCollections,
  Model.DBTable, Model.DBTrigger, Model.DBIndex, Model.DBView, Model.DBProcedure,Model.DBFunction,
  Model.DBGenerator,Table.Builder, Database.Interfaces ;

type TMigration = class

private
    FDatabaseModel : IDatabase;
    FScripts: IScriptBuilder;

protected
  property Scripts : IScriptBuilder read FScripts;
  function Table(aName: string) : ITableBuilder;

public
  procedure SetDatabaseModel(ADatabase: IDatabase);

  constructor Create();

  procedure CreateMigrations(); virtual;

end;

implementation

{ TMigration }


constructor TMigration.Create();
begin
    FScripts := TScriptBuilder.Create;
end;


procedure TMigration.CreateMigrations;
begin
  raise Exception.Create('Sobrescreva o método CreateMigrations');
end;

procedure TMigration.SetDatabaseModel(ADatabase: IDatabase);
begin
  FDatabaseModel := ADatabase;
end;

function TMigration.Table(aName : string): ITableBuilder;
var
 vTable : TDBTable;
begin
  vTable := FDatabaseModel.GetTables.First(function(T : TDBTable) : boolean
  begin
     Result := T.Name = aName;
  end);

  if (vTable = nil) then begin
    result := TTableBuilder.Create()
              .New(aName);
  end
  else begin
    result := TTableBuilder.Create
              .SetTable(vTable);
  end;
end;

end.
