unit Migration;

interface

uses  System.Classes,System.SysUtils, Model.DBObject,Splitters, Sql.Script.Builder,DCollections,
  Model.DBTable, Model.DBTrigger, Model.DBIndex, Model.DBView, Model.DBProcedure,Model.DBFunction,
  Model.DBGenerator,Table.Builder;

type TMigration = class

private
    FScripts: IScriptBuilder;

protected
  FTables : TList<TDBTable>;

  property Scripts : IScriptBuilder read FScripts;
  function Table(aName: string) : ITableBuilder;

public

  constructor Create();

  procedure CreateMigrations(); virtual;

end;

implementation

{ TMigration }


constructor TMigration.Create();
begin
    FScripts := TScriptBuilder.Create;
    FTables := TList<TDBTable>.Create;
end;


procedure TMigration.CreateMigrations;
begin
  raise Exception.Create('Sobrescreva o método CreateMigrations');
end;



function TMigration.Table(aName : string): ITableBuilder;
var
 vTable : TDBTable;
 index : integer;
begin

  index := FTables.IndexOf(function(T : TDBTable) : boolean
  begin
     Result := T.Name = aName;
  end);

  if (index = -1) then begin
    vTable := TDBTable.Create;
    vTable.Name := aName;
    FTables.Add(vTable);
  end
  else begin
    vTable := FTables[index];
  end;

  Result := TTableBuilder.Create
              .SetTable(vTable);

end;

end.
