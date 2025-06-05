unit Migration;

interface

uses  System.Classes,System.SysUtils, Model.DBObject,Splitters, Sql.Script.Builder,System.Generics.collections,DCollections,
  Model.DBTable, Model.DBTrigger, Model.DBIndex, Model.DBView, Model.DBProcedure,Model.DBFunction,
  Model.DBGenerator,Table.Builder;

type TMigration = class

private
  FScripts: IScriptBuilder;
  function GetTables: TList<TDBTable>;

protected
  FTableBuilders : TList<ITableBuilder>;


  property Scripts : IScriptBuilder read FScripts;

  function Table(aName: string) : ITableBuilder;

public
  constructor Create();

  procedure CreateMigrations(); virtual;

  property Tables : TList<TDBTable> read GetTables;

end;

implementation

{ TMigration }


constructor TMigration.Create();
begin
    FScripts := TScriptBuilder.Create;
    FTableBuilders := TList<ITableBuilder>.Create;
end;


procedure TMigration.CreateMigrations;
begin
  raise Exception.Create('Sobrescreva o método CreateMigrations');
end;



function TMigration.GetTables: TList<TDBTable>;
var
 builder : ITableBuilder;
begin
  Result := TList<TDBTable>.Create;
  for builder in FTableBuilders do begin
      Result.Add(builder.Build);
  end;
end;

function TMigration.Table(aName : string): ITableBuilder;
var
 index : integer;
begin

  index := FTableBuilders.IndexOf(function(I : ITableBuilder) : boolean
  begin
     Result := i.GetTableName = aName;
  end);

  if (index = -1) then begin
     result :=  TTableBuilder.Create
              .SetTable(TDBTable.Create(aName));

     FTableBuilders.Add(result);
  end
  else begin
     result := FTableBuilders[index];
  end;
end;

end.
