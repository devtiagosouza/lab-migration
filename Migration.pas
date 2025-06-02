unit Migration;

interface

uses  System.Classes,System.SysUtils, Model.DBObject,Splitters, Sql.Script.Builder,DCollections,
  Model.DBTable, Model.DBTrigger, Model.DBIndex, Model.DBView, Model.DBProcedure,Model.DBFunction,
  Model.DBGenerator,Table.Builder;

type TMigration = class

private


    FScripts: IScriptBuilder;

    FTables: TList<TDBTable>;
    FViews: TList<TDBView>;
    FProcedures: TList<TDBProcedure>;
    FFunctions: TList<TDBFunction>;
    FTriggers: TList<TDBTrigger>;
    FGenerators: TList<TDBGenerator>;
    FIndices: TList<TDBIndex>;

public
  property Scripts : IScriptBuilder read FScripts;

  function Table(aName: string) : ITableBuilder;

  constructor Create();

end;

implementation

{ TMigration }


constructor TMigration.Create();
begin
    FScripts := TScriptBuilder.Create;

    FTables     := TList<TDBTable>.Create;
    FViews      := TList<TDBView>.Create;
    FProcedures := TList<TDBProcedure>.Create;
    FFunctions  := TList<TDBFunction>.Create;
    FTriggers   := TList<TDBTrigger>.Create;
    FGenerators := TList<TDBGenerator>.Create;
    FIndices    := TList<TDBIndex>.Create;

end;


function TMigration.Table(aName : string): ITableBuilder;
begin
  result := TTableBuilder.Create
            .New(aName);
end;

end.
