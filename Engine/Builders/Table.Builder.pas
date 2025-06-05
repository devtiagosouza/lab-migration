unit Table.Builder;

interface

uses System.Classes, Model.DBField,Model.DBTable, Model.DBTrigger, DCollections;


type ITableBuilder = interface
['{2702E277-BAC7-4BCD-B629-67E15543D9E2}']
   function New(const aTableName: string): ITableBuilder; overload;
   function SetTable(aTable : TDBTable) : ITableBuilder; overload;
   function Column(const aColumnName, aTypeAndDefs: string): ITableBuilder;
   function Trigger(const aTriggerName: string) : ITableBuilder;

   function Build: TDBTable;

   function GetTableName : string;

end;

type TTableBuilder = class(TInterfacedObject,ITableBuilder)

strict private
    constructor Create;

private
  FTable : TDBTable;

public

     function New(const aTableName: string): ITableBuilder;
     function SetTable(aTable : TDBTable) : ITableBuilder;
     function Column(const aColumnName, aTypeAndDefs: string): ITableBuilder;

     function Trigger(const aTriggerDDLCommand: string) : ITableBuilder;

     function Build: TDBTable;

     function GetTableName : string;

     destructor Destroy; override;
end;


implementation

 uses Parser.Fields, Parser.Triggers;

{ TTableBuilder }

function TTableBuilder.Build: TDBTable;
begin
  Result := FTable;
  FTable := nil;
end;

function TTableBuilder.Column(const aColumnName,
  aTypeAndDefs: string): ITableBuilder;
  var
 field : TDBField;
 index : integer;
begin
 index := FTable.Fields.IndexOf(function(f : TDBField) : boolean
 begin
    result := (f.Name = aColumnName);
 end);

 field := TFieldParser.ParseFieldFromDefinition(aColumnName,FTable.Name, aTypeAndDefs);
 if (field <> nil) then begin
   if (index < 0) then begin
      FTable.Fields.Add(Field);
   end
   else begin
      FTable.Fields[index] := field;
   end;
 end;


 Result := Self;
end;


constructor TTableBuilder.Create;
begin
  inherited;
end;

destructor TTableBuilder.Destroy;
begin
  FTable.Free;
  inherited;
end;

function TTableBuilder.GetTableName: string;
begin
  Result := FTable.Name;
end;

function TTableBuilder.New(const aTableName: string): ITableBuilder;
begin
  FTable := TDBTable.Create(aTableName);
  Result := Self;
end;

function TTableBuilder.SetTable(aTable: TDBTable): ITableBuilder;
begin
  FTable := aTable;
  Result := Self;
end;

function TTableBuilder.Trigger(const aTriggerDDLCommand: string): ITableBuilder;
var
 trg : TDBTrigger;
 index : integer;
 parser : TTriggerParser;
begin
   parser := TTriggerParser.Create;

   trg := parser.Parse(aTriggerDDLCommand);
   if (trg <> nil) then begin
       index := FTable.Triggers.IndexOf(function(t : TDBTrigger) : boolean
       begin
          result := (t.Name = trg.Name);
       end);

         if (index < 0) then begin
            FTable.Triggers.Add(trg);
         end
         else begin
            FTable.Triggers[index] := trg;
         end;
   end;

  Result := Self;

end;

end.
