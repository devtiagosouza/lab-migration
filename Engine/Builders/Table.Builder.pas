unit Table.Builder;

interface

uses System.Classes, Model.DBField,Model.DBTable,Field.Builder, DCollections;


type ITableBuilder = interface
['{2702E277-BAC7-4BCD-B629-67E15543D9E2}']
   function New(const aTableName: string): ITableBuilder; overload;
   function SetTable(aTable : TDBTable) : ITableBuilder; overload;
   function Column(const aColumnName, aTypeAndDefs: string): ITableBuilder;
   function Build: TDBTable;

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

     function Build: TDBTable;

     destructor Destroy; override;
end;


implementation

 uses Parser.Fields;

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

end.
