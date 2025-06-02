unit Table.Builder;

interface

uses System.Classes, Model.DBField,Model.DBTable,Field.Builder, DCollections;


type ITableBuilder = interface
['{2702E277-BAC7-4BCD-B629-67E15543D9E2}']
   function New(const aTableName: string): ITableBuilder;
   function Column(const aColumnName, aTypeAndDefs: string): ITableBuilder;
   function Build: TDBTable;

end;

type TTableBuilder = class(TInterfacedObject,ITableBuilder)

private
  FTable : TDBTable;

public
     function New(const aTableName: string): ITableBuilder;
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
begin
 field := TFieldParser.ParseField(aTypeAndDefs);
 field.Name := aColumnName;
 field.TableName := FTable.Name;

 FTable.Fields.Add(Field);
  Result := Self;
end;

destructor TTableBuilder.Destroy;
begin
  FTable.Free;
  inherited;
end;

function TTableBuilder.New(const aTableName: string): ITableBuilder;
begin
  FTable := TDBTable.Create;
  FTable.Name := aTableName;
  Result := Self;
end;

end.
