unit Table.Builder;

interface

uses System.Classes, Model.DBIndex, Model.DBField,Model.DBTable, Model.DBTrigger, DCollections;


type IColumnBuilder = interface
['{5A27CDB5-C933-4607-8844-311F225F02DB}']


end;



type ITableBuilder = interface
['{2702E277-BAC7-4BCD-B629-67E15543D9E2}']
   function New(const aTableName: string): ITableBuilder; overload;
   function SetTable(aTable : TDBTable) : ITableBuilder; overload;
   function Column(const aColumnName, TypeDefinition: string): ITableBuilder;
   function ColumnPK(const aColumnName, TypeDefinition: string; constraintName : string = ''): ITableBuilder;

   function Trigger(const aTriggerName: string) : ITableBuilder;

   function Build: TDBTable;

   function GetTableName : string;

end;

type TTableBuilder = class(TInterfacedObject,ITableBuilder)

strict private
    constructor Create;

private
  FTable : TDBTable;
  FPrimaryKeys : TList<TDBPrimaryKey>;

public

     function New(const aTableName: string): ITableBuilder;
     function SetTable(aTable : TDBTable) : ITableBuilder;
     function Column(const aColumnName, TypeDefinition: string): ITableBuilder;
     function ColumnPK(const aColumnName, TypeDefinition: string; constraintName : string = ''): ITableBuilder;


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

function TTableBuilder.Column(const aColumnName, TypeDefinition: string): ITableBuilder;
  var
 field : TDBField;
 index : integer;
begin
 index := FTable.Fields.IndexOf(function(f : TDBField) : boolean
 begin
    result := (f.Name = aColumnName);
 end);

 field := TFieldParser.ParseFieldFromDefinition(aColumnName,FTable.Name, TypeDefinition);
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


function TTableBuilder.ColumnPK(const aColumnName, TypeDefinition: string;
  constraintName: string): ITableBuilder;
  var
   builder : ITableBuilder;
   field : TDBField;
   idx : integer;
   pk : TDBPrimaryKey;
begin
 builder := Column(aColumnName, TypeDefinition);

 idx := FTable.Fields.IndexOf(function(f : TDBField) : boolean
 begin
    result := (f.Name = aColumnName);
 end);

 if (idx > -1) then begin
     FTable.Fields[idx].IsPK := true;

     FTable.Fields.Where(function(p : TDBField) : boolean
     begin
        result := p.IsPK = true;
     end);



     FTable.PrimaryKeys.Clear;
     pk := TDBPrimaryKey.Create(constraintName);
     pk.TableName := FTable.Name;
     pk.OnFields := FTable.Fields[idx].Name;

     FTable.PrimaryKeys.Add(pk);
 end;




end;

constructor TTableBuilder.Create;
begin
  inherited;
  FPrimaryKeys := TList<TDBPrimaryKey>.Create;
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
