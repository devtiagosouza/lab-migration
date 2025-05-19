unit Model.DBIndex;

interface
  uses Model.DBObject, System.SysUtils, System.StrUtils,System.Classes, sql.builder.SqlTemplate;

type TIndexType = (PRIMARY_KEY,FOREIGN_KEY,CHECK,UNIQUE,INDEX);


type TDBPrimaryKey = class(TDBObject)
  private
    FOnFields: string;
    FIndexName: string;
    FIndexSorting: string;
    FTableName: string;

public
   property TableName : string read FTableName write FTableName;
   property OnFields : string read FOnFields write FOnFields;
   property IndexName : string read FIndexName write FIndexName;
   property IndexSorting : string read FIndexSorting write FIndexSorting;

   function DDLCreate: string; override;

end;

type TDBForeignKey = class(TDBObject)
  private
    FOnFields: string;
    FFKTable: string;
    FFKField: string;
    FUpdateRule: string;
    FDeleteRule: string;
    FIndexName: string;
    FIndexSorting: string;
    FTableName: string;


public

    property TableName : string read FTableName write FTableName;
    property OnFields : string read FOnFields write FOnFields;
    property FKTable : string read FFKTable write FFKTable;
    property FKField : string read FFKField write FFKField;
    property UpdateRule : string read FUpdateRule write FUpdateRule;
    property DeleteRule : string read FDeleteRule write FDeleteRule;
    property IndexName : string read FIndexName write FIndexName;
    property IndexSorting : string read FIndexSorting write FIndexSorting;

    function DDLCreate: string; override;

end;


type TDBCheck = class(TDBObject)
  private
    FSource: string;
    FTableName: string;


public
    property TableName: string read FTableName write FTableName;
    property Source: string read FSource write FSource;

    function DDLCreate: string; override;
end;


type TDBUnique = class(TDBObject)
  private
    FOnFields: string;
    FIndexName: string;
    FIndexSorting: string;
    FTableName: string;


public
    property TableName : string read FTableName write FTableName;
    property OnFields : string read FOnFields write FOnFields;
    property IndexName : string read FIndexName write FIndexName;
    property IndexSorting: string read FIndexSorting write FIndexSorting;

    function DDLCreate: string; override;
end;





type TDBIndex = class(TDBObject)

public
  private
    FOnFields: string;
    FExpression: string;
    FUnique: boolean;
    FActive: boolean;
    FSorting: string;
    FForeignKey: string;
    FTableName: string;


public
    property TableName : string read FTableName write FTableName;
    property OnFields : string read FOnFields write FOnFields;
    property Expression: string read FExpression write FExpression;
    property Unique : boolean read FUnique write FUnique;
    property Active : boolean read FActive write FActive;
    property Sorting : string read FSorting write FSorting;


    function DDLCreate: string; override;

end;

implementation

{ TDBIndex }

function TDBIndex.DDLCreate: string;
var
 command : TStringList;
 sql : ISQLTemplate;
begin
  sql := TSQLTemplate.Create('CREATE {UNIQUE} {DESCENDING} INDEX :INDEX_NAME ON :TABLE_NAME :FIELDS;');

  sql.SetPar('UNIQUE',FUnique)
      .SetPar('DESCENDING',fSorting.ToUpper() = 'DESCENDING')
      .SetPar('INDEX_NAME', Name,true)
      .SetPar('TABLE_NAME', TableName,true)
      .SetPar('FIELDS',string.IsNullOrEmpty(FOnFields) = false,'('+FOnFields+')','COMPUTED BY '+FExpression);


      result := sql.AsString(';');

end;

{ TDBPrimaryKey }

function TDBPrimaryKey.DDLCreate: string;
const sintax = 'ALTER TABLE %s ADD CONSTRAINT %s PRIMARY KEY (%s)';
begin
   Result := Format(sintax,[ftableName,GetFormatedName(),FOnFields]);

   if (IndexName <> '') and (IndexName.ToUpper() <> Name.ToUpper()) then
      Result := Result+' USING '+IfThen(IndexSorting.ToUpper() = 'DESCENDING', IndexSorting.ToUpper(),'')+
      ' INDEX '+IndexName;

   result := Result +';';
end;

{ TDBUnique }

function TDBUnique.DDLCreate: string;
const sintax = 'ALTER TABLE %s ADD CONSTRAINT %s UNIQUE (%s)';
begin
   Result := Format(sintax,[ FTableName,Name,FOnFields]);
    if (IndexName.ToUpper() <> Name.ToUpper()) then
      Result := Result+' USING '+IfThen(IndexSorting.ToUpper() = 'DESCENDING', IndexSorting.ToUpper(),'')+
      ' INDEX '+IndexName;

end;

{ TDBForeignKey }

function TDBForeignKey.DDLCreate: string;
const sintax = 'ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s(%s)';
begin
   Result := Format(sintax,[ FTableName,Name,FOnFields, FKTable, FkField]);
   if (IndexName.ToUpper() <> Name.ToUpper()) then
      Result := Result+' USING '+IfThen(IndexSorting.ToUpper() = 'DESCENDING', IndexSorting.ToUpper(),'')+
      ' INDEX '+IndexName;
end;

{ TDBCheck }

function TDBCheck.DDLCreate: string;
const sintax = 'ALTER TABLE %s ADD CONSTRAINT %s %s';
begin
    Result := Format(sintax,[FTableName,GetFormatedName,FSource]);
end;

end.
