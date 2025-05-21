unit Model.DBField;

interface
  uses Model.DBObject, system.SysUtils, system.strUtils;

 type TDBField = class(TDBObject)

 private
    FFieldType: string;
    FDefaultValue: string;
    FNotNull: boolean;
    FCharset: string;
    FCollate: string;
    FTableName: string;

 public

    property TableName : string read FTableName write FTableName;
    property FieldType : string read FFieldType write FFieldType;
    property NotNull : boolean read FNotNull write FNotNull;
    property DefaultValue : string read FDefaultValue write FDefaultValue;
    property Charset : string read FCharset write FCharset;
    property Collate : string read FCollate write FCollate;

    function GetFieldSet : string;

    function DDLCreate: string; override;

   function GetFullFieldSet(spacing : integer = 0) : string;

   constructor Create();

 end;


 TDBFieldBuilder = class
 private
    FModel : TDBField;
    FTableName : string;
 public
    function New(aFieldName: string; aFieldType : string) : TDBFieldBuilder;
    function AsField : TDBField;

    constructor Create(aTableName : string);
 end;

implementation

  uses Sql.Builder;

{ TDBField }


constructor TDBField.Create();
begin
  inherited Create;
  ObjectTypeFriendlyName := 'Campo';
end;

function TDBField.DDLCreate: string;
begin
   result := TSQLBuilder.Create()
   .Append('ALTER TABLE :TABLE_NAME ADD :NAME :TYPE')
     .Append(GetFieldSet)
    .AsTemplate
     .SetPar('TABLE_NAME', TableName, True)
     .SetPar('NAME',Name,true)
      .SetPar('TYPE',FieldType)
   .asString(';')
end;

function TDBField.GetFieldSet: string;
begin
  if (string.isnullorempty(DefaultValue) = false) then
        Result := Result +' '+DefaultValue;

   if (NotNull) then
        Result := Result +' NOT NULL';

   if (string.isnullorempty(Charset) = FALSE) AND (Charset <> 'NONE') then
         Result := Result +' '+Charset;

  if (string.isnullorempty(Collate) = FALSE) AND (Collate <> 'NONE') then
         Result := Result +' '+Collate;

         if (string.isNullOrEmpty(Result.Trim()) = false) then
             Result := ' '+Result.Trim()
         else Result := '';

end;

function TDBField.GetFullFieldSet(spacing : integer = 0): string;
var
 vName : string;
begin
   if (spacing > 0) then
      vName :=   GetFormatedName.PadRight(spacing,' ')
   else vName := GetFormatedName;

   Result:= Trim(vName+' '+FieldType+GetFieldSet);
end;

{ TDBFieldBuilder }

function TDBFieldBuilder.AsField: TDBField;
begin
  Result := FModel;
end;

constructor TDBFieldBuilder.Create(aTableName : string);
begin
  FTableName := aTableName;
end;

function TDBFieldBuilder.New(aFieldName,
  aFieldType: string): TDBFieldBuilder;
begin
    FModel := TDBField.Create;
    FModel.TableName := FTableName;
    FModel.Name := aFieldName;
    FModel.FieldType := aFieldType;
    Result := Self;
end;

end.
