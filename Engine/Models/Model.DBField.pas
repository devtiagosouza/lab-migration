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
    FFieldSet: string;

 public

    property TableName : string read FTableName write FTableName;
    property FieldType : string read FFieldType write FFieldType;
    property FieldSet : string read FFieldSet write FFieldSet;
    property NotNull : boolean read FNotNull write FNotNull;
    property DefaultValue : string read FDefaultValue write FDefaultValue;
    property Charset : string read FCharset write FCharset;
    property Collate : string read FCollate write FCollate;

    function GetFieldSet : string;

   function DDLCreate: string; override;

   function GetFullFieldSet(spacing : integer = 0) : string;

 end;

implementation

  uses Sql.Builder;

{ TDBField }


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

end.
