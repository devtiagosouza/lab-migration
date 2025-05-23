unit Model.DBField;

interface
  uses Model.DBObject, system.SysUtils;

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

    function EqualityScript(Obj: TDBObject) : string; override;


    function GetFullFieldSet(spacing : integer = 0) : string;

   constructor Create();

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



function TDBField.EqualityScript(Obj: TDBObject): string;
var
  Outro: TDBField;
begin
 result := '';
 if (isSameObject(Obj)) then begin
     Outro := TDBField(Obj);

     if (FTableName = outro.TableName) then begin

        if (FFieldType <> Outro.FFieldType) or
               (FDefaultValue <> Outro.FDefaultValue) or
               (FNotNull <> Outro.FNotNull) or
               (FCharset <> Outro.FCharset) or
               (FCollate <> Outro.FCollate) then
             result :=  DDLCreate;
     end;
 end;


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
