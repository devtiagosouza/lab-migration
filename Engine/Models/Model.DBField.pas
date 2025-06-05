unit Model.DBField;

interface
  uses Model.DBObject,System.Classes, system.SysUtils,System.StrUtils, DCollections;

 type TDBField = class(TDBObject)



 private
    FFieldType: string;
    FDefaultValue: string;
    FNotNull: boolean;
    FCharset: string;
    FCollate: string;
    FTableName: string;
    FDomainName: string;
    FIsPk: boolean;
    procedure SetDefaultValue(const Value: string);

 public
    property DomainName : string read FDomainName write FDomainName;
    property TableName : string read FTableName;
    property FieldType : string read FFieldType write FFieldType;
    property NotNull : boolean read FNotNull write FNotNull;
    property DefaultValue : string read FDefaultValue write SetDefaultValue;
    property Charset : string read FCharset write FCharset;
    property Collate : string read FCollate write FCollate;
    property IsPK : boolean read FIsPk write FIsPk;

    function GetFieldSet : string;

    function DDLCreate: string; override;
    function DDLAlter: string; override;

    function EqualityScript(Obj: TDBObject; args : array of TObject) : string; override;


    function GetFullFieldSet(spacing : integer = 0) : string;

    constructor Create(AName, ATableName : string);

 end;





implementation

  uses Sql.Builder,TypeConverter.Interfaces;

{ TDBField }


constructor TDBField.Create(AName, ATableName : string);
begin
  inherited Create(AName);
  FTableName := ATableName;
  ObjectTypeFriendlyName := 'Campo';
end;

function TDBField.DDLAlter: string;
begin
   result := TSQLBuilder.Create()
   .Append('ALTER TABLE :TABLE_NAME ALTER COLUMN :NAME :TYPE')
     .Append(GetFieldSet)
    .AsTemplate
     .SetPar('TABLE_NAME', TableName, True)
     .SetPar('NAME',Name,true)
      .SetPar('TYPE',FieldType)
   .asString(';')
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



function TDBField.EqualityScript(Obj: TDBObject; args : array of TObject): string;
var
  Outro: TDBField;
  converter : ITypeConverter;
begin
 result := '';
 if (isSameObject(Obj)) then begin
     Outro := TDBField(Obj);


     if (FTableName = outro.TableName) then begin

        if (FFieldType <> outro.FieldType) or
           (FNotNull <> outro.NotNull) or
           (FDefaultValue <> outro.DefaultValue) or
           (FCharset <> outro.Charset) or
           (FCollate <> Outro.Collate)
        then begin
           converter := TConverterFactory.GetConverter(fTableName,self, outro);
           if (converter <> nil) then begin
                Result := converter.GenerateScript;
           end
        end;

     end;
 end;


end;

function TDBField.GetFieldSet: string;
var
 parts : TStringList;
begin
 try
   parts := TStringList.Create;
   try

     if (string.isnullorempty(Charset) = FALSE) AND (Charset <> 'NONE') then
          parts.Add(ifthen(DefaultValue.Trim.StartsWith('CHARACTER SET') = false,'CHARACTER SET ','' ) + Charset);

     if (string.isnullorempty(DefaultValue) = false) then
       parts.Add(ifthen(DefaultValue.Trim.StartsWith('DEFAULT') = false,'DEFAULT ','' ) + DefaultValue);

     if (NotNull) then
         parts.Add('NOT NULL');


      if (string.isnullorempty(Collate) = FALSE) AND (Collate <> 'NONE') then
            parts.Add(ifthen(DefaultValue.Trim.StartsWith('COLLATE') = false,'COLLATE ','' ) + Collate);


      if (parts.Count > 0) then begin
         Result := ' '+String.Join(' ', Parts.ToStringArray);
      end
      else result := '';

   except on e: exception do begin
       raise e
   end;
   end;

 finally
    parts.Free;
 end;



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


procedure TDBField.SetDefaultValue(const Value: string);
begin
  FDefaultValue := Value.Trim;

  if (FDefaultValue.Trim.StartsWith('DEFAULT')) then begin
     FDefaultValue := Copy(FDefaultValue.Trim,('DEFAULT').Length+1).Trim;
  end;


end;

end.
