unit TypeConverter.FloatToNumeric;

interface
uses TypeConverter.Interfaces, System.Classes, Model.DBField, system.SysUtils;


type TFloatToNumericConverter = class(TInterfacedObject, ITypeConverter)

private
   FTableName : string;
   FFieldName : string;
   FNewField  : TDBField;
   FOldField    : TDBField;

public
   function GenerateScript : string;

   constructor Create(aTableName : string; ANewField, AOldField : TDBField );

end;

implementation

uses Firebird.Types, Sql.Script.Builder;

{ TFloatToNumericConverter }

constructor TFloatToNumericConverter.Create(aTableName : string; ANewField, AOldField : TDBField);
begin
   FTableName := aTableName;
   FNewField  := ANewField;
   FOldField    := AOldField;
end;

function TFloatToNumericConverter.GenerateScript: string;
var
  newTypeMatch,oldTypeMatch : TFieldTypeMatch;
  script : IScriptBuilder;
  sql : string;
  newFieldNameTmp : string;
begin
 script := TScriptBuilder.Create;
 sql := '';
 newTypeMatch := MatchFirebirdType(FNewField.FieldType);
 oldTypeMatch := MatchFirebirdType(FOldField.FieldType);

 if (newTypeMatch.FieldType in [ftNumeric,ftDecimal]) and
    (oldTypeMatch.FieldType in [ftFloat,ftDoublePrecision] ) then begin

    newFieldNameTmp := FNewField.Name+'_TMP';

    sql := Format('ALTER TABLE %s ADD %s %s',[FTableName,newFieldNameTmp,FNewField.FieldType+FNewField.GetFieldSet])+';';
    script.AppendLine(sql);
    //Commit;
    sql := Format('UPDATE %s SET %s = %s',[FTableName,newFieldNameTmp,FNewField.GetFormatedName])+';';
    script.AppendLine(sql);

    sql := Format('ALTER TABLE %s DROP %s',[FTableName,FNewField.GetFormatedName])+';';
    script.AppendLine(sql);

    sql := Format('ALTER TABLE %s ALTER %s TO %s',[FTableName,newFieldNameTmp,FNewField.GetFormatedName])+';';
    script.AppendLine(sql);

 end;


 result := script.AsString;
end;

end.
