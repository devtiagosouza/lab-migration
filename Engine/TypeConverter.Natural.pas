unit TypeConverter.Natural;

interface
uses TypeConverter.Interfaces, System.Classes, Model.DBField, system.SysUtils;


type TNaturalTypeConverter = class(TInterfacedObject, ITypeConverter)


private
   FTableName : string;
   FNewField  : TDBField;
   FOldField    : TDBField;

   function CanConvertComparingSize(aNewSize, aOldSize : string) : boolean;
   function CanConvertComparingSizeAndScale(aNewSize, aNewScale, aOldSize, aOldScale : string) : boolean;
public
   function GenerateScript : string;

   constructor Create(aTableName : string; ANewField, AOldField : TDBField );

end;

implementation

uses Firebird.Types, Sql.Script.Builder;

{ TFloatToNumericConverter }

function TNaturalTypeConverter.CanConvertComparingSize(aNewSize,
  aOldSize: string): boolean;
var
  newSizeInt : Integer;
  oldSizeInt : Integer;
begin
  if (integer.TryParse(aNewSize.Trim,newSizeInt)) and (integer.TryParse(aOldSize.Trim, oldSizeInt)) then begin
     result :=  newSizeInt > oldSizeInt;
  end;
end;

function TNaturalTypeConverter.CanConvertComparingSizeAndScale(
 aNewSize, aNewScale, aOldSize, aOldScale : string): boolean;
var
  newSizeInt,newScaleInt : Integer;
  oldSizeInt, oldScaleInt : Integer;
begin
  if integer.TryParse(aNewSize.Trim,newSizeInt) and integer.TryParse(aNewScale.Trim, newScaleInt) and
     integer.TryParse(aOldSize.Trim,oldSizeInt) and integer.TryParse(aOldScale.Trim, oldScaleInt)
  then begin
     result :=  (newSizeInt > oldSizeInt) or (newScaleInt > oldScaleInt);
  end;
end;

constructor TNaturalTypeConverter.Create(aTableName : string; ANewField, AOldField : TDBField );
begin
   FTableName := aTableName;
   FNewField  := ANewField;
   FOldField    := AOldField;
end;

function TNaturalTypeConverter.GenerateScript: string;

                             procedure AppendSql(aScript: IScriptBuilder);
                             var alterSql : string;
                             begin
                                alterSql := string.format('ALTER DOMAIN %s TYPE %s',[FOldField.DomainName, FNewField.FieldType+FNewField.GetFieldSet])+';';
                                alterSql := alterSql+' /*'+FOldField.Name+' da tabela '+FTableName+' */';
                                aScript.AppendLine(alterSql);
                             end;

var
newTypeMatch,oldTypeMatch : TFieldTypeMatch;
script : IScriptBuilder;
sql : string;
newCharset : string;
begin
   script := TScriptBuilder.Create;
   sql := '';

   if (FNewField.FieldType <> FOldField.FieldType) then begin
       newTypeMatch := MatchFirebirdType(FNewField.FieldType);
       oldTypeMatch := MatchFirebirdType(FOldField.FieldType);

       if ((newTypeMatch.FieldType = ftVarchar) and (oldTypeMatch.FieldType = ftVarchar)) or
          ((newTypeMatch.FieldType = ftChar) and (oldTypeMatch.FieldType = ftChar))
       then begin

          if CanConvertComparingSize(newTypeMatch.Match.Groups[2].Value,oldTypeMatch.Match.Groups[2].Value) then begin
              AppendSql(script);
          end;

       end
       else if ((newTypeMatch.FieldType = ftNumeric) and (oldTypeMatch.FieldType = ftNumeric)) or
          ((newTypeMatch.FieldType = ftDecimal) and (oldTypeMatch.FieldType = ftDecimal))
       then begin

          if CanConvertComparingSizeAndScale(newTypeMatch.Match.Groups[2].Value,newTypeMatch.Match.Groups[3].Value,
              oldTypeMatch.Match.Groups[2].Value,oldTypeMatch.Match.Groups[3].Value ) then begin
              AppendSql(script);
          end;

       end
       else begin

          AppendSql(script);
       end;

   end;

   if (FNewField.NotNull) AND (FOldField.NotNull = false) then begin
     sql := string.format('ALTER TABLE %s ALTER %s SET NOT NULL',[FtableName,FNewField.GetFormatedName])+';';
     script.AppendLine(sql);
   end
   else if (FNewField.NotNull = false) and (FOldField.NotNull = true) then begin
     sql := string.format('ALTER TABLE %s ALTER %s DROP NOT NULL',[FtableName,FNewField.GetFormatedName])+';';
     script.AppendLine(sql);
   end;

   if (FNewField.DefaultValue.Trim <> FOldField.DefaultValue.Trim) then begin
     sql := string.format('ALTER TABLE %s ALTER COLUMN %s SET DEFAULT %s',[FtableName,FNewField.GetFormatedName, FNewField.DefaultValue])+';';

     if ( FNewField.DefaultValue.Trim = '') and (FOldField.DefaultValue.Trim <> '') then begin
        sql := string.format('ALTER TABLE %s ALTER COLUMN % DROP DEFAULT',[FtableName,FNewField.GetFormatedName])+';';
     end;

     script.AppendLine(sql);

   end;

   if (FNewField.Charset <> FOldField.Charset) then begin
       newCharset := FNewField.Charset.Trim;
       if (newCharset = '') then
           newCharset := 'NONE';

      sql := string.format('ALTER DOMAIN %s TYPE %s CHARACTER SET %s',[FOldField.DomainName, FNewField.FieldType, newCharset])+';';
      script.AppendLine(sql);
   end;

   result := script.AsString;
end;

end.

