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
  sourceSizeInt : Integer;
  targetSizeInt : Integer;
begin
  if (integer.TryParse(aNewSize.Trim,sourceSizeInt)) and (integer.TryParse(aOldSize.Trim, targetSizeInt)) then begin
     result :=  targetSizeInt > sourceSizeInt;
  end;
end;

function TNaturalTypeConverter.CanConvertComparingSizeAndScale(
 aNewSize, aNewScale, aOldSize, aOldScale : string): boolean;
var
  sourceSizeInt,sourceScaleInt : Integer;
  targetSizeInt, targetScaleInt : Integer;
begin
  if integer.TryParse(aNewSize.Trim,sourceSizeInt) and integer.TryParse(aNewScale.Trim, sourceScaleInt) and
     integer.TryParse(aOldSize.Trim,targetSizeInt) and integer.TryParse(aOldScale.Trim, targetScaleInt)
  then begin
     result :=  (targetSizeInt > sourceSizeInt) or (targetScaleInt > sourceScaleInt);
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
patternFrom : string;
patternTo : string;
newTypeMatch,oldTypeMatch : TFieldTypeMatch;
script : IScriptBuilder;
sizeStr : string;
sql : string;
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

   end
   else begin
       if (FNewField.NotNull = false) and (FOldField.NotNull = true) then begin
           sql := string.format('ALTER DOMAIN %s DROP NOT NULL',[FOldField.DomainName])+';';
           script.AppendLine(sql);
       end
       ELSE BEGIN


           if (FNewField.NotNull <> FOldField.NotNull) or
              (FNewField.DefaultValue <> FOldField.DefaultValue) or
              (FNewField.Charset <> FOldField.Charset) or
              (FNewField.Collate <> FOldField.Collate)
           then begin
               sql := string.format('ALTER DOMAIN %s TYPE %s',[FOldField.DomainName, FNewField.FieldType+FNewField.GetFieldSet])+';';
               script.AppendLine(sql);
           end;

       END;

   end;



//   if (sql <> '') then
//     script.AppendLine(sql)
//         .Append(' /*').Append('tabela: '+FTableName).Append('*/');

   result := script.AsString;
end;

end.

