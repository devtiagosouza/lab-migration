unit TypeConverter.Natural;

interface
uses TypeConverter.Interfaces, System.Classes, Model.DBField, system.SysUtils;


type TNaturalTypeConverter = class(TInterfacedObject, ITypeConverter)

private
   FTableName : string;
   FTypeFrom  : TDBField;
   FTypeTo    : TDBField;

   function CanConvertComparingSize(sourceSize, targetSize : string) : boolean;
   function CanConvertComparingSizeAndScale(sourceSize, sourceScale, targetSize, targetScale : string) : boolean;
public
   function GenerateScript : string;

   constructor Create(aTableName : string; ATypeFrom, ATypeTo : TDBField );

end;

implementation

uses Firebird.Types, Sql.Script.Builder;

{ TFloatToNumericConverter }

function TNaturalTypeConverter.CanConvertComparingSize(sourceSize,
  targetSize: string): boolean;
var
  sourceSizeInt : Integer;
  targetSizeInt : Integer;
begin
  if (integer.TryParse(sourceSize.Trim,sourceSizeInt)) and (integer.TryParse(targetSize.Trim, targetSizeInt)) then begin
     result :=  targetSizeInt > sourceSizeInt;
  end;
end;

function TNaturalTypeConverter.CanConvertComparingSizeAndScale(
 sourceSize, sourceScale, targetSize, targetScale : string): boolean;
var
  sourceSizeInt,sourceScaleInt : Integer;
  targetSizeInt, targetScaleInt : Integer;
begin
  if integer.TryParse(sourceSize.Trim,sourceSizeInt) and integer.TryParse(sourceScale.Trim, sourceScaleInt) and
     integer.TryParse(targetSize.Trim,targetSizeInt) and integer.TryParse(targetScale.Trim, targetScaleInt)
  then begin

     result :=  targetSizeInt > sourceSizeInt;
  end;
end;

constructor TNaturalTypeConverter.Create(aTableName : string; ATypeFrom, ATypeTo : TDBField );
begin
   FTableName := aTableName;
   FTypeFrom  := aTypeFrom;
   FTypeTo    := aTypeTo;
end;

function TNaturalTypeConverter.GenerateScript: string;
var
patternFrom : string;
patternTo : string;
typeFromMatch,typeToMatch : TFieldTypeMatch;
script : TScriptBuilder;
sizeStr : string;
sql : string;
begin
   script := TScriptBuilder.Create;

   typeFromMatch := MatchFirebirdType(FTypeFrom.FieldType);
   typeToMatch := MatchFirebirdType(FTypeTo.FieldType);

   if ((typeFromMatch.FieldType = ftVarchar) and (typeToMatch.FieldType = ftVarchar)) or
      ((typeFromMatch.FieldType = ftChar) and (typeToMatch.FieldType = ftChar))
   then begin

      if CanConvertComparingSize(typeFromMatch.Match.Groups[2].Value,typeToMatch.Match.Groups[2].Value) then begin
          sql := string.format('ALTER DOMAIN %s TYPE %s',[FTypeFrom.DomainName+' /*'+FTypeTo.Name+'*/', FTypeTo.FieldType+FTypeTo.GetFieldSet])+';';
          script.
             AppendLine(sql).Append(' /*').Append('tabela: '+FTableName).Append('*/');
      end;

   end;



   result := script.AsString;
end;

end.

