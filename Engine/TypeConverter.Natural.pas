unit TypeConverter.Natural;

interface
uses TypeConverter.Interfaces, System.Classes, Model.DBField;


type TNaturalTypeConverter = class(TInterfacedObject, ITypeConverter)

private
   FTableName : string;
   FTypeFrom  : TDBField;
   FTypeTo    : TDBField;

public
   function GenerateScript : string;

   constructor Create(aTableName : string; ATypeFrom, ATypeTo : TDBField );

end;

implementation

uses Firebird.Types, Sql.Script.Builder;

{ TFloatToNumericConverter }

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
begin
   script := TScriptBuilder.Create;

   typeFromMatch := MatchFirebirdType(FTypeFrom.FieldType);
   typeToMatch := MatchFirebirdType(FTypeTo.FieldType);

   if (typeFromMatch.FieldType = ftVarchar) and (typeToMatch.FieldType = ftVarchar)  then begin
      //verifica tamanho
      typeFromMatch.Match.Groups[2]

   end;





   script.AsString;
end;

end.

