unit TypeConverter.FloatToNumeric;

interface
uses TypeConverter.Interfaces, System.Classes;


type TFloatToNumericConverter = class(TInterfacedObject, ITypeConverter)

private
   FTableName : string;
   FFieldName : string;
   FTypeFrom  : string;
   FTypeTo    : string;

public
   function GenerateScript : string;

   constructor Create(aTableName, aFieldName, aTypeFrom, aTypeTo : string);

end;

implementation

{ TFloatToNumericConverter }

constructor TFloatToNumericConverter.Create(aTableName,aFieldName, aTypeFrom,
  aTypeTo: string);
begin
   FTableName := aTableName;
   FFieldName := aFieldName;
   FTypeFrom  := aTypeFrom;
   FTypeTo    := aTypeTo;
end;

function TFloatToNumericConverter.GenerateScript: string;

begin
 result := '';
end;

end.
