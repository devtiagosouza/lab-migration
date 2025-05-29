unit TypeConverter.Interfaces;

interface

uses  Model.DBField;

Type
  ITypeConverter = interface
  ['{EED13399-716F-4B84-ACBA-BCFA3FA91DF0}']

    function GenerateScript : string;

  end;

  type TConverterFactory = class

  public
     class function GetConverter(aTableName: string; aTypeFrom, aTypeTo : TDBField) : ITypeConverter; static;

  end;

implementation

uses TypeConverter.Natural, typeConverter.FloatToNumeric, Firebird.Types;



{ TConverterFactory }

class function TConverterFactory.GetConverter(aTableName: string; aTypeFrom, aTypeTo : TDBField): ITypeConverter;
var
 typeConversor : TTypeConversion;
begin
  Result := nil;
  typeConversor := ConversionTypeSupported(aTypeFrom.FieldType, aTypeTo.FieldType);
  if (typeConversor = conversionNatural) then
     Result := TNaturalTypeConverter.Create(aTableName,aTypeFrom,ATypeTo)
  else if (typeConversor = conversionDataTransfer) then
     Result := TFloatToNumericConverter.Create(aTableName,aTypeFrom.Name,aTypeFrom.FieldType, aTypeTo.FieldType );
end;

end.
