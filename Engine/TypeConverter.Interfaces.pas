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
     class function GetConverter(aTableName: string; aNewField, aOldField : TDBField) : ITypeConverter; static;

  end;

implementation

uses TypeConverter.Natural, typeConverter.FloatToNumeric, Firebird.Types;



{ TConverterFactory }

class function TConverterFactory.GetConverter(aTableName: string; aNewField, aOldField : TDBField): ITypeConverter;
var
 typeConversor : TTypeConversion;
begin
  Result := nil;

  typeConversor := ConversionTypeSupported(aNewField.FieldType, aOldField.FieldType);
  if (typeConversor = conversionDataTransfer) then
     Result := TFloatToNumericConverter.Create(aTableName,aNewField.Name,aNewField.FieldType, aOldField.FieldType )
  else begin
      Result := TNaturalTypeConverter.Create(aTableName,aNewField,aOldField);
  end;
end;

end.
