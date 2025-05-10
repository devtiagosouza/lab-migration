unit DCollections.Attributes;

interface

 type
  DisplayName = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue;
  end;

  type
  Hide = class(TCustomAttribute)

  end;

implementation

{ DisplayName }

constructor DisplayName.Create(const AValue: string);
begin
 FValue := AValue;
end;

end.
