unit SqlResources;

interface

 uses
  Classes, SysUtils,Windows;

Type TSqlResources = class

private


public
   class function Read(const aResourceName: string): string; static;

end;



implementation

{ TSqlResources }

class function TSqlResources.Read(const aResourceName: string): string;
var
  Stream: TResourceStream;
  StringStream: TStringStream;
begin
  Stream := TResourceStream.Create(HInstance, aResourceName, RT_RCDATA);
  try
    StringStream := TStringStream.Create('', TEncoding.UTF8); // ou outra codificação
    try
      StringStream.LoadFromStream(Stream);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    Stream.Free;
  end;
end;

end.
