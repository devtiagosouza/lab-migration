unit SqlResources;

interface

 uses
  Classes, SysUtils,Windows;

Type TSqlResources = class

private


public
   class function Read(const aResourceName: string): string; static;
   class function SaveFile(ResourceName : string; Path : string; FileName : string): Boolean;
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

class function TSqlResources.SaveFile(ResourceName : string; Path: string; FileName : string): Boolean;
var
 Fs : TFileStream;
 pathSalvar : string;
 res : TResourceStream;
begin
 pathSalvar := Path+'\'+FileName;
 if (FileExists(pathSalvar)) then begin
     DeleteFile(Pchar(pathSalvar));
 end
 else begin
     ForceDirectories(Path);
 end;

 fs :=  TFileStream.Create(pathSalvar,fmCreate);

 res := TResourceStream.Create(HInstance,ResourceName,RT_RCDATA);
 res.SaveToStream(fs);
 fs.Free;

 result := FileExists(pathSalvar);
end;

end.
