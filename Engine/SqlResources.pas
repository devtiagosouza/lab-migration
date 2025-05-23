unit SqlResources;

interface

 uses
  Classes, SysUtils,Windows,IdHashMessageDigest, IdGlobal;

Type TSqlResources = class

private


public
   class function Read(const aResourceName: string): string; static;
   class function SaveFile(ResourceName : string; Path : string; FileName : string): Boolean;
   class function GetMD5FromResource(const ResName: string; const ResType: PChar): string;
   class function GetMD5FromFile(const FileName: string): string;
end;



implementation

{ TSqlResources }

class function TSqlResources.GetMD5FromFile(const FileName: string): string;
var
  FileStream: TFileStream;
  MD5: TIdHashMessageDigest5;
begin
  // Verifica se o arquivo existe
  if not FileExists(FileName) then
    raise Exception.Create('Arquivo não encontrado: ' + FileName);

  // Cria o stream para ler o arquivo
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    // Cria o objeto MD5
    MD5 := TIdHashMessageDigest5.Create;
    try
      // Calcula o hash MD5 do arquivo
      Result := MD5.HashStreamAsHex(FileStream);
    finally
      MD5.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

class function TSqlResources.GetMD5FromResource(const ResName: string;
  const ResType: PChar): string;
var
  ResInfo: THandle;
  ResData: Pointer;
  ResSize: DWORD;
  Stream: TMemoryStream;
  MD5: TIdHashMessageDigest5;
begin
  // Localiza o recurso binário pelo nome e tipo
  ResInfo := FindResource(HInstance, PChar(ResName), ResType);
  if ResInfo = 0 then
    raise Exception.Create('Recurso não encontrado.');

  // Carrega os dados do recurso
  ResData := LockResource(LoadResource(HInstance, ResInfo));
  ResSize := SizeofResource(HInstance, ResInfo);

  // Cria um stream na memória com os dados do recurso
  Stream := TMemoryStream.Create;
  try
    Stream.Write(ResData^, ResSize);
    Stream.Position := 0; // Reposiciona o stream para o início

    // Calcula o hash MD5
    MD5 := TIdHashMessageDigest5.Create;
    try
      Result := MD5.HashStreamAsHex(Stream);
    finally
      MD5.Free;
    end;
  finally
    Stream.Free;
  end;
end;

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
