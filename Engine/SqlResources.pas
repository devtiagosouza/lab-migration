unit SqlResources;

interface

 uses
  Classes, SysUtils,Windows,VCL.Forms,IdHashMessageDigest, IdGlobal,System.Zip;

Type TSqlResources = class

private


public
   class function Read(const aResourceName: string): string; static;
   class function SaveFile(ResourceName : string; Path : string; FileName : string): Boolean;
   class function SaveZipFile(ResourceName : string; Path : string; FileName : string) : Boolean;
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
  if not FileExists(FileName) then
    raise Exception.Create('Arquivo não encontrado: ' + FileName);

  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    MD5 := TIdHashMessageDigest5.Create;
    try
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
  ResInfo := FindResource(HInstance, PChar(ResName), ResType);
  if ResInfo = 0 then
    raise Exception.Create('Recurso não encontrado.');

  ResData := LockResource(LoadResource(HInstance, ResInfo));
  ResSize := SizeofResource(HInstance, ResInfo);

  Stream := TMemoryStream.Create;
  try
    Stream.Write(ResData^, ResSize);
    Stream.Position := 0;

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

class function TSqlResources.SaveZipFile(ResourceName, Path, FileName: string): Boolean;
var
  ext: string;
  fileZip: string;
  zip: TZipFile;
  Stream: TMemoryStream;
  TmpStream: TStream;
  Header: TZipHeader;
  index: Integer;
  arquivoCompactado : string;
begin
 ext := ExtractFileExt(FileName);
  fileZip := Copy(FileName, 1, FileName.LastIndexOf(ext)) + '.zip';
  SaveFile(ResourceName, Path, fileZip);

  try
    zip := TZipFile.Create;
    zip.Open(Path + '\' + fileZip, TZipMode.zmRead);
    index := 0;
    arquivoCompactado := zip.FileName[0];
    zip.Close;
    zip.Free;
    TZipFile.ExtractZipFile(Path + '\' + fileZip,Path );
    RenameFile(path+'\'+arquivoCompactado, path+'\'+FileName);

    DeleteFile(pchar(path+'\'+fileZip));

    Result := True;
  except
    on e: Exception do
      Result := False;
  end;
end;


end.
