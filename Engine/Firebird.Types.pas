unit Firebird.Types;

interface

uses
  System.Generics.Collections, System.RegularExpressions;

  procedure InitializeFirebirdTypes;
  function IsValidFirebirdType(const FieldType: string): Boolean;
  function MatchFirebirdType(const Text: string): TMatch;

  implementation

  uses  System.SysUtils;

type
  TFirebirdTypePattern = record
    TypeName: string;
    RegexPattern: string;
  end;

var
  FirebirdTypes: TDictionary<string, string>;


procedure InitializeFirebirdTypes;
begin
  FirebirdTypes := TDictionary<string, string>.Create;

  FirebirdTypes.Add('BIGINT', '\bBIGINT\b');
  FirebirdTypes.Add('BLOB SUB_TYPE BINARY SEGMENT SIZE', '\bBLOB\s+SUB_TYPE\s+BINARY\s+SEGMENT\s+SIZE\s+\d+\b');
  FirebirdTypes.Add('BLOB SUB_TYPE TEXT SEGMENT SIZE', '\bBLOB\s+SUB_TYPE\s+TEXT\s+SEGMENT\s+SIZE\s+\d+\b');
  FirebirdTypes.Add('BOOLEAN', '\bBOOLEAN\b');
  FirebirdTypes.Add('CHAR', '\bCHAR\(\d+\)\b');
  FirebirdTypes.Add('CHARACTER', '\bCHARACTER\(\d+\)\b');
  FirebirdTypes.Add('DATE', '\bDATE\b');
  FirebirdTypes.Add('DECIMAL', '\bDECIMAL\(\d+,\d+\)\b');
  FirebirdTypes.Add('DOUBLE PRECISION', '\bDOUBLE\s+PRECISION\b');
  FirebirdTypes.Add('FLOAT', '\bFLOAT\b');
  FirebirdTypes.Add('INTEGER', '\bINTEGER\b');
  FirebirdTypes.Add('INT', '\bINT\b');
  FirebirdTypes.Add('NUMERIC', '\bNUMERIC\(\d+,\d+\)\b');
  FirebirdTypes.Add('SMALLINT', '\bSMALLINT\b');
  FirebirdTypes.Add('TIME', '\bTIME\b');
  FirebirdTypes.Add('TIMESTAMP', '\bTIMESTAMP\b');
  FirebirdTypes.Add('VARCHAR', '\bVARCHAR\(\d+\)');
end;

{ Retorna o primeiro match válido da lista de regex do dicionário Firebird }
function MatchFirebirdType(const Text: string): TMatch;
var
  Pair: TPair<string, string>;
  Regex: TRegEx;
  M: TMatch;
begin
  Result := Default(TMatch);
  for Pair in FirebirdTypes do
  begin
    Regex := TRegEx.Create(Pair.Value, [roIgnoreCase]);
    M := Regex.Match(Text);
    if M.Success then
    begin
      Result := M;
      Exit;
    end;
  end;
end;

function IsValidFirebirdType(const FieldType: string): Boolean;
var
  Pair: TPair<string, string>;
  Regex: TRegEx;
begin
  Result := False;
  for Pair in FirebirdTypes do
  begin
    Regex := TRegEx.Create(Pair.Value, [roIgnoreCase]);
    if Regex.IsMatch(FieldType) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;


initialization
  InitializeFirebirdTypes;

finalization
  FirebirdTypes.Free;

End.

