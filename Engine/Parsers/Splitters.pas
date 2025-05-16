unit Splitters;

interface

uses
  System.SysUtils, System.Classes, DCollections,  System.RegularExpressions, System.StrUtils, ClipBrd;

type
  TDDLCommandType = (ddlUnknown, ddlCreate, ddlAlter, ddlDrop);
  TDBObjectType = (objUnknown, objTable, objView, objProcedure, objFunction, objTrigger, objPrimaryKey,
                   objIndex, objGenerator);

  TDDLCommand = class
  public
    CommandText: string;
    CommandType: TDDLCommandType;
    ObjectType: TDBObjectType;
    ObjectName: string;
  end;

  TDelimitersParts = record
     SqlPart : string;
     Delimiter : char;
  end;

  TCommandSplitter = class
  private
    function DetectCommandType(const SQL: string): TDDLCommandType;
    function DetectObjectType(const SQL: string): TDBObjectType;
    function ExtractObjectName(const SQL: string): string;
    function RemoveComments(const SQL: string): string;
    function SplitSQL(const DDLText: string): TArray<string>;
    function SplitDelimitersParts(const DDLText: string) : TArray<TDelimitersParts>;

  public
    constructor Create();
    function Split(const DDLText: string): TList<TDDLCommand>;

  end;

implementation

{ TCommandSplitter }

constructor TCommandSplitter.Create;
begin
end;

function TCommandSplitter.DetectCommandType(const SQL: string): TDDLCommandType;
var
  Regex: TRegEx;
begin
  if TRegEx.IsMatch(SQL, '^\s*CREATE', [roIgnoreCase]) then
    Result := ddlCreate
  else if TRegEx.IsMatch(SQL, '^\s*ALTER', [roIgnoreCase]) then
    Result := ddlAlter
  else if TRegEx.IsMatch(SQL, '^\s*DROP', [roIgnoreCase]) then
    Result := ddlDrop
  else
    Result := ddlUnknown;
end;


function TCommandSplitter.DetectObjectType(const SQL: string): TDBObjectType;
var
  Regex: TRegEx;
begin
  if TRegEx.IsMatch(SQL, '^\s*CREATE\s+OR\s+ALTER\s+TRIGGER', [roIgnoreCase]) then
    Result := objTrigger
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+OR\s+ALTER\s+PROCEDURE', [roIgnoreCase]) then
    Result := objProcedure
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+OR\s+ALTER\s+FUNCTION', [roIgnoreCase]) then
    Result := objFunction
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+OR\s+ALTER\s+VIEW', [roIgnoreCase]) then
    Result := objView


  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+TABLE', [roIgnoreCase]) then
    Result := objTable
   else if TRegEx.IsMatch(SQL, '^\s*ALTER\s+TABLE\s+(\w+)\s+ADD\s+CONSTRAINT\s+(\w+)\s+PRIMARY\s+KEY\s*\(([^)]+)\)', [roIgnoreCase]) then
    result := objPrimaryKey
   else if TRegEx.IsMatch(SQL, '^\s*ALTER\s+TABLE', [roIgnoreCase]) then
    Result := objTable
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+INDEX', [roIgnoreCase]) then
    Result := objIndex
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+GENERATOR', [roIgnoreCase]) then
    Result := objGenerator
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+PRIMARY\s+KEY', [roIgnoreCase]) then
    Result := objPrimaryKey
  else
    Result := objUnknown;
end;




function TCommandSplitter.SplitDelimitersParts(const DDLText: string): TArray<TDelimitersParts>;
var
  DelimiterPart : TDelimitersParts;
  Commands: TArray<string>;
  part : string;
  list : TList<TDelimitersParts>;
begin

   list := TList<TDelimitersParts>.Create;

   Commands := DDLText.Split(['SET TERM'], TStringSplitOptions.ExcludeEmpty);
   for part in Commands do begin
      if (part.Trim.Replace('^ ;','').Replace('; ^','').Trim <> '') then begin
         if TRegEx.IsMatch(part, '^\s*\^\s;', [roIgnoreCase]) then
         begin
             DelimiterPart.SqlPart := part.TrimLeft.Replace('^ ;', '').Replace('; ^', '').Trim;
             DelimiterPart.Delimiter := '^';
             list.Add(DelimiterPart);
         end
         else begin
             DelimiterPart.SqlPart := part.TrimLeft.Replace('; ^','');
             DelimiterPart.Delimiter := ';';
             list.Add(DelimiterPart);
         end;
      end;

   end;

   Result := list.ToArray;

end;

function TCommandSplitter.ExtractObjectName(const SQL: string): string;
var
  Regex: TRegEx;
  Match: TMatch;
begin
  Match := TRegEx.Match(SQL, '\s*(TABLE|VIEW|PROCEDURE|FUNCTION|TRIGGER|INDEX|GENERATOR)\s+(\w+)', [roIgnoreCase]);
  if Match.Success then
    Result := Match.Groups[2].Value
  else
    Result := '';
end;



function TCommandSplitter.RemoveComments(const SQL: string): string;
var
  Regex: TRegEx;
begin
  // Remove comentários de bloco (/* ... */) incluindo multilinhas
  Regex := TRegEx.Create('/\*[\s\S]*?\*/', [roIgnoreCase]);
  Result := Regex.Replace(SQL, '');

  // Remove comentários de linha (-- ...), preservando as quebras de linha
 // Regex := TRegEx.Create('--.*$', [roIgnoreCase, roMultiline]);
 // Result := Regex.Replace(Result, '');

  // Não substitua quebras de linha por espaços, preserve o texto original
  Result := Result.Trim;
end;

function TCommandSplitter.SplitSQL(const DDLText: string): TArray<string>;
var
  Statements: TList<string>;
  CleanDDL: string;
  PosStartTerm, PosEndTerm: Integer;
  PartBefore, PartBetween, PartAfter: string;
  SplitPart: TArray<string>;
  Cmd: string;
  Part : TDelimitersParts;

  procedure AddCommandsFrom(const Text: string; Delimiter: Char);
  var
    Commands: TArray<string>;
    C, TrimmedCmd: string;
  begin
    Commands := Text.Split([Delimiter], TStringSplitOptions.ExcludeEmpty);
    for C in Commands do
    begin
      TrimmedCmd := C.Trim;
      if TrimmedCmd <> '' then
        Statements.Add(TrimmedCmd);
    end;
  end;

begin
    Statements := TList<string>.Create;
    CleanDDL := RemoveComments(DDLText)
                .Replace('SET SQL DIALECT 3;','',[rfReplaceAll, rfIgnoreCase]);


  for Part in SplitDelimitersParts(CleanDDL) do begin
      AddCommandsFrom(Part.SqlPart,Part.Delimiter);
  end;

  Result := Statements.ToArray;
  Statements.Free;
end;



function TCommandSplitter.Split(const DDLText: string): TList<TDDLCommand>;
var
  ResultList: TList<TDDLCommand>;
  Statements: TArray<string>;
  SQL, CleanSQL: string;
  Command: TDDLCommand;
begin
  ResultList := TList<TDDLCommand>.Create;

  Statements := SplitSQL(DDLText);

  for SQL in Statements do
  begin
    CleanSQL := SQL.Trim;
    if CleanSQL = '' then
      Continue;

    if TRegEx.IsMatch(CleanSQL, '^\s*GRANT\b', [roIgnoreCase]) then
      Continue;

    Command := TDDLCommand.Create;
    Command.CommandText := CleanSQL;
    Command.CommandType := DetectCommandType(CleanSQL);
    Command.ObjectType := DetectObjectType(CleanSQL);
    Command.ObjectName := ExtractObjectName(CleanSQL);

    ResultList.Add(Command);
  end;

  Result := ResultList;
end;

end.

