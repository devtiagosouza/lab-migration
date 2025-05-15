unit Parser;

interface

uses
  System.SysUtils, System.Classes, DCollections,  System.RegularExpressions, System.StrUtils, ClipBrd;

type
  TDDLCommandType = (ddlUnknown, ddlCreate, ddlAlter, ddlDrop);
  TDBObjectType = (objUnknown, objTable, objView, objProcedure, objFunction, objTrigger, objIndex, objGenerator);

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

  TParser = class
  private
    function DetectCommandType(const SQL: string): TDDLCommandType;
    function DetectObjectType(const SQL: string): TDBObjectType;
    function ExtractObjectName(const SQL: string): string;
    function RemoveComments(const SQL: string): string;
    function RemoveSetTerm(const SQL: string): string;
    function SplitSQL(const DDLText: string): TArray<string>;
    function SplitDelimitersParts(const DDLText: string) : TArray<TDelimitersParts>;

  public
    constructor Create();
    function Parse(const DDLText: string): TList<TDDLCommand>;

  end;

implementation

{ TParser }

constructor TParser.Create;
begin
end;

function TParser.DetectCommandType(const SQL: string): TDDLCommandType;
var
  Regex: TRegEx;
begin
  // Verifica se o comando é CREATE, ALTER ou DROP
  if TRegEx.IsMatch(SQL, '^\s*CREATE', [roIgnoreCase]) then
    Result := ddlCreate
  else if TRegEx.IsMatch(SQL, '^\s*ALTER', [roIgnoreCase]) then
    Result := ddlAlter
  else if TRegEx.IsMatch(SQL, '^\s*DROP', [roIgnoreCase]) then
    Result := ddlDrop
  else
    Result := ddlUnknown;
end;


function TParser.DetectObjectType(const SQL: string): TDBObjectType;
var
  Regex: TRegEx;
begin
  // Para TRIGGER, PROCEDURE, FUNCTION, VIEW, somente CREATE OR ALTER é permitido
  if TRegEx.IsMatch(SQL, '^\s*CREATE\s+OR\s+ALTER\s+TRIGGER', [roIgnoreCase]) then
    Result := objTrigger
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+OR\s+ALTER\s+PROCEDURE', [roIgnoreCase]) then
    Result := objProcedure
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+OR\s+ALTER\s+FUNCTION', [roIgnoreCase]) then
    Result := objFunction
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+OR\s+ALTER\s+VIEW', [roIgnoreCase]) then
    Result := objView

  // Para TABLE, INDEX, GEN, etc., somente CREATE é permitido
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+TABLE', [roIgnoreCase]) then
    Result := objTable
   else if TRegEx.IsMatch(SQL, '^\s*ALTER\s+TABLE', [roIgnoreCase]) then
    Result := objTable
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+INDEX', [roIgnoreCase]) then
    Result := objIndex
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+GENERATOR', [roIgnoreCase]) then
    Result := objGenerator
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+PRIMARY\s+KEY', [roIgnoreCase]) then
    Result := objUnknown  // Adicionar aqui se precisar identificar mais tipos como chave primária
  else
    Result := objUnknown;
end;




function TParser.SplitDelimitersParts(const DDLText: string): TArray<TDelimitersParts>;
var
  DelimiterPart : TDelimitersParts;
  Commands: TArray<string>;
  part : string;
  list : TList<TDelimitersParts>;
begin

   list := TList<TDelimitersParts>.Create;

   Commands := DDLText.Split(['SET TERM'], TStringSplitOptions.ExcludeEmpty);
   for part in Commands do begin
      if (part.Trim.Replace('^ ;','').Replace('; ^','') <> '') then begin
         if TRegEx.IsMatch(part, '^\s*\^\s;', [roIgnoreCase]) then
         begin
             DelimiterPart.SqlPart := part.TrimLeft.Replace('^ ;','');
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

function TParser.ExtractObjectName(const SQL: string): string;
var
  Regex: TRegEx;
  Match: TMatch;
begin
  // Busca pelo nome do objeto após o comando CREATE, ALTER ou DROP
  Match := TRegEx.Match(SQL, '\s*(TABLE|VIEW|PROCEDURE|FUNCTION|TRIGGER|INDEX|GENERATOR)\s+(\w+)', [roIgnoreCase]);
  if Match.Success then
    Result := Match.Groups[2].Value
  else
    Result := '';  // Retorna vazio se não encontrar o nome
end;



function TParser.RemoveComments(const SQL: string): string;
var
  Regex: TRegEx;
begin
  // Remove comentários de múltiplas linhas (/* ... */) e de linha (-- ...)

  // Remove comentários de bloco (/* ... */)
  Regex := TRegEx.Create('/\*.*?\*/', [roIgnoreCase, roSingleLine]);
  Result := Regex.Replace(SQL, '');

  // Remove comentários de linha (-- ...)
  Regex := TRegEx.Create('--.*$', [roIgnoreCase, roMultiline]);
  Result := Regex.Replace(Result, '');

  // Remove as quebras de linha extras que podem ser deixadas pela remoção
  Result := Result.Replace(#13#10, ' ').Replace(#10, ' ').Trim;
end;

function TParser.RemoveSetTerm(const SQL: string): string;
begin
   Result := SQL.Replace('SET TERM ^ ;','',[rfReplaceAll, rfIgnoreCase])
               .Replace('SET TERM ; ^','',[rfReplaceAll, rfIgnoreCase])
               .Replace('SET SQL DIALECT 3;','',[rfReplaceAll, rfIgnoreCase])
               .Trim();


  // Remove os comandos "SET TERM ^ ;" e "SET TERM ; ^", permitindo quebras de linha e espaços extras
 // Result := TRegEx.Replace(SQL, '^\s*SET\s+TERM\s*[\^;]\s*[\^;]\s*;', '', [roIgnoreCase, roMultiline]);
end;

function TParser.SplitSQL(const DDLText: string): TArray<string>;
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

  //CleanDDL := RemoveSetTerm(CleanDDL);

  // Busca posições dos delimitadores SET TERM


  for Part in SplitDelimitersParts(CleanDDL) do begin
      AddCommandsFrom(Part.SqlPart,Part.Delimiter);
  end;





//  PosStartTerm := Pos('SET TERM ^ ;', UpperCase(DDLText));
//  PosEndTerm := Pos('SET TERM ; ^', UpperCase(DDLText));
//
//  if (PosStartTerm > 0) and (PosEndTerm > PosStartTerm) then
//  begin
//    // Extrai texto antes do SET TERM ^ ;
//    PartBefore := Copy(DDLText, 1, PosStartTerm - 1);
//    // Extrai texto entre SET TERM ^ ; e SET TERM ; ^
//    PartBetween := Copy(DDLText, PosStartTerm + Length('SET TERM ^ ;'), PosEndTerm - (PosStartTerm + Length('SET TERM ^ ;')));
//    // Extrai texto depois do SET TERM ; ^
//    PartAfter := Copy(DDLText, PosEndTerm + Length('SET TERM ; ^'), Length(DDLText));
//
//    // Divide o texto antes e depois por ';'
//    AddCommandsFrom(PartBefore, ';');
//    AddCommandsFrom(PartBetween, '^');  // Delimitador para bloco entre SET TERM
//    AddCommandsFrom(PartAfter, ';');
//  end
//  else
//  begin
//    // Se não encontrar delimitadores SET TERM, divide tudo por ';'
//    AddCommandsFrom(DDLText, ';');
//  end;

  Result := Statements.ToArray;
  Statements.Free;
end;



function TParser.Parse(const DDLText: string): TList<TDDLCommand>;
var
  ResultList: TList<TDDLCommand>;
  Statements: TArray<string>;
  SQL, CleanSQL: string;
  Command: TDDLCommand;
begin
  ResultList := TList<TDDLCommand>.Create;

  // Divida o SQL com base nos delimitadores ";", "END^" para triggers, procedures e functions
  Statements := SplitSQL(DDLText);

  for SQL in Statements do
  begin
    CleanSQL := SQL.Trim;  // Cria uma nova variável CleanSQL com a string limpa
    if CleanSQL = '' then
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

