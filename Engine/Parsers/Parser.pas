unit Parser;

interface

uses
  System.SysUtils, System.Classes, DCollections, System.RegularExpressions, System.StrUtils;

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

  TParser = class
  private
    function DetectCommandType(const SQL: string): TDDLCommandType;
    function DetectObjectType(const SQL: string): TDBObjectType;
    function ExtractObjectName(const SQL: string): string;
    function RemoveComments(const SQL: string): string;
    function RemoveSetTerm(const SQL: string): string;
    function SplitSQL(const DDLText: string): TArray<string>;
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
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+INDEX', [roIgnoreCase]) then
    Result := objIndex
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+GENERATOR', [roIgnoreCase]) then
    Result := objGenerator
  else if TRegEx.IsMatch(SQL, '^\s*CREATE\s+PRIMARY\s+KEY', [roIgnoreCase]) then
    Result := objUnknown  // Adicionar aqui se precisar identificar mais tipos como chave primária
  else
    Result := objUnknown;
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
  CleanSQL, Part: string;
  StartPos, EndPos, EndProcPos: Integer;
begin
  Statements := TList<string>.Create;
  CleanSQL := DDLText;

  // Primeiro, remove comentários e o SET TERM
  CleanSQL := RemoveComments(CleanSQL);
  CleanSQL := RemoveSetTerm(CleanSQL);

  // Processa o texto inteiro, procurando por delimitadores "^" ou ";" para dividir
  StartPos := 1;

  while StartPos <= Length(CleanSQL) do
  begin
    // Para TRIGGER, PROCEDURE, FUNCTION, VIEW, com "END" seguido de "^"
    EndProcPos := TRegEx.Match(CleanSQL.Substring(StartPos - 1), 'END\s*\^', [roIgnoreCase, roMultiline]).Index;

    if EndProcPos > 0 then
    begin
      // Encontra o "END^", captura o comando completo e adiciona à lista
      EndPos := StartPos + EndProcPos + 4;  // Pula o "END^"
      Part := CleanSQL.Substring(StartPos - 1, EndPos - StartPos);
      Statements.Add(Part.Trim);  // Adiciona a parte do comando SQL
      StartPos := EndPos + 1;     // Move para o próximo comando após o "END^"
    end
    else
    begin
      // Para comandos gerais, divide com ";"
      EndPos := PosEx(';', CleanSQL, StartPos);
      if EndPos = 0 then
        Break;  // Caso não haja mais delimitadores, saímos do loop

      Part := CleanSQL.Substring(StartPos - 1, EndPos - StartPos);
      Statements.Add(Part.Trim);  // Adiciona a parte do comando SQL
      StartPos := EndPos + 1;     // Move para o próximo comando após o ";"
    end;
  end;

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

