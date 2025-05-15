unit Parser.Tables;

interface

uses
  System.SysUtils, System.RegularExpressions, DCollections,
  Model.DBTable, Model.DBField, System.Generics.Collections,
  Firebird.Types;

type
  TTableParser = class
  private
    class function SplitColumns(const ColumnsSection: string): TArray<string>; static;
  public
    class function Parse(const TableDDL: string): TDBTable;
  end;

implementation



class function TTableParser.Parse(const TableDDL: string): TDBTable;
var
  Table: TDBTable;
  ColumnsSection: string;
  Lines: TArray<string>;
  Line, TrimmedLine: string;
  Field: TDBField;
  Match, TypeMatch: TMatch;
  DefaultPart, CharSet, Collate: string;
  FieldTypeCandidate: string;
begin
Table := TDBTable.Create(nil);

  Match := TRegEx.Match(TableDDL, 'CREATE\s+TABLE\s+(\w+)', [roIgnoreCase]);
  if Match.Success then
    Table.Name := Match.Groups[1].Value;

  Match := TRegEx.Match(TableDDL, '\((.*)\)', [roSingleLine]);
  if Match.Success then
    ColumnsSection := Match.Groups[1].Value
  else
    ColumnsSection := '';

  Lines := SplitColumns(ColumnsSection);

  for Line in Lines do
  begin
    TrimmedLine := Line.Trim;
    if TrimmedLine = '' then
      Continue;

    Field := TDBField.Create;
    Field.TableName := Table.Name;

    CharSet := '';
    Collate := '';
    DefaultPart := '';

    // Extrair nome e resto da linha (tipo + atributos)
    Match := TRegEx.Match(TrimmedLine, '^(\w+)\s+(.+)', [roIgnoreCase]);
    if Match.Success then
    begin
      Field.Name := Match.Groups[1].Value;
      FieldTypeCandidate := Match.Groups[2].Value;

      // Extrair e remover atributos para isolar o tipo real
      if TRegEx.IsMatch(FieldTypeCandidate, '\bCHARACTER\s+SET\s+\w+', [roIgnoreCase]) then
      begin
        Match := TRegEx.Match(FieldTypeCandidate, '\bCHARACTER\s+SET\s+(\w+)', [roIgnoreCase]);
        if Match.Success then
          CharSet := 'CHARACTER SET ' + Match.Groups[1].Value;
        FieldTypeCandidate := TRegEx.Replace(FieldTypeCandidate, '\bCHARACTER\s+SET\s+\w+', '', [roIgnoreCase]).Trim;
      end;

      if TRegEx.IsMatch(FieldTypeCandidate, '\bCOLLATE\s+\w+', [roIgnoreCase]) then
      begin
        Match := TRegEx.Match(FieldTypeCandidate, '\bCOLLATE\s+(\w+)', [roIgnoreCase]);
        if Match.Success then
          Collate := 'COLLATE ' + Match.Groups[1].Value;
        FieldTypeCandidate := TRegEx.Replace(FieldTypeCandidate, '\bCOLLATE\s+\w+', '', [roIgnoreCase]).Trim;
      end;

      if TRegEx.IsMatch(FieldTypeCandidate, '\bDEFAULT\s+((''([^'']|'')*'')|[^\s]+)', [roIgnoreCase]) then
      begin
        Match := TRegEx.Match(FieldTypeCandidate, '\bDEFAULT\s+((''([^'']|'')*'')|[^\s]+)', [roIgnoreCase]);
        if Match.Success then
          DefaultPart := 'DEFAULT ' + Match.Groups[1].Value;
        FieldTypeCandidate := TRegEx.Replace(FieldTypeCandidate, '\bDEFAULT\s+((''([^'']|'')*'')|[^\s]+)', '', [roIgnoreCase]).Trim;
      end;

      // Validar e capturar o tipo exato usando o dicionário
      TypeMatch := MatchFirebirdType(FieldTypeCandidate);
      if TypeMatch.Success then
        Field.FieldType := TypeMatch.Value
      else
        Field.FieldType := FieldTypeCandidate; // fallback para valor bruto se não encontrar

      Field.Charset := CharSet;
      Field.Collate := Collate;
      Field.DefaultValue := DefaultPart;

      // Verificar NOT NULL no texto completo da coluna
      Field.NotNull := TRegEx.IsMatch(TrimmedLine, '\bNOT\s+NULL\b', [roIgnoreCase]);
    end
    else
    begin
      Field.Name := TrimmedLine;
    end;

    Table.Fields.Add(Field);
  end;

  Result := Table;
end;

class function TTableParser.SplitColumns(const ColumnsSection: string): TArray<string>;
var
    I, StartIndex, ParenLevel: Integer;
    Ch: Char;
    ColList: TList<string>;
    Part: string;
begin
    ColList := TList<string>.Create;
    try
      ParenLevel := 0;
      StartIndex := 1;
      for I := 1 to Length(ColumnsSection) do
      begin
        Ch := ColumnsSection[I];
        case Ch of
          '(':
            Inc(ParenLevel);
          ')':
            if ParenLevel > 0 then
              Dec(ParenLevel);
          ',':
            if ParenLevel = 0 then
            begin
              Part := Trim(Copy(ColumnsSection, StartIndex, I - StartIndex));
              if Part <> '' then
                ColList.Add(Part);
              StartIndex := I + 1;
            end;
        end;
      end;
      Part := Trim(Copy(ColumnsSection, StartIndex, Length(ColumnsSection) - StartIndex + 1));
      if Part <> '' then
        ColList.Add(Part);
      Result := ColList.ToArray;
    finally
      ColList.Free;
    end;
end;

end.

