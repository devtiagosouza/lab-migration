unit Parser.Fields;

interface

uses
  System.SysUtils, System.RegularExpressions, DCollections,Model.DBField,Firebird.Types;

type
  TFieldParser = class
  private
    class function SplitColumns(const ColumnsSection: string): TArray<string>;
  public
    class function ParseField(const AColumnsDefs, ATableName: string): TDBField;  overload;
    class function ParseField(const Definition: string) : TDBField; overload;

    class function Parse(const AColumnsSection, ATableName: string) : TList<TDBField>;
  end;

implementation

{ TFieldParser }

class function TFieldParser.ParseField(const Definition: string): TDBField;
const
 // REGEX_PATTERN = '(?i)(VARCHAR\(\d+(?:,\d+)?\)|\w+(?:\(\d+(?:,\d+)?\))?|BLOB|SUB_TYPE\s+\w+|SEGMENT\s+SIZE\s+\d+|CHARACTER\s+SET\s+\w+|COLLATE\s+\w+|DEFAULT\s+(?:''''[^'']*''''|\S+)|NOT\s+NULL|NULL|CHECK\s*\([^\)]+\)|PRIMARY\s+KEY|UNIQUE|REFERENCES\s+\w+)';
REGEX_PATTERN =
    '(?i)(VARCHAR\(\d+(?:,\d+)?\)|BLOB|SUB_TYPE\s+\w+|SEGMENT\s+SIZE\s+\d+|' +
    'CHARACTER\s+SET\s+\w+|COLLATE\s+\w+|DEFAULT\s+(?:''''[^'']*''''|\S+)|NOT\s+NULL|NULL|' +
    'CHECK\s*\([^\)]+\)|PRIMARY\s+KEY|UNIQUE|REFERENCES\s+\w+|\w+(?:\(\d+(?:,\d+)?\))?)';

var
  Matches: TMatchCollection;
  Match: TMatch;
  FieldTypeSet: Boolean;
  Field: TDBField;
  Token: string;
  RawDefault: string;
begin
 Field := TDBField.Create;
  FieldTypeSet := False;

  Matches := TRegEx.Matches(Definition, REGEX_PATTERN);

  for Match in Matches do
  begin
    Token := Trim(Match.Value.ToUpper);

    if not FieldTypeSet then
    begin
      Field.FieldType := Match.Value; // original case
      FieldTypeSet := True;
    end
    else if Token.StartsWith('DEFAULT') then
    begin
      RawDefault := Trim(Copy(Match.Value, 9, MaxInt));
       // Remove aspas duplas externas se existirem (ex: ''TESTE'')
      if RawDefault.StartsWith('''') and RawDefault.EndsWith('''') then
        RawDefault := Copy(RawDefault, 2, Length(RawDefault) - 2);

      // Substitui aspas duplicadas internas por aspas simples normais
      RawDefault := StringReplace(RawDefault, '''''', '''', [rfReplaceAll]);

      Field.DefaultValue := RawDefault;

     // Field.DefaultValue := Trim(Copy(Match.Value, 9, MaxInt)); // remove "DEFAULT "



    end
    else if Token = 'NOT NULL' then
      Field.NotNull := True
    else if Token.StartsWith('CHARACTER SET') then
      Field.Charset := Copy(Match.Value, Length('CHARACTER SET ') + 1, MaxInt)
    else if Token.StartsWith('COLLATE') then
      Field.Collate := Copy(Match.Value, Length('COLLATE ') + 1, MaxInt);
  end;

  Result := Field;

end;

class function TFieldParser.ParseField(const AColumnsDefs, ATableName: string): TDBField;
var
 Field : TDBField;
 DefaultPart, CharSet, Collate: string;
 Match : TMatch;
 TypeMatch: TMatch;
 FieldMatch : TFieldTypeMatch;
 FieldTypeCandidate: string;
begin
    CharSet := '';
    Collate := '';
    DefaultPart := '';

    Field := TDBField.Create;
    Field.TableName := ATableName;
    Match := TRegEx.Match(AColumnsDefs, '^(\w+)\s+(.+)', [roIgnoreCase]);
    if Match.Success then
    begin
      Field.Name := Match.Groups[1].Value;
      FieldTypeCandidate := Match.Groups[2].Value;

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

      TypeMatch := MatchFirebirdType(FieldTypeCandidate).Match;
      if TypeMatch.Success then
        Field.FieldType := FieldMatch.Match.Value
      else
        Field.FieldType := FieldTypeCandidate;

      Field.Charset      := CharSet;
      Field.Collate      := Collate;
      Field.DefaultValue := DefaultPart;
      Field.NotNull      := TRegEx.IsMatch(AColumnsDefs, '\bNOT\s+NULL\b', [roIgnoreCase]);
    end
    else
    begin
      Field.Name := AColumnsDefs;
    end;

    Result := Field;
end;


class function TFieldParser.Parse(const AColumnsSection, ATableName: string): TList<TDBField>;
var
 Lines: TArray<string>;
 Line,TrimmedLine : string;
 Field : TDBField;
 DefaultPart, CharSet, Collate: string;
  Match, TypeMatch: TMatch;
  FieldTypeCandidate: string;
begin
  Lines := SplitColumns(AColumnsSection);
  Result := TList<TDBField>.Create;

  for Line in Lines do
  begin
    TrimmedLine := Line.Trim;
    if TrimmedLine = '' then
      Continue;

    Field := ParseField(TrimmedLine,ATableName);
    if (Field <> nil) then
        Result.Add(Field);
  end;
end;

class function TFieldParser.SplitColumns(
  const ColumnsSection: string): TArray<string>;
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
