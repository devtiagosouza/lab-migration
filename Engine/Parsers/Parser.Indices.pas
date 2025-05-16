unit Parser.Indices;

interface

uses
  System.SysUtils, System.RegularExpressions, System.Generics.Collections,
  Model.DBIndex;

type
  TIndexParser = class
  public
    class function Parse(const IndexDDL: string): TDBIndex;
  end;

implementation

class function TIndexParser.Parse(const IndexDDL: string): TDBIndex;
var
  Index: TDBIndex;
  Match: TMatch;
  ColumnsStr: string;
  Columns: TArray<string>;
  i: Integer;
begin
  Index := TDBIndex.Create;

  if TRegEx.IsMatch(IndexDDL, '^CREATE\s+UNIQUE\s+INDEX', [roIgnoreCase]) then
    Index.Unique := True;

  Match := TRegEx.Match(IndexDDL, 'INDEX\s+(\w+)', [roIgnoreCase]);
  if Match.Success then
    Index.Name := Match.Groups[1].Value;

  Match := TRegEx.Match(IndexDDL, 'ON\s+(\w+)', [roIgnoreCase]);
  if Match.Success then
    Index.TableName := Match.Groups[1].Value;

  Match := TRegEx.Match(IndexDDL, '\(([^)]+)\)', [roIgnoreCase]);
  if Match.Success then
  begin
    ColumnsStr := Match.Groups[1].Value;
    Columns := ColumnsStr.Split([','], TStringSplitOptions.ExcludeEmpty);
    Index.OnFields := '';
    for i := Low(Columns) to High(Columns) do
    begin
      Index.OnFields := Index.OnFields + Columns[i].Trim;
      if i < High(Columns) then
        Index.OnFields := Index.OnFields + ',';
    end;
  end;

  Result := Index;
end;

end.

