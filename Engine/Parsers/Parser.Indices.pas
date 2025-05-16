unit Parser.Indices;

interface

uses
  System.SysUtils, System.RegularExpressions,System.StrUtils, System.Generics.Collections,
  Model.DBIndex;

type
  TIndexParser = class
  public
    class function Parse(const IndexDDL: string): TDBIndex;
  end;

implementation

class function TIndexParser.Parse(const IndexDDL: string): TDBIndex;
 const   RegexIndiceCampos =
    '(?i)^\s*CREATE\s+(UNIQUE DESCENDING|UNIQUE|DESCENDING)?\s*INDEX\s+(\w+)\s+ON\s+(\w+)\s*\(\s*([^)]+?)\s*\)';
 const RegexComputedBy =
    '(?i)^\s*CREATE\s+(UNIQUE DESCENDING|UNIQUE|DESCENDING)?\s*INDEX\s+(\w+)\s+ON\s+(\w+)\s+COMPUTED\s+BY\s*\((.*)\)\s*$';

var
  Index: TDBIndex;
  Match: TMatch;
  ColumnsStr: string;
  Columns: TArray<string>;
  i: Integer;


  modified : string;
begin

  Match := TRegEx.Match(IndexDDL, RegexIndiceCampos, [roIgnoreCase]);
  if Match.Success then begin
     Index := TDBIndex.Create;
     modified := Match.Groups[1].Value;
     Index.Name :=  Match.Groups[2].Value;
     Index.TableName :=  Match.Groups[3].Value;
     Index.OnFields := Match.Groups[4].Value;
     Index.Unique := TRegEx.IsMatch(modified, '\s+UNIQUE\s', [roIgnoreCase]);
     Index.Sorting := ifthen( TRegEx.IsMatch(modified, '\s+DESCENDING\s', [roIgnoreCase]), 'DESCENDING', '');
  end
  else begin
     Match := TRegEx.Match(IndexDDL, RegexComputedBy, [roIgnoreCase]);
     if Match.Success then begin
       Index := TDBIndex.Create;
       modified := Match.Groups[1].Value;
       Index.Name :=  Match.Groups[2].Value;
       Index.TableName :=  Match.Groups[3].Value;
       Index.OnFields := Match.Groups[4].Value;
       Index.Unique := TRegEx.IsMatch(modified, '\s+UNIQUE\s', [roIgnoreCase]);
       Index.Sorting := ifthen( TRegEx.IsMatch(modified, '\s+DESCENDING\s', [roIgnoreCase]), 'DESCENDING', '');
     end
  end;

  Result := Index;

  //  if TRegEx.IsMatch(IndexDDL, '^CREATE\s+UNIQUE\s+INDEX', [roIgnoreCase]) then
  //    Index.Unique := True;
  //
  //  Match := TRegEx.Match(IndexDDL, 'INDEX\s+(\w+)', [roIgnoreCase]);
  //  if Match.Success then
  //    Index.Name := Match.Groups[1].Value;
  //
  //  Match := TRegEx.Match(IndexDDL, 'ON\s+(\w+)', [roIgnoreCase]);
  //  if Match.Success then
  //    Index.TableName := Match.Groups[1].Value;
  //
  //  Match := TRegEx.Match(IndexDDL, '\(([^)]+)\)', [roIgnoreCase]);
  //  if Match.Success then
  //  begin
  //    ColumnsStr := Match.Groups[1].Value;
  //    Columns := ColumnsStr.Split([','], TStringSplitOptions.ExcludeEmpty);
  //    Index.OnFields := '';
  //    for i := Low(Columns) to High(Columns) do
  //    begin
  //      Index.OnFields := Index.OnFields + Columns[i].Trim;
  //      if i < High(Columns) then
  //        Index.OnFields := Index.OnFields + ',';
  //    end;
  //  end;

end;

end.

