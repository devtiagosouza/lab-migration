unit Parser.Constraints;

interface

uses
  System.SysUtils, System.RegularExpressions,
  Model.DBIndex, Model.DBObject;

type
  TConstraintParser = class
  public
    class function Parse(const ConstraintSQL: string): TDBObject;
  end;

implementation

class function TConstraintParser.Parse(const ConstraintSQL: string): TDBObject;
var
  Constraint: TDBPrimaryKey;
  Match: TMatch;
  ColumnsStr, RefColsStr: string;
  Columns, RefColumns: TArray<string>;
  i: Integer;
  ConstraintName: string;
  UQ : TDBUnique;
  FK : TDBForeignKey;
  CHK : TDBCheck;
begin
  Result := nil;

  // PRIMARY KEY
  Match := TRegEx.Match(ConstraintSQL, '^\s*ALTER\s+TABLE\s+(\w+)\s+ADD\s+CONSTRAINT\s+(\w+)\s+PRIMARY\s+KEY\s*\(([^)]+)\)', [roIgnoreCase]);
  if Match.Success then begin
  begin
     Constraint := TDBPrimaryKey.Create;
     Constraint.TableName := Match.Groups[1].Value;
     ConstraintName := Match.Groups[2].Value;
     Constraint.Name := ConstraintName;
     ColumnsStr := Match.Groups[3].Value;
    Columns := ColumnsStr.Split([','], TStringSplitOptions.ExcludeEmpty);
    Constraint.OnFields := '';
    for i := Low(Columns) to High(Columns) do
    begin
      Constraint.OnFields := Constraint.OnFields + Columns[i].Trim;
      if i < High(Columns) then
        Constraint.OnFields := Constraint.OnFields + ',';
    end;
    Result := Constraint;
    Exit;
  end;
  end;
  // PRIMARY KEY
  Match := TRegEx.Match(ConstraintSQL, 'CONSTRAINT\s+(\w+)\s+PRIMARY\s+KEY\s*\(([^)]+)\)', [roIgnoreCase]);
  if Match.Success then
  begin
    Constraint := TDBPrimaryKey.Create;
    ConstraintName := Match.Groups[1].Value;
    Constraint.Name := ConstraintName;
    Constraint.TableName := '';
    ColumnsStr := Match.Groups[2].Value;
    Columns := ColumnsStr.Split([','], TStringSplitOptions.ExcludeEmpty);
    Constraint.OnFields := '';
    for i := Low(Columns) to High(Columns) do
    begin
      Constraint.OnFields := Constraint.OnFields + Columns[i].Trim;
      if i < High(Columns) then
        Constraint.OnFields := Constraint.OnFields + ',';
    end;
    Result := Constraint;
    Exit;
  end;

  // UNIQUE
  Match := TRegEx.Match(ConstraintSQL, 'CONSTRAINT\s+(\w+)\s+UNIQUE\s*\(([^)]+)\)', [roIgnoreCase]);
  if Match.Success then
  begin
    UQ := TDBUnique.Create;
    UQ.Name := Match.Groups[1].Value;
    UQ.TableName := '';
    ColumnsStr := Match.Groups[2].Value;
    Columns := ColumnsStr.Split([','], TStringSplitOptions.ExcludeEmpty);
    UQ.OnFields := '';
    for i := Low(Columns) to High(Columns) do
    begin
      UQ.OnFields := UQ.OnFields + Columns[i].Trim;
      if i < High(Columns) then
        UQ.OnFields := UQ.OnFields + ',';
    end;
    Result := UQ;
    Exit;
  end;

  // FOREIGN KEY
  Match := TRegEx.Match(ConstraintSQL,
    'CONSTRAINT\s+(\w+)\s+FOREIGN\s+KEY\s*\(([^)]+)\)\s+REFERENCES\s+(\w+)\s*\(([^)]+)\)', [roIgnoreCase]);
  if Match.Success then
  begin
    FK := TDBForeignKey.Create;
    FK.Name := Match.Groups[1].Value;
    FK.TableName := '';
    ColumnsStr := Match.Groups[2].Value;
    FK.OnFields := ColumnsStr.Replace(' ', '');
    FK.FKTable := Match.Groups[3].Value;
    RefColsStr := Match.Groups[4].Value;
    FK.FKField := RefColsStr.Replace(' ', '');
    Result := FK;
    Exit;
  end;

  // CHECK
  Match := TRegEx.Match(ConstraintSQL, 'CONSTRAINT\s+(\w+)\s+CHECK\s*\((.+)\)', [roIgnoreCase]);
  if Match.Success then
  begin
    CHK := TDBCheck.Create;
    CHK.Name := Match.Groups[1].Value;
    CHK.TableName := '';
    CHK.Source := Match.Groups[2].Value.Trim;
    Result := CHK;
    Exit;
  end;

  // Se não reconhecido
  Result := nil;
end;

end.

