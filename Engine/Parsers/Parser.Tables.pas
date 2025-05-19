unit Parser.Tables;

interface

uses
  System.SysUtils, System.RegularExpressions, DCollections,Model.DBTable;

type
  TTableParser = class

  public
    class function Parse(const TableDDL: string): TDBTable;
  end;

implementation

 uses Parser.Fields;

class function TTableParser.Parse(const TableDDL: string): TDBTable;
var
  Table: TDBTable;
  ColumnsSection: string;
  Match : TMatch;
begin
  Table := TDBTable.Create;

  Match := TRegEx.Match(TableDDL, 'CREATE\s+TABLE\s+(\w+)', [roIgnoreCase]);
  if Match.Success then
    Table.Name := Match.Groups[1].Value;

  Match := TRegEx.Match(TableDDL, '\((.*)\)', [roSingleLine]);
  if Match.Success then
    ColumnsSection := Match.Groups[1].Value
  else
    ColumnsSection := '';

  Table.Fields := TFieldParser.Parse(ColumnsSection,Table.Name);

  Result := Table;
end;


end.

