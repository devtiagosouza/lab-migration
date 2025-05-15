unit Parser.Fields;

interface

uses
  System.SysUtils, System.RegularExpressions, DCollections,
  Model.DBTable, Model.DBField, System.Generics.Collections,
  Firebird.Types;

type
  TFieldParser = class
  private
    class function SplitColumns(const ColumnsSection: string): TArray<string>; static;
  public
    class function Parse(const FieldDef: string): TDBField;
  end;

implementation

{ TFieldParser }

class function TFieldParser.Parse(const FieldDef: string): TDBField;
begin

end;

class function TFieldParser.SplitColumns(
  const ColumnsSection: string): TArray<string>;
begin

end;

end.
