unit Parser.Views;

interface

uses
  System.SysUtils, System.RegularExpressions,
  Model.DBView, Firebird.Types;

type
  TViewParser = class

private
  ViewPattern : string;

public
  function Parse(const ViewDDL: string): TDBView;

  constructor Create();
end;

implementation

{ TViewParser }

constructor TViewParser.Create();
begin
   ViewPattern := GetPattern(comCreateOrAlterView);
end;

function TViewParser.Parse(const ViewDDL: string): TDBView;
var
 View: TDBView;
 Match: TMatch;
 ColumnsSection : string;
 i : integer;
 field : string;
begin
    View := TDBView.Create;
    Match := TRegEx.Match(ViewDDL, ViewPattern, [roIgnoreCase,roSingleLine]);
    if Match.Success then begin
        View.Name := Match.Groups[1].Value;
        ColumnsSection := Match.Groups[2].Value;

        for field in ColumnsSection.Split([','], TStringSplitOptions.ExcludeEmpty) do begin
             view.FieldList.Add(field.Trim);
        end;

        View.ViewSource := Match.Groups[3].Value.Replace(';','');
    end;

    Result := View;
end;

end.
