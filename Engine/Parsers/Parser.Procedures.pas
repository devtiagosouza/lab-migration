unit Parser.Procedures;

interface

uses
  System.SysUtils, System.RegularExpressions,System.StrUtils, DCollections,
  Model.DBProcedure, Model.DBField, Parser.Fields, Firebird.Types;

type
  TProcedureParser = class

  private
      Pattern : string;
      FieldParser : TFieldParser;



  public
      function Parse(const ProcedureDDL: string): TDBProcedure;

      constructor Create();

  end;

implementation

{ TTableParser }

constructor TProcedureParser.Create();
begin
  Pattern := GetPattern(comCreateOrAlterProcedure);
  FieldParser := TFieldParser.Create;
end;

function TProcedureParser.Parse(const ProcedureDDL: string): TDBProcedure;
var
  proc: TDBProcedure;
  Match: TMatch;
  ParamSection: string;
  Params: TArray<string>;

  InputParams : string;
  OutputParams : string;
begin
  proc := TDBProcedure.Create;

  Match := TRegEx.Match(ProcedureDDL, Pattern, [roIgnoreCase, roSingleLine]);

  if Match.Success then
  begin
    proc.Name := Match.Groups[1].Value;

    InputParams := Match.Groups[2].Value;
    OutputParams := Match.Groups[3].Value;

    if (String.IsNullOrEmpty(InputParams) = false) then begin
      Proc.InputFields := FieldParser.Parse(InputParams,proc.Name);
    end;

    if (String.IsNullOrEmpty(OutputParams) = false) then begin
      Proc.OutputFields := FieldParser.Parse(OutputParams,proc.Name);
    end;

    Proc.ProcedureSource := Match.Groups[4].Value;
  end;

  Result := Proc;
end;

end.

