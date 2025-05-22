unit Parser.Functions;

interface

uses
  System.SysUtils, System.RegularExpressions,DCollections,
  Model.DBFunction, Model.DBField, Parser.Fields, Firebird.Types;

type
  TFunctionParser = class

  private
    Pattern : string;
    FieldParser : TFieldParser;

  public

      function Parse(const FunctionDDL: string): TDBFunction;
      constructor Create();

  end;

implementation

{ TFunctionParser }

constructor TFunctionParser.Create;
begin
  Pattern := GetPattern(comCreateOrAlterFunction);
  FieldParser := TFieldParser.Create;
end;

function TFunctionParser.Parse(const FunctionDDL: string): TDBFunction;
var
  func: TDBFunction;
  Match: TMatch;

  InputParams : string;
begin
  func := TDBFunction.Create;

  Match := TRegEx.Match(FunctionDDL, Pattern, [roIgnoreCase, roSingleLine]);

  if Match.Success then
  begin
    func.Name := Match.Groups[1].Value;

    InputParams := Match.Groups[2].Value;
    func.ReturnType := Match.Groups[3].Value;

    if (String.IsNullOrEmpty(InputParams) = false) then begin
      func.InputFields := FieldParser.Parse(InputParams,func.Name);
    end;


    func.FunctionSource := Match.Groups[4].Value;
  end;

  Result := func;
end;

end.
