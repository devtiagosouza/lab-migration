unit Sql.Builder.SqlTemplate;

interface

uses
  System.SysUtils, System.Generics.Collections,FirebirdKeywords, System.StrUtils;


type
  ISQLTemplate = interface
    ['{AF122E6B-5B8A-4BE6-A0D1-2D4171EFC1B3}']
    function SetPar(const Name: string; const Value: string; const considerReservedWords : boolean = false): ISQLTemplate; overload;
    function SetPar(const Name: string; const ACondition : boolean; const ATrue: string; const aFalse: string;const considerReservedWords : boolean = false): ISQLTemplate; overload;
    function SetPar(const Name: string; const Enabled: Boolean; const considerReservedWords : boolean = false): ISQLTemplate; overload;
    function AsString(CommandDelimiter : char): string;
  end;




TSQLTemplate = class(TInterfacedObject, ISQLTemplate)
  private
    FTemplate: string;
    FStringParams: TDictionary<string, string>;
    FConditionals: TDictionary<string, Boolean>;
  public
    constructor Create(const Template: string);
    destructor Destroy; override;

    function SetPar(const Name: string; const Value: string;const considerReservedWords : boolean = false): ISQLTemplate; overload;
    function SetPar(const Name: string; const ACondition : boolean; const ATrue: string; const aFalse: string;const considerReservedWords : boolean = false): ISQLTemplate; overload;
    function SetPar(const Name: string; const Enabled: Boolean;const considerReservedWords : boolean = false): ISQLTemplate; overload;
    function AsString(CommandDelimiter : char): string;
  end;

implementation

uses  System.RegularExpressions;

{ TCommandTemplate }

constructor TSQLTemplate.Create(const Template: string);
begin
  inherited Create;
  FTemplate := Template;
  FStringParams := TDictionary<string, string>.Create;
  FConditionals := TDictionary<string, Boolean>.Create;
end;

destructor TSQLTemplate.Destroy;
begin
  FStringParams.Free;
  FConditionals.Free;
  inherited;
end;

function TSQLTemplate.SetPar(const Name: string; const Value: string;const considerReservedWords : boolean = false): ISQLTemplate;
begin
  FStringParams.AddOrSetValue(Name.ToUpper,IfThen(considerReservedWords and TFirebirdKeywords.IsReservedWord(Value),'"'+value+'"', value ));
  Result := Self;
end;


function TSQLTemplate.SetPar(const Name: string; const Enabled: Boolean;const considerReservedWords : boolean = false): ISQLTemplate;
begin
  FConditionals.AddOrSetValue(Name.ToUpper, Enabled);
  Result := Self;
end;

function TSQLTemplate.SetPar(const Name: string; const ACondition: boolean;
  const ATrue, aFalse: string;const considerReservedWords : boolean = false): ISQLTemplate;
  var
    value : string;
begin
   value := ifthen(ACondition,ATrue,aFalse);

  FStringParams.AddOrSetValue(Name.ToUpper,IfThen(considerReservedWords and TFirebirdKeywords.IsReservedWord(Value),'"'+value+'"', value ));
  Result := Self;
end;

function TSQLTemplate.AsString(CommandDelimiter : char): string;
var
  Key, Output: string;
  Match: TMatch;
  Regex: TRegEx;
begin
  Output := FTemplate;

  Regex := TRegEx.Create('\{(.*?)\}', [roIgnoreCase]);
  for Match in Regex.Matches(Output) do
  begin
    Key := Match.Groups[1].Value.Trim;
    if FConditionals.ContainsKey(Key.ToUpper) and FConditionals[Key.ToUpper] then
      Output := Output.Replace(Match.Value, Key)
    else
      Output := Output.Replace(Match.Value, '');
  end;

  for Key in FStringParams.Keys do
    Output := Output.Replace(':' + Key, FStringParams[Key], [rfReplaceAll, rfIgnoreCase]);

  Output := TRegEx.Replace(Output, '[ \t]+', ' ');
  Output := TRegEx.Replace(Output, '\s*\n\s*', sLineBreak);
  Output := Output.Trim;

  if (Output.Trim.Replace(sLineBreak,'').EndsWith(CommandDelimiter) = false) and (CommandDelimiter <> '') then
      Output := Output+CommandDelimiter;

  Result := Output;


end;



end.

