unit Parser.Triggers;

interface

uses
  System.SysUtils, System.RegularExpressions,
  Model.DBTrigger, Firebird.Types;

type
   TTriggerParser = class

  private
     Pattern : string;

  public

    function Parse(const TriggerDDL: string): TDBTrigger;

    constructor Create();

  end;

implementation

{ TTriggerParser }

constructor TTriggerParser.Create();
begin
    Pattern := GetPattern(comCreateOrAlterTrigger)
end;

function TTriggerParser.Parse(const TriggerDDL: string): TDBTrigger;
var
  trigger: TDBTrigger;
  Match: TMatch;
  triggerType : string;
begin
  trigger := TDBTrigger.Create;
  Match := TRegEx.Match(TriggerDDL, Pattern, [roIgnoreCase, roSingleLine]);
  if Match.Success then
  begin
    trigger.Name := Match.Groups[1].Value;
    trigger.TableName := Match.Groups[2].Value;
    trigger.IsActive := Match.Groups[3].Value <> 'INACTIVE';
    triggerType := Match.Groups[4].Value.trim;
    trigger.SetTriggerTypeFromString(triggerType);
    trigger.TriggerPosition := StrToInt(Match.Groups[5].Value);
    trigger.TriggerSource := Match.Groups[6].Value;

    if (trigger.TriggerSource.Trim.Replace(sLineBreak,'').ToUpper.StartsWith('AS') = false) then begin
       trigger.TriggerSource.Insert(0,'AS'+sLineBreak);

    end;

  end;

  Result := trigger;
end;

end.
