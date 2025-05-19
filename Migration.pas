unit Migration;

interface

uses  System.Classes,System.SysUtils, System.StrUtils;

type TMigration = class

protected
    procedure AddScript(const AScript : string);

private
    ScriptList : TStringList;
    function GetScript: string;

public
  property Script : string read GetScript;

  constructor Create();
  destructor Destroy; override;

end;

implementation

{ TMigration }

procedure TMigration.AddScript(const AScript: string);
begin
 ScriptList.Add(AScript);
end;

constructor TMigration.Create();
begin
   ScriptList := TStringList.Create;

end;

destructor TMigration.Destroy;
begin
  if Assigned(ScriptList) then
      ScriptList.Free;
end;

function TMigration.GetScript: string;
begin
  result := string.join(sLineBreak+sLineBreak+sLineBreak,ScriptList.ToStringArray);
end;


end.
