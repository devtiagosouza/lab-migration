unit Migration;

interface

uses  System.Classes,System.SysUtils, Model.DBObject;

type TMigration<T : TDBObject> = class

protected
    procedure AddScript(const AScript : string);
    
private
    ScriptList : TStringList;
    FBuilder : IBuilder<T>;
    
    function GetScript: string;

public
  property Script : string read GetScript;

  constructor Create();
  destructor Destroy; override;

end;

implementation

{ TMigration }

procedure TMigration<T>.AddScript(const AScript: string);
begin
  ScriptList.Add(AScript);
end;

constructor TMigration<T>.Create();
begin
   ScriptList := TStringList.Create;
   FBuilder := TBuilder<T>.Create;
end;

destructor TMigration<T>.Destroy;
begin
  if Assigned(ScriptList) then
      ScriptList.Free;
end;

function TMigration<T>.GetScript: string;
begin
  result := string.join(sLineBreak+sLineBreak+sLineBreak,ScriptList.ToStringArray);
end;


end.
