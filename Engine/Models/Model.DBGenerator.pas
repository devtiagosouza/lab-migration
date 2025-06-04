unit Model.DBGenerator;

interface

  uses Model.DBObject, System.Classes, System.SysUtils;

  type TDBGenerator = class(TDBObject)

  private
    FTriggerName: string;

  public
      function DDLCreate: string; override;

       function EqualityScript(Obj: TDBObject; args : array of TObject) : string; override;

      constructor Create(AName : string);
  end;


implementation

{ TDBGenerator }

constructor TDBGenerator.Create(AName : string);
begin
  inherited Create(AName);
  ObjectTypeFriendlyName := 'Generator';
end;

function TDBGenerator.DDLCreate: string;
begin
  result := 'CREATE GENERATOR '+GetFormatedName+';';
end;

function TDBGenerator.EqualityScript(Obj: TDBObject; args : array of TObject): string;
begin
   result := '';
end;

end.

