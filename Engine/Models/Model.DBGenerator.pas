unit Model.DBGenerator;

interface

  uses Model.DBObject, System.Classes, System.SysUtils;

  type TDBGenerator = class(TDBObject)

  private
    FTriggerName: string;

  public
      function DDLCreate: string; override;

      constructor Create();
  end;


implementation

{ TDBGenerator }

constructor TDBGenerator.Create();
begin
  inherited Create;
  ObjectTypeFriendlyName := 'Generator';
end;

function TDBGenerator.DDLCreate: string;
begin
  result := 'CREATE GENERATOR '+GetFormatedName+';';
end;

end.

