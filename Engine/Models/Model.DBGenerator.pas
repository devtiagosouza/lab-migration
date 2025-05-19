unit Model.DBGenerator;

interface

  uses Model.DBObject, System.Classes, System.SysUtils, System.StrUtils,TypInfo;

  type TDBGenerator = class(TDBObject)

  private
    FIncrement: Int64;
    FInitialValue: Int64;
    FTriggerName: string;

  public

      property InitialValue : Int64 read FInitialValue write FInitialValue;
      property Increment : Int64 read FIncrement write FIncrement;
      property TriggerName : string read FTriggerName write FTriggerName;

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

