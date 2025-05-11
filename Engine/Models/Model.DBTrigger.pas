
unit Model.DBTrigger;


interface

  uses Model.DBObject, Model.DBGenerator, DCollections, TypInfo,  System.Classes, System.SysUtils, System.StrUtils;



  type TTriggerType = (
      BEFORE_INSERT = 1,
      AFTER_INSERT = 2,
      BEFORE_UPDATE = 3,
      AFTER_UPDATE = 4,
      BEFORE_DELETE = 5,
      AFTER_DELETE = 6,
      BEFORE_INSERT_OR_UPDATE = 17,
      AFTER_INSERT_OR_UPDATE = 18,
      BEFORE_INSERT_OR_DELETE = 25,
      AFTER_INSERT_OR_DELETE = 26,
      BEFORE_UPDATE_OR_DELETE = 27,
      AFTER_UPDATE_OR_DELETE = 28,
      BEFORE_INSERT_OR_UPDATE_OR_DELETE = 113,
      AFTER_INSERT_OR_UPDATE_OR_DELETE = 114,
      ON_CONNECT = 8192,
      ON_DISCONNECT = 8193,
      ON_TRANSACTION_START =  8194,
      ON_TRANSACTION_COMMIT = 8195,
      ON_TRANSACTION_ROLLBACK = 8196
  );



  type TDBTrigger = class(TDBObject)

  private
    FTriggerSource: string;
    FTableName: string;
    FTriggerType: TTriggerType;
    FTriggerPosition: integer;
    FIsForGenerator: boolean;
    FIsActive: boolean;
    FGenerators: TList<TDBGenerator>;

    function TriggerTypeToString(Value: TTriggerType): string;

  public

      property TableName : string read FTableName write FTableName;
      property TriggerSource : string read FTriggerSource write FTriggerSource;
      property TriggerType : TTriggerType read FTriggerType write FTriggerType;
      property TriggerPosition : integer read FTriggerPosition write FTriggerPosition;
      property IsForGenerator : boolean read FIsForGenerator write FIsForGenerator;
      property IsActive : boolean read FIsActive write FIsActive;
      property Generators : TList<TDBGenerator> read FGenerators write FGenerators;

      function DDLCreate: string; override;

      constructor Create();
  end;


implementation

{ TDBTrigger }

constructor TDBTrigger.Create();
begin
  FGenerators := TList<TDBGenerator>.Create();

end;

function TDBTrigger.DDLCreate: string;
var
 command : TStringList;

begin
  try
    try
      command := TStringList.Create;
      command.Add('CREATE OR ALTER TRIGGER '+Name+' FOR '+TableName);
      command.Add(ifthen(IsActive,'ACTIVE','INACTIVE')+' '+TriggerTypeToString(FTriggerType)+' POSITION '+TriggerPosition.ToString);
      command.Add(TriggerSource);

      result := string.join(sLineBreak,command.ToStringArray);
    except
     raise;
    end;
  finally
     if Assigned(command) then
        freeandNil(command);
  end;

end;

function TDBTrigger.TriggerTypeToString(Value: TTriggerType): string;
begin
  case Integer(Value) of
    1: Result := 'BEFORE INSERT';
    2: Result := 'AFTER INSERT';
    3: Result := 'BEFORE UPDATE';
    4: Result := 'AFTER UPDATE';
    5: Result := 'BEFORE DELETE';
    6: Result := 'AFTER DELETE';
    17: Result := 'BEFORE INSERT OR UPDATE';
    18: Result := 'AFTER INSERT OR UPDATE';
    25: Result := 'BEFORE INSERT OR DELETE';
    26: Result := 'AFTER INSERT OR DELETE';
    27: Result := 'BEFORE UPDATE OR DELETE';
    28: Result := 'AFTER UPDATE OR DELETE';
    113: Result := 'BEFORE INSERT OR UPDATE OR DELETE';
    114: Result := 'AFTER INSERT OR UPDATE OR DELETE';
    8192: Result := 'ON CONNECT';
    8193: Result := 'ON DISCONNECT';
    8194: Result := 'ON TRANSACTION START';
    8195: Result := 'ON TRANSACTION COMMIT';
    8196: Result := 'ON TRANSACTION ROLLBACK';
  else
    Result := 'UNKNOWN';
  end;
end;

end.

