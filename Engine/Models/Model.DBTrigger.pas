
unit Model.DBTrigger;


interface

  uses Model.DBObject,System.TypInfo, Model.DBGenerator,System.Generics.Collections, DCollections,
    System.Classes, System.SysUtils, System.StrUtils,System.RegularExpressions;



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
    FIsActive: boolean;
    FGenerators: TList<TDBGenerator>;

    function TriggerTypeToString(Value: TTriggerType): string;
    function ExtrairGenerators(): TArray<string>;
    procedure SetTriggerSource(const Value: string);


  public

      property TableName : string read FTableName write FTableName;
      property TriggerSource : string read FTriggerSource write FTriggerSource;
      property TriggerType : TTriggerType read FTriggerType write FTriggerType;
      property TriggerPosition : integer read FTriggerPosition write FTriggerPosition;
      property IsActive : boolean read FIsActive write FIsActive;


      function DDLCreate: string; override;
      function EqualityScript(Obj: TDBObject) : string; override;


      procedure SetTriggerTypeFromString(Value: string);


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

function TDBTrigger.EqualityScript(Obj: TDBObject): string;
begin
 result := '';
 if (isSameObject(Obj)) then begin
   if (not isSameText(DDLCreate, obj.DDLCreate)) then begin
      result := DDLCreate;
   end;
 end;
end;

function TDBTrigger.ExtrairGenerators(): TArray<string>;
var
  Regex: TRegEx;
  MatchCollection: TMatchCollection;
  i: Integer;
begin
  Regex := TRegEx.Create('GEN_ID\(([^,]+),\d+\)');
  MatchCollection := Regex.Matches(TriggerSource);

  SetLength(Result, MatchCollection.Count);



  for i := 0 to MatchCollection.Count - 1 do
  begin
    Result[i] := MatchCollection.Item[i].Groups[1].Value;  // Pega o nome do generator
  end;
end;



procedure TDBTrigger.SetTriggerSource(const Value: string);
var
  Match: TMatch;
  Regex: TRegEx;
  Generators: TDictionary<string, Boolean>;
  gen : string;
  generator : TDBGenerator;
begin
  FTriggerSource := Value;
  FGenerators := TList<TDBGenerator>.Create;


  Generators := TDictionary<string, Boolean>.Create;
  try
    Regex := TRegEx.Create('GEN_ID\((\w+),\d+\)');

    for Match in Regex.Matches(FTriggerSource) do
    begin
      Generators.AddOrSetValue(Match.Groups[1].Value, True);
    end;



    for gen in Generators.Keys do begin
       generator := TDBGenerator.Create();
       generator.Name := gen;

        FGenerators.Add(generator);
    end;

  finally
    Generators.Free;
  end;


end;

procedure TDBTrigger.SetTriggerTypeFromString(Value: string);
var
  TriggerTypeMap: TDictionary<string, TTriggerType>;
begin
  value := Value.Trim.Replace(' ','_');

  TriggerTypeMap := TDictionary<string, TTriggerType>.Create;
  try

    TriggerTypeMap.Add('BEFORE_INSERT', BEFORE_INSERT);
    TriggerTypeMap.Add('AFTER_INSERT', AFTER_INSERT);
    TriggerTypeMap.Add('BEFORE_UPDATE', BEFORE_UPDATE);
    TriggerTypeMap.Add('AFTER_UPDATE', AFTER_UPDATE);
    TriggerTypeMap.Add('BEFORE_DELETE', BEFORE_DELETE);
    TriggerTypeMap.Add('AFTER_DELETE', AFTER_DELETE);
    TriggerTypeMap.Add('BEFORE_INSERT_OR_UPDATE', BEFORE_INSERT_OR_UPDATE);
    TriggerTypeMap.Add('AFTER_INSERT_OR_UPDATE', AFTER_INSERT_OR_UPDATE);
    TriggerTypeMap.Add('BEFORE_INSERT_OR_DELETE', BEFORE_INSERT_OR_DELETE);
    TriggerTypeMap.Add('AFTER_INSERT_OR_DELETE', AFTER_INSERT_OR_DELETE);
    TriggerTypeMap.Add('BEFORE_UPDATE_OR_DELETE', BEFORE_UPDATE_OR_DELETE);
    TriggerTypeMap.Add('AFTER_UPDATE_OR_DELETE', AFTER_UPDATE_OR_DELETE);
    TriggerTypeMap.Add('BEFORE_INSERT_OR_UPDATE_OR_DELETE', BEFORE_INSERT_OR_UPDATE_OR_DELETE);
    TriggerTypeMap.Add('AFTER_INSERT_OR_UPDATE_OR_DELETE', AFTER_INSERT_OR_UPDATE_OR_DELETE);
    TriggerTypeMap.Add('ON_CONNECT', ON_CONNECT);
    TriggerTypeMap.Add('ON_DISCONNECT', ON_DISCONNECT);
    TriggerTypeMap.Add('ON_TRANSACTION_START', ON_TRANSACTION_START);
    TriggerTypeMap.Add('ON_TRANSACTION_COMMIT', ON_TRANSACTION_COMMIT);
    TriggerTypeMap.Add('ON_TRANSACTION_ROLLBACK', ON_TRANSACTION_ROLLBACK);

    // Verifica se a string existe no dicionário e atribui o valor correspondente
    if not TriggerTypeMap.TryGetValue(Value, FTriggerType) then
      raise Exception.Create('Valor inválido para o TriggerType: ' + Value);
  finally
    TriggerTypeMap.Free;
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

