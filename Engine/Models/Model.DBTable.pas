unit Model.DBTable;

interface
  uses Model.DBObject, Model.DBField, Model.DBIndex, Model.DBGenerator, Model.DBTrigger, DCollections,
  System.Classes, System.StrUtils, System.SysUtils, Sql.Script.Builder, Sql.Builder;

  type TDBTable = class(TDBObject)

  private
    FFields: TList<TDBField>;
    FPrimaryKeys: TList<TDBPrimaryKey>;
    FForeignKeys: TList<TDBForeignKey>;
    FCheckContrainsts: TList<TDBCheck>;
    FUniqueConstraints: TList<TDBUnique>;
    FIndices: TList<TDBIndex>;
    FTriggers: TList<TDBTrigger>;


    function GetMaxDigitCount<T: class>(const AList: TList<T>; PropertyGetter: TFunc<T, string>): Integer;

  public
      property Fields : TList<TDBField> read FFields write FFields;
      property PrimaryKeys : TList<TDBPrimaryKey> read FPrimaryKeys write FPrimaryKeys;
      property ForeignKeys : TList<TDBForeignKey> read FForeignKeys write FForeignKeys;
      property CheckContrainsts : TList<TDBCheck> read FCheckContrainsts write FCheckContrainsts;
      property UniqueConstraints : TList<TDBUnique> read FUniqueConstraints write FUniqueConstraints;
      property Indices : TList<TDBIndex> read FIndices write FIndices;
      property Triggers : TList<TDBTrigger> read FTriggers write FTriggers;

      function DDLCreate: string; override;


      constructor Create(AOwner: TComponent);




  end;


implementation

{ TDBTable }

constructor TDBTable.Create(AOwner: TComponent);
begin
  FFields := TList<TDBField>.create;
  FPrimaryKeys := TList<TDBPrimaryKey>.Create;
  FForeignKeys := TList<TDBForeignKey>.Create;
  FCheckContrainsts := TList<TDBCheck>.Create;
  FUniqueConstraints := TList<TDBUnique>.Create;
  FIndices := TList<TDBIndex>.Create;
  FTriggers := TList<TDBTrigger>.Create;
end;

function TDBTable.DDLCreate: string;
var
  vField : TDBField;
  i,x: Integer;
  Script: IScriptBuilder;
  primaryKey : TDBPrimaryKey;
  vGenerator : TDBGenerator;

  Sql : ISqlBuilder;
  MaxDigits : integer;
begin

  Script := TScriptBuilder.Create;


  Script.AppendLine(
    '/******************************************************************************/'+sLineBreak+
    '/****                                Tables                                ****/'+sLineBreak+
    '/******************************************************************************/');



  for I := 0 to Triggers.Count - 1 do begin
        for x := 0 to Triggers[i].Generators.Count - 1 do begin
           vGenerator := Triggers[i].Generators[x];

           Script.AppendLine(vGenerator.DDLCreate);
        end;
  end;


    MaxDigits := GetMaxDigitCount<TDBField>(Fields, function(Item: TDBField): string
                                                  begin
                                                    Result := Item.Name;
                                                  end);

    Sql := TSQLBuilder.Create
           .AppendLine('CREATE TABLE '+GetFormatedName+' (')
           .IncIndent;



    for i := 0 to Pred(Fields.Count) do
    begin
      vField := Fields[i];
      Sql.AppendLine(vField.GetFullFieldSet(MaxDigits+1));

      if (i < Pred(Fields.Count)) then
        Sql.DecIndent.Append(',').IncIndent;

    end;

    Sql.DecIndent
        .AppendLine(')');

    Script.AppendLine(Sql.AsString(';'));



    if (PrimaryKeys.Count > 0) then
    begin
      Script.AppendLine(
      '/******************************************************************************/'+sLineBreak+
      '/****                             Primary keys                             ****/'+sLineBreak+
      '/******************************************************************************/');

      for I := 0 to PrimaryKeys.Count - 1 do begin
         Script.AppendLine(PrimaryKeys[i].DDLCreate);
      end;

    end;

    if (ForeignKeys.Count > 0) then
    begin
        Script.AppendLine(
        '/******************************************************************************/'+sLineBreak+
        '/****                             Foreign keys                             ****/'+sLineBreak+
        '/******************************************************************************/');

      for I := 0 to ForeignKeys.Count - 1 do begin
         Script.AppendLine(ForeignKeys[i].DDLCreate);
      end;
    end;

    if (CheckContrainsts.Count > 0) then
    begin
        Script.AppendLine(
        '/******************************************************************************/'+sLineBreak+
        '/****                          Check constraints                           ****/'+sLineBreak+
        '/******************************************************************************/');

      for I := 0 to CheckContrainsts.Count - 1 do begin
         Script.AppendLine(CheckContrainsts[i].DDLCreate);
      end;
    end;

    if (UniqueConstraints.Count > 0) then
    begin
      Script.AppendLine(
      '/******************************************************************************/'+sLineBreak+
      '/****                          Unique constraints                          ****/'+sLineBreak+
      '/******************************************************************************/');

      for I := 0 to UniqueConstraints.Count - 1 do begin
         Script.AppendLine(UniqueConstraints[i].DDLCreate);
      end;
    end;

    if (Indices.Count > 0) then
    begin
      Script.AppendLine(
      '/******************************************************************************/'+sLineBreak+
      '/****                               Indices                                ****/'+sLineBreak+
      '/******************************************************************************/');

      for I := 0 to Indices.Count - 1 do begin
         Script.AppendLine(Indices[i].DDLCreate);
      end;
    end;

    if (Triggers.Count > 0) then
    begin
      Script.AppendLine('SET TERM ^ ;');

       Script.AppendLine(
      '/******************************************************************************/'+sLineBreak+
      '/****                         Triggers for tables                          ****/'+sLineBreak+
      '/******************************************************************************/');

      for I := 0 to Triggers.Count - 1 do begin
         Script.AppendLine(Triggers[i].DDLCreate);
         Script.AppendLine('^');
      end;

      Script.AppendLine('SET TERM ; ^');

    end;


    result := Script.AsString;
end;

function TDBTable.GetMaxDigitCount<T>(const AList: TList<T>;
  PropertyGetter: TFunc<T, string>): Integer;
var
  Item: T;
  MaxLength, LengthNome: Integer;
begin
  MaxLength := 0;

  for Item in AList do
  begin
    LengthNome := Length(PropertyGetter(Item));  // Calculando o comprimento da propriedade Nome
    if LengthNome > MaxLength then
      MaxLength := LengthNome;
  end;

  Result := MaxLength;
end;

end.
