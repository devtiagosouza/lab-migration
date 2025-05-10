unit Model.DBTable;

interface
  uses Model.DBObject, Model.DBField, Model.DBIndex, Model.DBGenerator, Model.DBTrigger, DCollections,
  System.Classes, System.StrUtils, System.SysUtils, Sql.Query.Builder.CommandTemplate,Sql.Query.Builder;

  type TDBTable = class(TDBObject)

  private
    FFields: TList<TDBField>;
    FPrimaryKeys: TList<TDBPrimaryKey>;
    FForeignKeys: TList<TDBForeignKey>;
    FCheckContrainsts: TList<TDBCheck>;
    FUniqueConstraints: TList<TDBUnique>;
    FIndices: TList<TDBIndex>;
    FTriggers: TList<TDBTrigger>;


  public
      property Fields : TList<TDBField> read FFields write FFields;
      property PrimaryKeys : TList<TDBPrimaryKey> read FPrimaryKeys write FPrimaryKeys;
      property ForeignKeys : TList<TDBForeignKey> read FForeignKeys write FForeignKeys;
      property CheckContrainsts : TList<TDBCheck> read FCheckContrainsts write FCheckContrainsts;
      property UniqueConstraints : TList<TDBUnique> read FUniqueConstraints write FUniqueConstraints;
      property Indices : TList<TDBIndex> read FIndices write FIndices;
      property Triggers : TList<TDBTrigger> read FTriggers write FTriggers;

      function CreateCommand: string; override;


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

function TDBTable.CreateCommand: string;
const
  sintax : string = 'CREATE TABLE %s (' + sLineBreak + '%s' + sLineBreak + ');';
var
  vField : TDBField;
  strFields: TStringBuilder;
  i,x: Integer;
  Script: TStringList;
  primaryKey : TDBPrimaryKey;
  vGenerator : TDBGenerator;

  builder : TCommandTemplate;

begin
  strFields := TStringBuilder.Create;
  Script := TStringList.Create;


  







  for I := 0 to Triggers.Count - 1 do begin
        for x := 0 to Triggers[i].Generators.Count - 1 do begin
           vGenerator := Triggers[i].Generators[x];
           Result := Result+sLineBreak + vGenerator.CreateCommand+';'+sLineBreak;
        end;
  end;


  try
    for i := 0 to Fields.Count - 1 do
    begin
      vField := Fields[i];
      strFields.AppendFormat('    %s', [vField.CreateCommand]);

      if i < Fields.Count - 1 then
        strFields.Append(',' + sLineBreak);
    end;


    Result := Result+sLineBreak+Format(sintax, [Self.GetFormatedName, strFields.ToString]);





    if (PrimaryKeys.Count > 0) then
    begin

      Result := Result + sLineBreak+sLineBreak+
      '/******************************************************************************/'+sLineBreak+
      '/****                             Primary keys                             ****/'+sLineBreak+
      '/******************************************************************************/';

      for I := 0 to PrimaryKeys.Count - 1 do begin
         Result := Result+sLineBreak +PrimaryKeys[i].CreateCommand+';';
      end;

    end;

    if (ForeignKeys.Count > 0) then
    begin
        Result := Result+sLineBreak + sLineBreak+sLineBreak+
        '/******************************************************************************/'+sLineBreak+
        '/****                             Foreign keys                             ****/'+sLineBreak+
        '/******************************************************************************/';

      for I := 0 to ForeignKeys.Count - 1 do begin
         Result := Result+sLineBreak + ForeignKeys[i].CreateCommand+';';
      end;
    end;

    if (CheckContrainsts.Count > 0) then
    begin
        Result := Result + sLineBreak+sLineBreak+
        '/******************************************************************************/'+sLineBreak+
        '/****                          Check constraints                           ****/'+sLineBreak+
        '/******************************************************************************/';

      for I := 0 to CheckContrainsts.Count - 1 do begin
         Result := Result+sLineBreak + CheckContrainsts[i].CreateCommand+';';
      end;
    end;

    if (UniqueConstraints.Count > 0) then
    begin
      Result := Result + sLineBreak+sLineBreak+
      '/******************************************************************************/'+sLineBreak+
      '/****                          Unique constraints                          ****/'+sLineBreak+
      '/******************************************************************************/';

      for I := 0 to UniqueConstraints.Count - 1 do begin
         Result := Result+sLineBreak + UniqueConstraints[i].CreateCommand+';';
      end;
    end;

    if (Indices.Count > 0) then
    begin
      Result := Result + sLineBreak+sLineBreak+
      '/******************************************************************************/'+sLineBreak+
      '/****                               Indices                                ****/'+sLineBreak+
      '/******************************************************************************/';

      for I := 0 to Indices.Count - 1 do begin
         Result := Result+sLineBreak + Indices[i].CreateCommand+';';
      end;
    end;

    if (Triggers.Count > 0) then
    begin

       Result := Result + sLineBreak+'SET TERM ^ ;';

       Result := Result + sLineBreak+sLineBreak+
      '/******************************************************************************/'+sLineBreak+
      '/****                         Triggers for tables                          ****/'+sLineBreak+
      '/******************************************************************************/';

      for I := 0 to Triggers.Count - 1 do begin
         Result := Result+sLineBreak + Triggers[i].CreateCommand+sLineBreak+'^';
      end;

      Result := Result+sLineBreak +'SET TERM ; ^';

    end;


  finally
    strFields.Free;
  end;

end;

end.
