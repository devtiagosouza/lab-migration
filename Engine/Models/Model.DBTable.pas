unit Model.DBTable;

interface
  uses Model.DBObject,System.Regularexpressions, System.Generics.Collections, Model.DBField,
  Model.DBIndex, Model.DBGenerator, Model.DBTrigger, DCollections, System.Classes,System.SysUtils,
  Sql.Script.Builder, Sql.Builder;

  type TDBTable = class(TDBObject)

  private
    FFields: TList<TDBField>;
    FPrimaryKeys: TList<TDBPrimaryKey>;
    FForeignKeys: TList<TDBForeignKey>;
    FCheckContraints: TList<TDBCheck>;
    FUniqueConstraints: TList<TDBUnique>;
    FIndices: TList<TDBIndex>;
    FTriggers: TList<TDBTrigger>;


    function GetMaxDigitCount<T: class>(const AList: TList<T>; PropertyGetter: TFunc<T, string>): Integer;
    function GetGenerators: TList<TDBGenerator>;

  public
      property Fields : TList<TDBField> read FFields write FFields;
      property PrimaryKeys : TList<TDBPrimaryKey> read FPrimaryKeys write FPrimaryKeys;
      property ForeignKeys : TList<TDBForeignKey> read FForeignKeys write FForeignKeys;
      property CheckContrainsts : TList<TDBCheck> read FCheckContraints write FCheckContraints;
      property UniqueConstraints : TList<TDBUnique> read FUniqueConstraints write FUniqueConstraints;
      property Indices : TList<TDBIndex> read FIndices write FIndices;
      property Triggers : TList<TDBTrigger> read FTriggers write FTriggers;


      property Generators : TList<TDBGenerator> read GetGenerators;


      function DDLCreate(args: array of TObject): string; overload; override;
      function DDLCreate() : string; overload; override;
      function EqualityScript(Obj: TDBObject; args : array of TObject) : string; override;

      constructor Create(AName : string);

  end;


implementation

{ TDBTable }

constructor TDBTable.Create(AName : string);
begin
  inherited Create(AName);

  FFields := TList<TDBField>.create;
  FPrimaryKeys := TList<TDBPrimaryKey>.Create;
  FForeignKeys := TList<TDBForeignKey>.Create;
  FCheckContraints := TList<TDBCheck>.Create;
  FUniqueConstraints := TList<TDBUnique>.Create;
  FIndices := TList<TDBIndex>.Create;
  FTriggers := TList<TDBTrigger>.Create;
  ObjectTypeFriendlyName := 'Tabela';
end;

function TDBTable.DDLCreate: string;
begin
  Result := DDLCreate([]);
end;

function TDBTable.DDLCreate(args: array of TObject): string;
var
  vField : TDBField;
  i,x: Integer;
  Script: IScriptBuilder;
  primaryKey : TDBPrimaryKey;
 // vGenerator : TDBGenerator;

  Sql : ISqlBuilder;
  MaxDigits : integer;

  vGenToCompare : TList<TDBGenerator>;
begin

  Script := TScriptBuilder.Create;


  Script.AppendLine(
    '/******************************************************************************/'+sLineBreak+
    '/****                                Tables                                ****/'+sLineBreak+
    '/******************************************************************************/');



  for I := 0 to Generators.Count - 1 do begin

       if (Length(args) > 0) and (args[0] is TList<TDBGenerator>) then begin
         vGenToCompare := (args[0] as TList<TDBGenerator>);

         if vGenToCompare.Exists(function(g : TDBGenerator) : boolean
         begin
            result := g.Name = Generators[i].Name;
         end) then begin
             Script.AppendLine(Generators[i].DDLCreate);
         end;

       end
       else Script.AppendLine(Generators[i].DDLCreate);

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

function TDBTable.EqualityScript(Obj: TDBObject; args : array of TObject): string;
var
  outro : TDBTable;

  vGen,vOtherGen : TDBGenerator;
  vField, vOtherField : TDBField;
  vPK, vOtherPK  : TDBPrimaryKey;
  vFK, vOtherFK  : TDBForeignKey;
  vCheck, vOtherCheck  : TDBCheck;
  vUnique, vOtherUnique : TDBUnique;
  vIndex, vOtherIndex : TDBIndex;
  script : IScriptBuilder;
  sql : string;
  vTrigger, vOtherTrigger : TDBTrigger;
  scriptTriggers : IScriptBuilder;
begin
   script := TScriptBuilder.Create;

   outro := TDBTable(Obj);

   for vGen in GetGenerators do begin

       if (Length(args) > 0) and (args[0] is TList<TDBGenerator>) then begin

           if ((args[0] as TList<TDBGenerator>).Exists(function(g : TDBGenerator) : boolean
           begin
              result := g.Name = vGen.Name;
           end)) then begin

               vOtherGen := outro.Generators.First(function(g : TDBGenerator) : boolean begin
                  result := g.Name = vGen.Name;
               end);



               if (vOtherGen <> nil) then begin
                   sql := vGen.EqualityScript(vOtherGen,[]);
                   script.AppendLine(sql);
               end
               else begin
                  script.AppendLine(vGen.DDLCreate);
               end;
           end;

       end;


   end;

   for vField in FFields do begin

       vOtherField := outro.Fields.First(function(f : TDBField) : boolean begin
          result := f.Name = vField.Name;
       end);

       if (vOtherField <> nil) then begin
           sql := vField.EqualityScript(vOtherField,[]);
           script.AppendLine(sql);
       end
       else begin
          script.AppendLine(vField.DDLCreate);
       end;
   end;

   for vPK in FPrimaryKeys do begin
       vOtherPK := outro.PrimaryKeys.First(function(f : TDBPrimaryKey) : boolean begin
          result := f.Name = vPK.Name;
       end);

       if (vOtherPK <> nil) then begin
           sql := vPK.EqualityScript(vOtherPK,[]);
           script.AppendLine(sql);
       end
       else begin
          script.AppendLine(vPK.DDLCreate);
       end;

   end;

   for vFK in FForeignKeys do begin
       vOtherFK := outro.ForeignKeys.First(function(f : TDBForeignKey) : boolean begin
          result := f.Name = vFK.Name;
       end);

       if (vOtherFK <> nil) then begin
           sql := vFK.EqualityScript(vOtherFK,[]);
           script.AppendLine(sql);
       end
       else begin
          script.AppendLine(vFK.DDLCreate);
       end;

   end;

   for vCheck in FCheckContraints do begin
       vOtherCheck := outro.CheckContrainsts.First(function(f : TDBCheck) : boolean begin
          result := f.Name = vCheck.Name;
       end);

       if (vOtherCheck <> nil) then begin
           sql := vCheck.EqualityScript(vOtherCheck,[]);
           script.AppendLine(sql);
       end
       else begin
          script.AppendLine(vCheck.DDLCreate);
       end;
   end;

   for vUnique in FUniqueConstraints do begin
       vOtherUnique := outro.UniqueConstraints.First(function(f : TDBUnique) : boolean begin
          result := f.Name = vUnique.Name;
       end);

       if (vOtherUnique <> nil) then begin
           sql := vUnique.EqualityScript(vOtherUnique,[]);
           script.AppendLine(sql);
       end
       else begin
          script.AppendLine(vUnique.DDLCreate);
       end;
   end;

   for vIndex in FIndices do begin
       vOtherIndex := outro.Indices.First(function(f : TDBIndex) : boolean begin
          result := f.Name = vIndex.Name;
       end);

       if (vOtherIndex <> nil) then begin
           sql := vIndex.EqualityScript(vOtherIndex,[]);
           script.AppendLine(sql);
       end
       else begin
          script.AppendLine(vIndex.DDLCreate);
       end;
   end;

   if (FTriggers.Count > 0) then begin
      scriptTriggers := TScriptBuilder.Create;


       for vTrigger in FTriggers do begin


           vOtherTrigger := outro.Triggers.First(function(t : TDBTrigger) : boolean begin
              result := t.Name = vTrigger.Name;
           end);

           if (vOtherTrigger <> nil) then begin
               sql := vTrigger.EqualityScript(vOtherTrigger,[]);
               scriptTriggers.AppendLine(sql);

               if (string.IsNullOrEmpty(sql) = false) then
                   scriptTriggers.Append('^');

           end
           else begin
              scriptTriggers.AppendLine(vTrigger.DDLCreate)
                 .Append('^');
           end;
       end;

       if (scriptTriggers.AsString <> '') then begin

          script.AppendLine.AppendLine('SET TERM ^ ;').AppendLine;
          script.AppendLine(scriptTriggers.AsString);
          script.AppendLine.AppendLine('SET TERM ; ^').AppendLine;

       end;


   end;


   Result := script.AsString;

end;

function TDBTable.GetGenerators: TList<TDBGenerator>;
var
  Match: TMatch;
  Regex: TRegEx;
  DicGenerators: TDictionary<string, Boolean>;
  gen : string;


  trigger : TDBTrigger;
  triggerBody : string;

begin
 Result := TList<TDBGenerator>.Create;
 DicGenerators := TDictionary<string, Boolean>.Create;

 for trigger in Triggers do begin
    Regex := TRegEx.Create('GEN_ID\((\w+),\d+\)');

    for Match in Regex.Matches(trigger.TriggerSource) do
    begin
      DicGenerators.AddOrSetValue(Match.Groups[1].Value, True);
    end;

 end;


    for gen in DicGenerators.Keys do begin
        Result.Add( TDBGenerator.Create(gen));
    end;


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
    LengthNome := Length(PropertyGetter(Item));
    if LengthNome > MaxLength then
      MaxLength := LengthNome;
  end;

  Result := MaxLength;
end;


end.

