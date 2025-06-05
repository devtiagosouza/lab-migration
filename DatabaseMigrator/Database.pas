unit Database;

interface
  uses Model.DBObject, Model.DBTable, Model.DBField, Model.DBView, Model.DBProcedure, Model.DBFunction, Model.DBTrigger,
  Model.DBGenerator,Model.DBIndex, System.Classes,FireDAC.Comp.Client,SqlResources,
  System.SysUtils, DCollections, Database.Interfaces,FireDAC.Stan.Option,DebugFilter, Migration;


type TDatabase = class(TInterfacedObject, IDatabase)


private
    FConnection : TFDConnection;
    FQueryTables : TFDQuery;
    FQueryPK : TFDQuery;
    FQueryFK : TFDQuery;
    FQueryCheck : TFDQuery;
    FQueryUnique : TFDQuery;
    FQueryIndex : TFDQuery;
    FQueryTrigger : TFDQuery;
    FQueryGenerator : TFDQuery;
    FQueryFunctions : TFDQuery;
    FQueryFields : TFDQuery;

    FTables: TList<TDBTable>;
    FViews: TList<TDBView>;
    FProcedures: TList<TDBProcedure>;
    FFunctions: TList<TDBFunction>;
    FTriggers: TList<TDBTrigger>;
    FGeneratorsWithoutDeps: TList<TDBGenerator>;
    FGenerators: TList<TDBGenerator>;
    FIndices: TList<TDBIndex>;

    FAditionalMigrations: TList<TMigration>;


    procedure LoadTablesAndViews(aWhere : string = '');
    function GetFields(aTableName : string) : TList<TDBField>;
    procedure GetProcedureFields(aProcedureName : string; var aInputFields : TList<TDBField>; var aOutputFields :  TList<TDBField>);
    procedure GetFunctionFields(aFunctionName : string; var aInputFields : TList<TDBField>; var aOutputFieldType :  string);

    function GetFieldList(aViewName : string) : TStringList;
    function GetPrimaryKeys(aTableName: string) : TList<TDBPrimaryKey>;
    function GetForeignKeys(aTableName: string) : TList<TDBForeignKey>;
    function GetCheckConstraints(aTableName: string) : TList<TDBCheck>;
    function GetUniqueConstraints(aTableName: string) : TList<TDBUnique>;
    function GetIndicesFromTable(aTableName: string) : TList<TDBIndex>;


    procedure LoadProcedures;
    procedure LoadFunctions;
    procedure LoadTriggers;
    procedure LoadGeneratorsWithoutDependencies;
    procedure LoadGenerators;

    function CreateQuery : TFDQuery;

    procedure LoadIncrementalMigrations;

    procedure AppendAditionalMigrationTables(migrationTables : TList<TDBTable>);
    procedure AppendAditionalMigrationFields(tableIndex: integer; migrationFields : TList<TDBField>);
    procedure AppendAditionalMigrationTriggers(tableIndex: integer; migrationTriggers : TList<TDBTrigger>);


public
    function GetTables: TList<TDBTable>;
    function GetViews: TList<TDBView>;
    function GetProcedures: TList<TDBProcedure>;
    function GetFunctions: TList<TDBFunction>;
    function GetTriggers: TList<TDBTrigger>;
    function GetGenerators: TList<TDBGenerator>;
    function GetAllGenerators: TList<TDBGenerator>;
    function GetIndices: TList<TDBIndex>;

    procedure LoadMetadata();

    procedure AddIncrementalMigration(aMigration : TMigration);

    procedure AddOrSetTable(obj : TDBTable);
    procedure AddOrSetField(obj: TDBField);
    procedure AddOrSetView(obj : TDBView);
    procedure AddOrSetProcedure(obj : TDBProcedure);
    procedure AddOrSetFunction(obj : TDBFunction);
    procedure AddOrSetTrigger(obj : TDBTrigger);
    procedure AddOrSetGenerator(obj : TDBGenerator);
    procedure AddOrSetIndex(obj : TDBIndex);



    constructor Create(AConnection : TFDConnection);

end;


implementation

{ TDatabase }

procedure TDatabase.AddIncrementalMigration(aMigration: TMigration);
begin
   FAditionalMigrations.Add(aMigration);
end;

procedure TDatabase.AddOrSetField(obj: TDBField);
var
  tableIndex : integer;
  fieldIndex : integer;
  table : TDBField;
begin
 tableIndex := FTables.IndexOf(function(o : TDBTable) : Boolean
    begin
        Result := (o.Name = obj.Name);
    end
  );

  if (tableIndex > -1) then begin
    fieldIndex := FTables[tableIndex].Fields.IndexOf(function( o : TDBField) : Boolean
    begin
        Result := (o.Name = obj.Name);
    end);

    if (fieldIndex > -1) then begin
        FTables[tableIndex].Fields[fieldIndex] := obj;
    end
    else begin
         FTables[tableIndex].Fields.Add(obj);
    end;

  end;

end;

procedure TDatabase.AddOrSetFunction(obj: TDBFunction);
var
  index : integer;
begin
    index := FFunctions.IndexOf(function( o : TDBFunction) : Boolean
    begin
        Result := (o.Name = obj.Name);
    end);

    if (index > -1) then begin
        FFunctions[index] := obj;
    end
    else begin
        FFunctions.Add(obj);
    end;
end;

procedure TDatabase.AddOrSetGenerator(obj: TDBGenerator);
var
  index : integer;
begin
    index := FGenerators.IndexOf(function( o : TDBGenerator) : Boolean
    begin
        Result := (o.Name = obj.Name);
    end);

    if (index > -1) then begin
        FGenerators[index] := obj;
    end
    else begin
        FGenerators.Add(obj);
    end;

end;

procedure TDatabase.AddOrSetIndex(obj: TDBIndex);
var
  index : integer;
begin
    index := FIndices.IndexOf(function( o : TDBIndex) : Boolean
    begin
        Result := (o.Name = obj.Name);
    end);

    if (index > -1) then begin
        FIndices[index] := obj;
    end
    else begin
        FIndices.Add(obj);
    end;

end;

procedure TDatabase.AddOrSetProcedure(obj: TDBProcedure);
var
  index : integer;
begin
    index := FProcedures.IndexOf(function( o : TDBProcedure) : Boolean
    begin
        Result := (o.Name = obj.Name);
    end);

    if (index > -1) then begin
        FProcedures[index] := obj;
    end
    else begin
        FProcedures.Add(obj);
    end;

end;

procedure TDatabase.AddOrSetTable(obj: TDBTable);
var
  index : integer;
begin
    index := FTables.IndexOf(function( o : TDBTable) : Boolean
    begin
        Result := (o.Name = obj.Name);
    end);

    if (index > -1) then begin
        FTables[index] := obj;
    end
    else begin
        FTables.Add(obj);
    end;

end;

procedure TDatabase.AddOrSetTrigger(obj: TDBTrigger);
var
  index : integer;
begin
    index := FTriggers.IndexOf(function( o : TDBTrigger) : Boolean
    begin
        Result := (o.Name = obj.Name);
    end);

    if (index > -1) then begin
        FTriggers[index] := obj;
    end
    else begin
        FTriggers.Add(obj);
    end;
end;

procedure TDatabase.AddOrSetView(obj: TDBView);
var
  index : integer;
begin
    index := FViews.IndexOf(function( o : TDBView) : Boolean
    begin
        Result := (o.Name = obj.Name);
    end);

    if (index > -1) then begin
        FViews[index] := obj;
    end
    else begin
        FViews.Add(obj);
    end;

end;

procedure TDatabase.AppendAditionalMigrationFields(tableIndex: integer;
  migrationFields: TList<TDBField>);
  var
  dbMigrationField : TDBField;
  index : integer;
begin
  for dbMigrationField in migrationFields do begin

     index := FTables[tableIndex].Fields.IndexOf(function(f : TDBField) : boolean
     begin
         result := f.Name = dbMigrationField.Name;
     end);

     if (index > -1) then begin //Encontrou
         FTables[tableIndex].Fields[index] := dbMigrationField;
     end
     else begin  //não existe
        FTables[tableIndex].Fields.Add(dbMigrationField);
     end;

  end;
end;

procedure TDatabase.AppendAditionalMigrationTables(
  migrationTables: TList<TDBTable>);
  var
   dbMigrationTable : TDBTable;
   index : integer;
begin
      for dbMigrationTable in migrationTables do begin

          index := FTables.IndexOf(function(T : TDBTable) : boolean
          begin
              result := t.Name = dbMigrationTable.Name;
          end);

          if (index > -1) then begin  //Econtrou a tabela
             AppendAditionalMigrationFields(index, dbMigrationTable.Fields);
             AppendAditionalMigrationTriggers(index,dbMigrationTable.Triggers );
          end
          else begin
             AddOrSetTable(dbMigrationTable);
          end;

      end;
end;

procedure TDatabase.AppendAditionalMigrationTriggers(tableIndex: integer;
  migrationTriggers: TList<TDBTrigger>);
  var
    index : integer;
    dbMigrationTrigger : TDBTrigger;
begin
  for dbMigrationTrigger in migrationTriggers do begin

     index := FTables[tableIndex].Triggers.IndexOf(function(t : TDBTrigger) : boolean
     begin
         result := t.Name = dbMigrationTrigger.Name;
     end);

     if (index > -1) then begin //Encontrou
         FTables[tableIndex].Triggers[index] := dbMigrationTrigger;
     end
     else begin  //não existe
        FTables[tableIndex].Triggers.Add(dbMigrationTrigger);
     end;

  end;
end;

constructor TDatabase.Create(AConnection : TFDConnection);
begin
   FConnection := AConnection;

   FQueryTables := CreateQuery;
   FQueryPK := CreateQuery;
   FQueryFK := CreateQuery;
   FQueryCheck := CreateQuery;
   FQueryUnique := CreateQuery;
   FQueryIndex := CreateQuery;
   FQueryTrigger := CreateQuery;
   FQueryGenerator := CreateQuery;
   FQueryFunctions := CreateQuery;
   FQueryFields := CreateQuery;

   FAditionalMigrations := TList<TMigration>.Create;
end;

function TDatabase.CreateQuery: TFDQuery;
begin
   result := TFDQuery.Create(nil);
   with Result do begin
      Connection := FConnection;
      OptionsIntf.FetchOptions.Unidirectional := True;
      FetchOptions.Unidirectional := true;
      FetchOptions.RowsetSize := 500;
      FetchOptions.Mode := fmOnDemand;
   end;
end;

function TDatabase.GetAllGenerators: TList<TDBGenerator>;
begin
  Result := FGenerators;
end;

function TDatabase.GetCheckConstraints(
  aTableName: string): TList<TDBCheck>;
  var
  sql : string;
  vCheck : TDBCheck;
begin
  result := TList<TDBCheck>.Create;
  sql := TSqlResources.Read('QUERY_CKECK_SQL');
  FQueryCheck.Open(sql,[aTableName]);
  while not FQueryCheck.Eof do
  begin
     vCheck := TDBCheck.Create(FQueryCheck.FieldByName('CHECK_NAME').AsString);
     vCheck.TableName :=  aTableName.ToUpper();
     vCheck.Source :=  FQueryCheck.FieldByName('CHECK_SOURCE').AsString;

     Result.Add(vCheck);
     FQueryCheck.Next;
  end;

end;

function TDatabase.GetFieldList(aViewName: string): TStringList;
VAR
query : TFDQuery;
begin
  try
    try
      Result := TStringList.Create;
      query := TFDQuery.Create(nil);
      query.Connection := FConnection;

      query.SQL.Text := SqlResources.TSqlResources.Read('QUERY_FIELD_LIST_SQL');
      query.Params.ParamByName('VIEW_NAME').AsString := aViewName;
      query.Open;

      while not query.Eof do
      begin
         Result.Add(query.FieldByName('field_name').AsString);
         query.Next;
      end;

    except on e: exception do begin
       raise Exception.Create(e.Message);
    end;
    end;
  finally
    if Assigned(query) then
       FreeAndNil(query);
  end;
end;




function TDatabase.GetFields(aTableName: string): TList<TDBField>;
var
 query : TFDQuery;
 vField : TDBField;
 sql : string;
begin
  try
    try
      Result := TList<TDBField>.Create;
      query := TFDQuery.Create(nil);
      query.Connection := FConnection;
      sql := TSqlResources.Read('QUERY_FIELDS_SQL');

      query.Open(sql,[aTableName]);

      while not query.Eof do
      begin

        vField := TDBField.Create(query.FieldByName('FIELD_NAME').AsString,aTableName);
        with vField do begin
           DomainName   := query.FieldByName('FIELD_SOURCE').AsString;
           FieldType    := query.FieldByName('FIELD_TYPE').AsString;
           NotNull      :=  query.FieldByName('FIELD_NULL').AsString = 'NOT NULL';
           Charset      := query.FieldByName('FIELD_CHARSET').AsString;
           Collate      :=  query.FieldByName('FIELD_COLLATION').AsString;
           DefaultValue := query.FieldByName('FIELD_DEFAULT').AsString;
        end;

        Result.Add(vField);

        query.Next;
      end;

    except on e: exception do begin
       raise Exception.Create(e.Message);
    end;
    end;
  finally
    if Assigned(query) then
       FreeAndNil(query);
  end;

end;

function TDatabase.GetForeignKeys(
  aTableName: string): TList<TDBForeignKey>;
  var
  sql : string;
  vFK : TDBForeignKey;
begin
 Result := TList<TDBForeignKey>.create;
 FQueryFK.SQL.Text := TSqlResources.Read('QUERY_FK_SQL');
 FQueryFK.ParamByName('TABLE_NAME').AsString := ATableName;
 FQueryFK.Open;
 while not FQueryFK.Eof do
 begin
    vFK := TDBForeignKey.Create(FQueryFK.FieldByName('FK_NAME').AsString);
    vFK.TableName := aTableName.ToUpper();
    vFK.OnFields := FQueryFK.FieldByName('FIELD_NAME').AsString;
    vFK.FKTable :=  FQueryFK.FieldByName('REF_TABLE_NAME').AsString;
    vFK.FKField :=  FQueryFK.FieldByName('REF_FIELD_NAME').AsString;
    vFK.IndexName := FQueryFK.FieldByName('INDEX_NAME').AsString;

    Result.Add(vFK);

    FQueryFK.Next;
 end;

end;



function TDatabase.GetIndices: TList<TDBIndex>;
begin
  Result := FIndices;
end;

function TDatabase.GetIndicesFromTable(aTableName: string): TList<TDBIndex>;
var
  vIndex : TDBIndex;
begin
   Result := TList<TDBIndex>.Create();
   FQueryIndex.SQL.Text := TSqlResources.Read('QUERY_INDEX_SQL');
   FQueryIndex.Params.ParamByName('TABLE_NAME').AsString := aTableName;
   FQueryIndex.Open;
   while not FQueryIndex.Eof do
   begin
      vIndex := TDBIndex.Create(FQueryIndex.FieldByName('INDEX_NAME').AsString);
      vIndex.TableName := aTableName.ToUpper();
      vIndex.OnFields := FQueryIndex.FieldByName('FIELDS').AsString;
      vIndex.Expression := FQueryIndex.FieldByName('EXPRESSION').AsString;
      vIndex.Unique := FQueryIndex.FieldByName('IS_UNIQUE').AsString = 'S';
      vIndex.Active  := FQueryIndex.FieldByName('IS_ACTIVE').AsString = 'S';
      vIndex.Sorting := FQueryIndex.FieldByName('SORTING').AsString;

      Result.Add(vIndex);

      FQueryIndex.Next;
   end;

end;

function TDatabase.GetPrimaryKeys(aTableName: string): TList<TDBPrimaryKey>;
var
 vIndex : TDBPrimaryKey;
begin
  Result := TList<TDBPrimaryKey>.Create();
  FQueryPK.SQL.Text := SqlResources.TSqlResources.Read('QUERY_PRIMARY_KEYS_SQL');
  FQueryPK.Params.ParamByName('TABLE_NAME').AsString := aTableName;
  FQueryPK.Open();

  while not FQueryPK.Eof do begin
    vIndex := TDBPrimaryKey.Create(FQueryPK.FieldByName('NAME').AsString);
    vIndex.TableName := aTableName.ToUpper();
    vIndex.OnFields := FQueryPK.FieldByName('FIELDS').AsString;
    vIndex.IndexName := FQueryPK.FieldByName('INDEX_NAME').AsString;
    vIndex.IndexSorting := FQueryPK.FieldByName('SORTING').AsString;

    result.Add(vIndex);
    FQueryPK.Next;
  end;

end;


procedure TDatabase.GetFunctionFields(aFunctionName: string;
  var aInputFields: TList<TDBField>; var aOutputFieldType: string);
var
  sql : string;
  vField : TDBField;
begin
   try
      aInputFields := TList<TDBField>.Create;
      aOutputFieldType := '';
      sql := TSqlResources.Read('QUERY_FUNCTION_FIELDS_SQL');

      FQueryFields.Open(sql,[afunctionName]);


      while not FQueryFields.Eof do
      begin

         if (FQueryFields.FieldByName('FIELD_NAME').AsString <> '') then
         begin

            vField := TDBField.Create(FQueryFields.FieldByName('FIELD_NAME').AsString,'');
            with vField do begin
               FieldType    := FQueryFields.FieldByName('FIELD_TYPE').AsString;
               NotNull      :=  FQueryFields.FieldByName('FIELD_NULL').AsString = 'NOT NULL';
               Charset      := FQueryFields.FieldByName('FIELD_CHARSET').AsString;
               Collate      :=  FQueryFields.FieldByName('FIELD_COLLATION').AsString;
               DefaultValue := FQueryFields.FieldByName('FIELD_DEFAULT').AsString;
            end;

            if (FQueryFields.FieldByName('PARAMETER_DIRECTION').AsString = 'INPUT') then
                aInputFields.Add(vField);
         end
         else aOutputFieldType :=  FQueryFields.FieldByName('FIELD_TYPE').AsString;

        FQueryFields.Next;
      end;

    except on e: exception do begin
       raise Exception.Create(e.Message);
    end;
    end;
end;


function TDatabase.GetFunctions: TList<TDBFunction>;
begin
   Result := FFunctions;
end;

function TDatabase.GetGenerators: TList<TDBGenerator>;
begin
   result := FGeneratorsWithoutDeps;
end;

procedure TDatabase.GetProcedureFields(aProcedureName: string;
  var aInputFields, aOutputFields: TList<TDBField>);
  var
  sql : string;
  vField : TDBField;
begin
    try
      aInputFields := TList<TDBField>.Create;
      aOutputFields := TList<TDBField>.Create;
      sql := TSqlResources.Read('QUERY_PROCEDURE_FIELDS_SQL');

      FQueryFields.Open(sql,[aProcedureName]);


      while not FQueryFields.Eof do
      begin

        vField := TDBField.Create(FQueryFields.FieldByName('FIELD_NAME').AsString,aProcedureName.ToUpper);
        with vField do begin
           FieldType    := FQueryFields.FieldByName('FIELD_TYPE').AsString;
           NotNull      :=  FQueryFields.FieldByName('FIELD_NULL').AsString = 'NOT NULL';
           Charset      := FQueryFields.FieldByName('FIELD_CHARSET').AsString;
           Collate      :=  FQueryFields.FieldByName('FIELD_COLLATION').AsString;
           DefaultValue := FQueryFields.FieldByName('FIELD_DEFAULT').AsString;
        end;

        if (FQueryFields.FieldByName('PARAMETER_DIRECTION').AsString = 'INPUT') then
            aInputFields.Add(vField)
        ELSE aOutputFields.Add(vField);


        FQueryFields.Next;
      end;

    except on e: exception do begin
       raise Exception.Create(e.Message);
    end;
    end;
end;

function TDatabase.GetProcedures: TList<TDBProcedure>;
begin
  result := FProcedures;
end;

function TDatabase.GetTables: TList<TDBTable>;
begin
   Result := FTables;
end;

function TDatabase.GetTriggers: TList<TDBTrigger>;
begin
  Result := FTriggers;
end;

function TDatabase.GetUniqueConstraints(
  aTableName: string): TList<TDBUnique>;
  var
  sql : string;
  vUnique : TDBUnique;
begin
 Result := TList<TDBUnique>.create;
 FQueryFK.SQL.Text := TSqlResources.Read('QUERY_UNIQUE_SQL');
 FQueryFK.ParamByName('TABLE_NAME').AsString := ATableName;
 FQueryFK.Open;
 while not FQueryFK.Eof do
 begin
    vUnique := TDBUnique.Create(FQueryFK.FieldByName('UNIQUE_NAME').AsString);
    vUnique.TableName := aTableName.ToUpper();
    vUnique.OnFields := FQueryFK.FieldByName('FIELDS_NAME').AsString;
    vUnique.IndexName := FQueryFK.FieldByName('INDEX_NAME').AsString;

    vUnique.IndexSorting := FQueryFK.FieldByName('SORTING').AsString;

    Result.Add(vUnique);

    FQueryFK.Next;
 end;

end;

function TDatabase.GetViews: TList<TDBView>;
begin
  Result := FViews;
end;

procedure TDatabase.LoadMetadata;
var
 i : integer;
begin
try
   try
       FConnection.Connected := true;
       LoadTriggers;
       LoadTablesAndViews();
       LoadProcedures;
       LoadFunctions;
       LoadGeneratorsWithoutDependencies;
       LoadGenerators;
       LoadIncrementalMigrations;

   except
       raise;
   end;
finally
   FConnection.Connected := false;
end;


end;



procedure TDatabase.LoadGenerators;
var
 vGenerator : TDBGenerator;
begin
  FGenerators := TList<TDBGenerator>.create;
  FQueryGenerator.SQL.Text := 'select rdb$generator_name as generator_name from rdb$generators where rdb$system_flag = 0;';
  FQueryGenerator.Open;
  while not FQueryGenerator.Eof do
  begin
    if (TDebugFilter.Listar(FQueryGenerator.FieldByName('generator_name').AsString)) then begin
        vGenerator := TDBGenerator.Create(FQueryGenerator.FieldByName('generator_name').AsString);
        FGenerators.Add(vGenerator);
    end;

    FQueryGenerator.Next;
  end;
end;

procedure TDatabase.LoadGeneratorsWithoutDependencies;
var
 vGenerator : TDBGenerator;
begin
  FGeneratorsWithoutDeps := TList<TDBGenerator>.create;
  FQueryGenerator.SQL.Text := TSqlResources.Read('QUERY_GENERATORS_WITHOUT_DEPS_SQL');
  FQueryGenerator.Open;
  while not FQueryGenerator.Eof do
  begin
    if (TDebugFilter.Listar(FQueryGenerator.FieldByName('generator_name').AsString)) then begin
        vGenerator := TDBGenerator.Create(FQueryGenerator.FieldByName('generator_name').AsString);

        FGeneratorsWithoutDeps.Add(vGenerator);
    end;

    FQueryGenerator.Next;
  end;


end;

procedure TDatabase.LoadIncrementalMigrations;
var
   migration : TMigration;
   dbMigrationTable : TDBTable;
   dbMigrationField : TDBField;
   dbMigrationTrigger : TDBTrigger;
   tbIndex,flIndex, trIndex : integer;
begin


  for migration in FAditionalMigrations do begin
      migration.CreateMigrations;

      AppendAditionalMigrationTables(migration.Tables);
  end;




end;

procedure TDatabase.LoadFunctions;
const sql : string = 'select trim(p.rdb$function_name) as name, p.rdb$function_source as source from rdb$functions p where p.rdb$system_flag = 0';
var
 vFunction : TDBFunction;
  InputFields : TList<TDBField>;
  vReturnType : string;
begin
  FFunctions := TList<TDBFunction>.create;
  FQueryFunctions.Open(sql);
  while not FQueryFunctions.Eof do
  begin
    if (TDebugFilter.Listar(FQueryFunctions.FieldByName('name').AsString)) then begin
        vFunction := TDBFunction.Create(FQueryFunctions.FieldByName('name').AsString);
        vFunction.FunctionSource := FQueryFunctions.FieldByName('source').AsString;
        GetFunctionFields(vFunction.Name, InputFields, vReturnType );
        vFunction.InputFields := InputFields;
        vFunction.ReturnType := vReturnType;

        FFunctions.Add(vFunction);
    end;


    FQueryFunctions.Next;
  end;


end;



procedure TDatabase.LoadProcedures;
var
 vProcedure : TDBProcedure;
 InputFields : TList<TDBField>;
 outputFields : TList<TDBField>;

begin
  FProcedures := TList<TDBProcedure>.create;
  FQueryFunctions.SQL.Text := SqlResources.TSqlResources.Read('QUERY_PROCEDURES_SQL');
  FQueryFunctions.Open();
  while not FQueryFunctions.Eof do
  begin
    if (TDebugFilter.Listar(FQueryFunctions.FieldByName('name').AsString)) then begin


      vProcedure := TDBProcedure.Create(FQueryFunctions.FieldByName('name').AsString);
      vProcedure.ProcedureSource := FQueryFunctions.FieldByName('source').AsString;
      GetProcedureFields(vProcedure.Name, InputFields,outputFields);

      vProcedure.InputFields := InputFields;
      vProcedure.OutputFields := outputFields;

      FProcedures.Add(vProcedure);

    end;


    FQueryFunctions.Next;
  end;


end;

procedure TDatabase.LoadTablesAndViews(aWhere : string = '');
var
 name, source : string;
 isTable : boolean;
 vTable : TDBTable;
 vView : TDBView;
begin
  if (aWhere <> '') then
      aWhere := ' AND '+aWhere;

   FQueryTables.SQL.Text := TSqlResources.Read('QUERY_TABLES_AND_VIEWS_SQL');
   FQueryTables.SQL.Add(aWhere);
   FQueryTables.Open();



   FTables := TList<TDBTable>.Create;
   FViews := TList<TDBView>.Create;

   while not FQueryTables.Eof do
   begin
     name :=  FQueryTables.FieldByName('NAME').AsString;
     isTable := FQueryTables.FieldByName('SOURCE').IsNull;

     if (TDebugFilter.Listar(name)) then begin
       if (isTable) then begin
          vTable := TDBTable.Create(name);
          vTable.Fields := GetFields(name);
          vTable.PrimaryKeys := GetPrimaryKeys(name);
          vTable.ForeignKeys := GetForeignKeys(name);
          vTable.CheckContrainsts := GetCheckConstraints(name);
          vTable.UniqueConstraints := GetUniqueConstraints(name);
          vTable.Indices := GetIndicesFromTable(name);
          vTable.Triggers := FTriggers.Where(function(t : TDBTrigger) : boolean
          begin
             result := t.TableName = name;
          end);

          FTables.Add(vTable);

       end
       else begin
          vView := TDBView.Create(name);
          vView.FieldList := GetFieldList(name);
          vView.ViewSource :=  FQueryTables.FieldByName('SOURCE').AsString;

          FViews.Add(vView);

       end;
     end;


     FQueryTables.Next;
   end;

end;

procedure TDatabase.LoadTriggers;
var
 sql : string;
 vTrigger : TDBTrigger;
begin
  FTriggers := TList<TDBTrigger>.Create();
  FQueryTrigger.SQL.Text := TSqlResources.Read('QUERY_TRIGGER_SQL');
  FQueryTrigger.Open();
  while not FQueryTrigger.Eof do
  begin
     if (TDebugFilter.Listar(FQueryTrigger.FieldByName('TRIGGER_NAME').AsString)) then begin
        vTrigger := TDBTrigger.Create(FQueryTrigger.FieldByName('TRIGGER_NAME').AsString);
        vTrigger.TableName := FQueryTrigger.FieldByName('TABLE_NAME').AsString;
        vTrigger.TriggerSource := FQueryTrigger.FieldByName('TRIGGER_SOURCE').AsString;
        vTrigger.TriggerType :=  TTriggerType(FQueryTrigger.FieldByName('TRIGGER_TYPE').asInteger);
        vTrigger.TriggerPosition := FQueryTrigger.FieldByName('TRIGGER_POSITION').AsInteger;
        vTrigger.IsActive := FQueryTrigger.FieldByName('IS_ACTIVE').AsString = 'S';

        FTriggers.Add(vTrigger);
     end;

    FQueryTrigger.Next;
  end;

end;

end.

