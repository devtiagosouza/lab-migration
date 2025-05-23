unit Database;

interface
  uses Model.DBObject, Model.DBTable, Model.DBField, Model.DBView, Model.DBProcedure, Model.DBFunction, Model.DBTrigger,
  Model.DBGenerator,Model.DBIndex, System.Classes,FireDAC.Comp.Client,SqlResources,
  System.SysUtils, DCollections, Database.Interfaces,FireDAC.Stan.Option;


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
    FGenerators: TList<TDBGenerator>;
    FIndices: TList<TDBIndex>;




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
    procedure LoadGenerators;

    function CreateQuery : TFDQuery;

public
    function GetTables: TList<TDBTable>;
    function GetViews: TList<TDBView>;
    function GetProcedures: TList<TDBProcedure>;
    function GetFunctions: TList<TDBFunction>;
    function GetTriggers: TList<TDBTrigger>;
    function GetGenerators: TList<TDBGenerator>;
    function GetIndices: TList<TDBIndex>;

    procedure LoadMetadata();



    constructor Create(AConnection : TFDConnection);

end;


implementation

{ TDatabase }

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
     vCheck := TDBCheck.Create;
     vCheck.TableName :=  aTableName.ToUpper();
     vCheck.Name :=  FQueryCheck.FieldByName('CHECK_NAME').AsString;
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

        vField := TDBField.Create;
        with vField do begin
           TableName    :=  aTableName.ToUpper;
           Name         := query.FieldByName('FIELD_NAME').AsString;
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
    vFK := TDBForeignKey.Create();
    vFK.TableName := aTableName.ToUpper();
    vFK.Name :=  FQueryFK.FieldByName('FK_NAME').AsString;
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
      vIndex := TDBIndex.Create();
      vIndex.TableName := aTableName.ToUpper();
      vIndex.Name :=  FQueryIndex.FieldByName('INDEX_NAME').AsString;
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
    vIndex := TDBPrimaryKey.Create;
    vIndex.Name := FQueryPK.FieldByName('NAME').AsString;
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

            vField := TDBField.Create;
            with vField do begin
               Name         := FQueryFields.FieldByName('FIELD_NAME').AsString;
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
   result := FGenerators;
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

        vField := TDBField.Create;
        with vField do begin
           TableName    := aProcedureName.ToUpper;
           Name         := FQueryFields.FieldByName('FIELD_NAME').AsString;
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

end;

function TDatabase.GetTables: TList<TDBTable>;
begin

end;

function TDatabase.GetTriggers: TList<TDBTrigger>;
begin

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
    vUnique := TDBUnique.Create();
    vUnique.TableName := aTableName.ToUpper();
    vUnique.Name :=  FQueryFK.FieldByName('UNIQUE_NAME').AsString;
    vUnique.OnFields := FQueryFK.FieldByName('FIELDS_NAME').AsString;
    vUnique.IndexName := FQueryFK.FieldByName('INDEX_NAME').AsString;

    vUnique.IndexSorting := FQueryFK.FieldByName('SORTING').AsString;

    Result.Add(vUnique);

    FQueryFK.Next;
 end;

end;

function TDatabase.GetViews: TList<TDBView>;
begin

end;

procedure TDatabase.LoadMetadata;
begin
try
   try
       FConnection.Connected := true;
       LoadGenerators;
       LoadTriggers;
       LoadTablesAndViews();
       LoadProcedures;
       LoadFunctions;
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
  FQueryGenerator.SQL.Text := TSqlResources.Read('QUERY_GENERATORS_SQL');
  FQueryGenerator.Open;
  while not FQueryGenerator.Eof do
  begin
    vGenerator := TDBGenerator.Create;
    vGenerator.Name := FQueryGenerator.FieldByName('name').AsString;

    FGenerators.Add(vGenerator);

    FQueryGenerator.Next;
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
    vFunction := TDBFunction.Create;
    vFunction.Name := FQueryFunctions.FieldByName('name').AsString;
    vFunction.FunctionSource := FQueryFunctions.FieldByName('source').AsString;
    GetFunctionFields(vFunction.Name, InputFields, vReturnType );
    vFunction.InputFields := InputFields;
    vFunction.ReturnType := vReturnType;

    FFunctions.Add(vFunction);

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
    vProcedure := TDBProcedure.Create;
    vProcedure.Name := FQueryFunctions.FieldByName('name').AsString;
    vProcedure.ProcedureSource := FQueryFunctions.FieldByName('source').AsString;
    GetProcedureFields(vProcedure.Name, InputFields,outputFields);

    vProcedure.InputFields := InputFields;
    vProcedure.OutputFields := outputFields;

    FProcedures.Add(vProcedure);

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

     if (isTable) then begin
        vTable := TDBTable.Create;
        vTable.Name := name;
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
        vView := TDBView.Create();
        vView.Name := name;
        vView.FieldList := GetFieldList(name);
        vView.ViewSource :=  FQueryTables.FieldByName('SOURCE').AsString;

        FViews.Add(vView);

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
    vTrigger := TDBTrigger.Create;
    vTrigger.Name := FQueryTrigger.FieldByName('TRIGGER_NAME').AsString;
    vTrigger.TableName := FQueryTrigger.FieldByName('TABLE_NAME').AsString;
    vTrigger.TriggerSource := FQueryTrigger.FieldByName('TRIGGER_SOURCE').AsString;
    vTrigger.TriggerType :=  TTriggerType(FQueryTrigger.FieldByName('TRIGGER_TYPE').asInteger);
    vTrigger.TriggerPosition := FQueryTrigger.FieldByName('TRIGGER_POSITION').AsInteger;
    vTrigger.IsActive := FQueryTrigger.FieldByName('IS_ACTIVE').AsString = 'S';



    FTriggers.Add(vTrigger);

    FQueryTrigger.Next;
  end;

end;

end.

