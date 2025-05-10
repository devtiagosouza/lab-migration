unit DBSystemTables;

interface
  uses Model.DBObject, Model.DBTable, Model.DBField, Model.DBView, Model.DBProcedure, Model.DBFunction, Model.DBTrigger,
  Model.DBGenerator,Model.DBIndex, System.Classes,FireDAC.Comp.Client,SqlResources, System.SysUtils, DCollections;


type TDBSystemTables = class


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

    FTables: TList<TDBTable>;
    FDBView: TList<TDBView>;
    FProcedures: TList<TDBProcedure>;
    FFunctions: TList<TDBFunction>;
    FTriggers: TList<TDBTrigger>;
    FGenerators: TList<TDBGenerator>;
    FIndices: TList<TDBIndex>;


    procedure LoadTablesAndViews(aWhere : string = '');
    function GetFields(aTableName : string) : TList<TDBField>;
    function GetPrimaryKeys(aTableName: string) : TList<TDBPrimaryKey>;
    function GetForeignKeys(aTableName: string) : TList<TDBForeignKey>;
    function GetCheckConstraints(aTableName: string) : TList<TDBCheck>;
    function GetUniqueConstraints(aTableName: string) : TList<TDBUnique>;
    function GetIndices(aTableName: string) : TList<TDBIndex>;


    procedure LoadProcedures;
    procedure LoadFunctions;
    procedure LoadTriggers;
    procedure LoadGenerators;
   // procedure LoadIndices;

public
    property Tables : TList<TDBTable> read FTables write FTables;
    property Views : TList<TDBView> read FDBView write FDBView;
    property Procedures : TList<TDBProcedure> read FProcedures write FProcedures;
    property Functions : TList<TDBFunction>  read FFunctions write FFunctions;
    property Triggers : TList<TDBTrigger> read FTriggers write FTriggers;
    property Generators : TList<TDBGenerator> read FGenerators write FGenerators;
    property Indices : TList<TDBIndex> read FIndices write FIndices;



    procedure Load();



    constructor Create(AConnection : TFDConnection);

end;


implementation

{ TDBSystemTables }

constructor TDBSystemTables.Create(AConnection : TFDConnection);
begin
   FConnection := AConnection;

   FQueryTables := TFDQuery.Create(nil);
   FQueryPK := TFDQuery.Create(nil);
   FQueryFK := TFDQuery.Create(nil);
   FQueryCheck := TFDQuery.Create(nil);
   FQueryUnique := TFDQuery.Create(nil);
   FQueryIndex := TFDQuery.Create(nil);
   FQueryTrigger := TFDQuery.Create(nil);
   FQueryGenerator := TFDQuery.Create(nil);
   FQueryFunctions := TFDQuery.Create(nil);

   FQueryTables.Connection := AConnection;
   FQueryPK.Connection := AConnection;
   FQueryFK.Connection := AConnection;
   FQueryCheck.Connection := AConnection;
   FQueryUnique.Connection := AConnection;
   FQueryIndex.Connection := AConnection;
   FQueryTrigger.Connection := AConnection;
   FQueryGenerator.Connection := AConnection;
   FQueryFunctions.Connection := AConnection;
end;

function TDBSystemTables.GetCheckConstraints(
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

function TDBSystemTables.GetFields(aTableName: string): TList<TDBField>;
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
           Name         := query.FieldByName('FIELD_NAME').AsString;
           FieldType    := query.FieldByName('FIELD_TYPE').AsString;
           FieldSet     := query.FieldByName('FIELD_SET').AsString;
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

function TDBSystemTables.GetForeignKeys(
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

function TDBSystemTables.GetIndices(aTableName: string): TList<TDBIndex>;
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

function TDBSystemTables.GetPrimaryKeys(aTableName: string): TList<TDBPrimaryKey>;
const sql = 'SELECT RC.rdb$constraint_name AS NAME, SG.rdb$index_name AS INDEX_NAME, MAX(iif(coalesce(i.rdb$index_type,0) = 1,''DESCENDING'',''ASCENDING'' )) AS SORTING,  LIST(TRIM(sg.RDB$FIELD_NAME)) AS FIELDS  '+sLineBreak+
      ' FROM RDB$RELATION_CONSTRAINTS rc                                        '+sLineBreak+
      ' JOIN RDB$INDEX_SEGMENTS sg ON rc.RDB$INDEX_NAME = sg.RDB$INDEX_NAME     '+sLineBreak+
      ' join rdb$indices i on sg.RDB$INDEX_NAME = i.rdb$index_name              '+sLineBreak+
      ' WHERE rc.RDB$RELATION_NAME = :TABLE_NAME                                '+sLineBreak+
      ' AND rc.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY''                            '+sLineBreak+
      ' GROUP BY RC.rdb$constraint_name, SG.rdb$index_name                      ';
var
 vIndex : TDBPrimaryKey;
begin
  Result := TList<TDBPrimaryKey>.Create();
  FQueryPK.Open(sql);
  FQueryPK.Params.ParamByName('TABLE_NAME').AsString := aTableName;
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

function TDBSystemTables.GetUniqueConstraints(
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

procedure TDBSystemTables.Load;
begin
   LoadGenerators;
   LoadTriggers;
   LoadTablesAndViews('trim(t.rdb$relation_name) = '+QuotedStr('ABASTECIMENTO'));
   LoadProcedures;
   LoadFunctions;


 //  LoadIndices;

end;

procedure TDBSystemTables.LoadFunctions;
const sql : string = 'select p.rdb$function_name as name, p.rdb$function_source as source from rdb$functions p where p.rdb$system_flag = 0';
var
 vFunction : TDBFunction;
begin
  Functions := TList<TDBFunction>.create;
  FQueryFunctions.Open(sql);
  while not FQueryFunctions.Eof do
  begin
    vFunction := TDBFunction.Create;
    vFunction.Name := FQueryFunctions.FieldByName('name').AsString;
    vFunction.FunctionSource := FQueryFunctions.FieldByName('source').AsString;

    Functions.Add(vFunction);

    FQueryFunctions.Next;
  end;





end;

procedure TDBSystemTables.LoadGenerators;
var
 vGenerator : TDBGenerator;
begin
  Generators := TList<TDBGenerator>.create;
  FQueryGenerator.SQL.Text := TSqlResources.Read('QUERY_GENERATORS_SQL');
  FQueryGenerator.Open;
  while not FQueryGenerator.Eof do
  begin
    vGenerator := TDBGenerator.Create;
    vGenerator.Name := FQueryGenerator.FieldByName('name').AsString;
    vGenerator.InitialValue := FQueryGenerator.FieldByName('initial_value').Value;
    vGenerator.Increment :=  FQueryGenerator.FieldByName('increment').Value;
    vGenerator.TriggerName := FQueryGenerator.FieldByName('TRIGGER_NAME').AsString;

    Generators.Add(vGenerator);

    FQueryGenerator.Next;
  end;
end;

//procedure TDBSystemTables.LoadIndices;
//const sql : string = 'select  i.rdb$index_name as name,iif(RDB$INDEX_INACTIVE = 0,''S'',''N'') as ACTIVE, '+sLineBreak+
//'IIF(I.RDB$UNIQUE_FLAG = 0,''N'',''S'') as IS_UNIQUE, RDB$EXPRESSION_SOURCE AS EXPRESSION,      '+sLineBreak+
//'I.RDB$RELATION_NAME AS TABLE_NAME, i.RDB$FOREIGN_KEY as FOREIGN_KEY,   '+sLineBreak+
//'iif(coalesce(i.RDB$INDEX_TYPE,0) = 1,''desc'',''asc'') as SORTING     '+sLineBreak+
//'from rdb$indices i where i.rdb$system_flag = 0';
//var
// vIndex : TDBIndex;
//begin
//  Indices := TList<TDBIndex>.create;
//  FQuery.Open(sql);
//  while not FQuery.Eof do begin
//    vIndex := TDBIndex.Create;
//    vIndex.Name := FQuery.FieldByName('name').AsString;
//    vIndex.Expression := FQuery.FieldByName('EXPRESSION').AsString;
//    vIndex.Unique := FQuery.FieldByName('IS_UNIQUE').AsString = 'S';
//    vIndex.Active :=  FQuery.FieldByName('ACTIVE').AsString = 'S';
//    vIndex.Sorting := FQuery.FieldByName('SORTING').AsString;
//
//
//
//    Indices.Add(vIndex);
//    FQuery.Next;
//  end;
//
//
//end;

procedure TDBSystemTables.LoadProcedures;
const sql : string = 'select p.rdb$procedure_name as name, p.rdb$procedure_source as source from rdb$procedures p where p.rdb$system_flag = 0';
var
 vProcedure : TDBProcedure;
begin
  Procedures := TList<TDBProcedure>.create;
  FQueryFunctions.Open(sql);
  while not FQueryFunctions.Eof do
  begin
    vProcedure := TDBProcedure.Create;
    vProcedure.Name := FQueryFunctions.FieldByName('name').AsString;
    vProcedure.ProcedureSource := FQueryFunctions.FieldByName('source').AsString;

    Procedures.Add(vProcedure);

    FQueryFunctions.Next;
  end;


end;

procedure TDBSystemTables.LoadTablesAndViews(aWhere : string = '');
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



   Tables := TList<TDBTable>.Create;
   Views := TList<TDBView>.Create;

   while not FQueryTables.Eof do
   begin
     name :=  FQueryTables.FieldByName('NAME').AsString;
     isTable := FQueryTables.FieldByName('SOURCE').IsNull;

     if (isTable) then begin
        vTable := TDBTable.Create(nil);
        vTable.Name := name;
        vTable.Fields := GetFields(name);
        vTable.PrimaryKeys := GetPrimaryKeys(name);
        vTable.ForeignKeys := GetForeignKeys(name);
        vTable.CheckContrainsts := GetCheckConstraints(name);
        vTable.UniqueConstraints := GetUniqueConstraints(name);
        vTable.Indices := GetIndices(name);
        vTable.Triggers := Triggers.Where(function(t : TDBTrigger) : boolean
        begin
           result := t.TableName = name;
        end);

        Tables.Add(vTable);

     end
     else begin
        vView := TDBView.Create();
        vView.Name := name;
        vView.ViewSource :=  FQueryTables.FieldByName('SOURCE').AsString;

        Views.Add(vView);

     end;


     FQueryTables.Next;
   end;

end;

procedure TDBSystemTables.LoadTriggers;
var
 sql : string;
 vTrigger : TDBTrigger;
begin
  Triggers := TList<TDBTrigger>.Create();
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
    vTrigger.IsForGenerator := FQueryTrigger.FieldByName('IS_FOR_GENERATOR').AsString = 'S';
    vTrigger.IsActive := FQueryTrigger.FieldByName('IS_ACTIVE').AsString = 'S';

    vTrigger.Generators := Generators.Where(function(g : TDBGenerator) : boolean
    begin
       result := g.TriggerName = vTrigger.Name;
    end);


    Triggers.Add(vTrigger);

    FQueryTrigger.Next;
  end;

end;

end.
