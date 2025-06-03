unit MegaMigrator;

interface

 uses System.classes,FireDAC.Comp.Client,Model.DBView,  System.SysUtils, Database, Database.Interfaces, ClipBrd,
 Model.DBTable,Model.DBProcedure, Model.DBGenerator, Model.DBFunction,System.IOUtils,Migration.ClassWriter, 
 Model.DBObject,Model.DBTrigger, DCollections, IWSystem,SqlResources,FireDAC.Stan.Option, Sql.Script.Builder,
 System.Rtti, System.TypInfo,Migration;

 const CLASS_PATH = 'C:\Fontes\Labs\lab-migration\_migrations';

 type TMegaMigration = class

 private
   FTargetDatabasePath : string;

   TargetConnection : TFDConnection;
   ModelConnection : TFDConnection;

   TargetDatabase : IDatabase;
   ModelDatabase : IDatabase;

   FMigrations : TList<TMigration>;


   function CreateConnection(ADatabasePath : string) : TFDConnection;
   function CreateModelConnection() : TFDConnection;

   procedure AddEqualityScript<T : TDBObject>(AScript : IScriptBuilder; AModelList, ATargetList : TList<T>);
   procedure AddEqualityScriptTable(AScript : IScriptBuilder; AModelList, ATargetList : TList<TDBTable>);




 public
    procedure Migrate;
    function GenerateScript : string;
    procedure SaveClasses;

    procedure AddMigration(aMigration : TMigration);

    constructor Create(ADatabasePath : string);
 end;


implementation

{ TMegaMigration }


constructor TMegaMigration.Create(ADatabasePath : string);
begin
   FTargetDatabasePath := ADatabasePath;

   TargetConnection  := CreateConnection(FTargetDatabasePath);
   ModelConnection := CreateModelConnection;
   
   TargetDatabase := TDatabase.Create(TargetConnection);
   ModelDatabase := TDatabase.Create(ModelConnection);

   FMigrations := TList<TMigration>.Create;
end;

function TMegaMigration.CreateConnection(ADatabasePath: string): TFDConnection;
begin
   Result := TFDConnection.Create(nil);
   with Result do begin
       DriverName := 'FB';
       Params.DriverID := 'FB';
       Params.Database := ADatabasePath;
       Params.UserName := 'SYSDBA';
       Params.Password := 'masterkey';

       FetchOptions.AutoFetchAll := afDisable;
       TxOptions.AutoCommit := False;
   end;
end;

function TMegaMigration.CreateModelConnection: TFDConnection;
const MODEL_DB_NAME = 'MIGRATION_DB';
const MODEL_DB_FILE_NAME = '_migration.db';
var
 path : string;
 databasePath : string;
 targetDatabaseName : string;
begin
  targetDatabaseName := FTargetDatabasePath
                     .Replace('localhost:','',[rfReplaceall])
                     .Replace('LOCALHOST:','',[rfReplaceall])
                     .Replace('127.0.0.1','',[rfReplaceall]);



   path := 'C:\MFX\'; // gsAppPath;


   path := path+'_migration\';

   databasePath := path+MODEL_DB_FILE_NAME;
   if (FileExists(databasePath)) then
        FileDelete(databasePath);

   SqlResources.TSqlResources.SaveZipFile(MODEL_DB_NAME, path, MODEL_DB_FILE_NAME);

   Result := CreateConnection(databasePath);
end;

function TMegaMigration.GenerateScript: string;
var
  script : IScriptBuilder;
  i : integer;
begin
try
  try
    script := TScriptBuilder.Create;

    TargetDatabase.LoadMetadata;

    for I := 0 to FMigrations.Count - 1 do begin
       ModelDatabase.AddIncrementalMigration(FMigrations[i]);
    end;

    ModelDatabase.LoadMetadata;


    AddEqualityScriptTable(script, ModelDatabase.GetTables, TargetDatabase.GetTables);
    AddEqualityScript<TDBView>(script, ModelDatabase.GetViews, TargetDatabase.GetViews);
    AddEqualityScript<TDBProcedure>(script, ModelDatabase.GetProcedures, TargetDatabase.GetProcedures);
    AddEqualityScript<TDBFunction>(script, ModelDatabase.GetFunctions, TargetDatabase.GetFunctions);
    AddEqualityScript<TDBGenerator>(script, ModelDatabase.GetGenerators, TargetDatabase.GetGenerators);

    result := script.AsString;
  except
     raise
  end;
finally
   
end;

end;



procedure TMegaMigration.AddEqualityScriptTable(AScript: IScriptBuilder;
  AModelList, ATargetList: TList<TDBTable>);
var
 modelTable : TDBTable;
 targetTable : TDBTable;
 vGen : TDBGenerator;
begin
    for modelTable in AModelList do begin
       targetTable := ATargetList.First(function(obj : TDBTable) : boolean
       begin
           result := obj.Name = modelTable.Name;
       end);


       if (targetTable = nil) then begin
           AScript.AppendLine(modelTable.DDLCreate([TargetDatabase.GetAllGenerators]))
       end
       else begin
           AScript.AppendLine(modelTable.EqualityScript(targetTable,[TargetDatabase.GetAllGenerators]))
       end;
    end;
end;




procedure TMegaMigration.AddMigration(aMigration: TMigration);
begin
 FMigrations.Add(aMigration);
end;

procedure TMegaMigration.AddEqualityScript<T>(AScript : IScriptBuilder; AModelList,
  ATargetList: TList<T>);
var
 model : TDBObject;
 target : TDBObject;
 vGen : TDBGenerator;
begin
    for model in AModelList do begin
       target := ATargetList.First(function(obj : T) : boolean
       begin
           result := (obj as TDBObject).Name = model.Name;
       end);

       if (target = nil) then begin
          AScript.AppendLine(model.DDLCreate);
       end
       else begin
          AScript.AppendLine(model.EqualityScript(target,[]));
       end;
    end;
end;


procedure TMegaMigration.Migrate;
var
 script : string;
begin
  script := GenerateScript;
end;

procedure TMegaMigration.SaveClasses;
var
   Writer : TMigrationClassWriter;
begin
  Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBTable>( TargetDatabase.GetTables );

  Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBView>( TargetDatabase.GetViews );

   Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBProcedure>( TargetDatabase.GetProcedures );

   Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBFunction>( TargetDatabase.GetFunctions );

   Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBGenerator>( TargetDatabase.GetGenerators );
end;



end.

