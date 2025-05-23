unit MegaMigrator;

interface

 uses System.classes,FireDAC.Comp.Client,Model.DBView,  System.SysUtils, Database, Database.Interfaces, ClipBrd,
 Model.DBTable,Model.DBProcedure, Model.DBGenerator, Model.DBFunction,System.IOUtils,Migration.ClassWriter, 
 Model.DBObject, DCollections, IWSystem,SqlResources,FireDAC.Stan.Option;

 const CLASS_PATH = 'C:\Fontes\Labs\lab-migration\_migrations';

 type TMegaMigration = class

 private
   FTargetDatabasePath : string;

   TargetConnection : TFDConnection;
   ModelConnection : TFDConnection;
   TargetDatabase : IDatabase;
   ModelDatabase : IDatabase;

   function CreateConnection(ADatabasePath : string) : TFDConnection;
   function CreateModelConnection() : TFDConnection;

 public
    procedure Migrate;
    function GenerateScript : string;
    procedure SaveClasses;

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
const MODEL_DB_NAME = 'MODEL_DB';
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
   databasePath := path+MODEL_DB_FILE_NAME;
   if (FileExists(databasePath)) then
        FileDelete(databasePath);
   
   SqlResources.TSqlResources.SaveFile(MODEL_DB_NAME, path, MODEL_DB_FILE_NAME);

   Result := CreateConnection(databasePath);
end;

function TMegaMigration.GenerateScript: string;
var
  str : string;
  Stream : TFileStream;

begin
  TargetDatabase.LoadMetadata;
  ModelDatabase.LoadMetadata;

  result := str;

end;

procedure TMegaMigration.Migrate;
var
 str : string;
begin
  GenerateScript;


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

