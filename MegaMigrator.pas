unit MegaMigrator;

interface

 uses System.classes,FireDAC.Comp.Client,Model.DBView,  System.SysUtils, DBSystemTables, ClipBrd,
 Model.DBTable,Model.DBProcedure, Model.DBGenerator, Model.DBFunction,System.IOUtils,Migration.ClassWriter, Model.DBObject, DCollections;

 const CLASS_PATH = 'C:\Fontes\Labs\lab-migration\_migrations';

 type TMegaMigration = class

 private
   Connection : TFDConnection;
   SystemTables : TDBSystemTables;

 public
    procedure Migrate;
    function GenerateScript : string;
    procedure SaveClasses;

    constructor Create(AOwner: TComponent; AConnection : TFDConnection );
 end;


implementation

{ TMegaMigration }

constructor TMegaMigration.Create(AOwner: TComponent; AConnection : TFDConnection );
begin
   Connection  := AConnection;
   SystemTables := TDBSystemTables.Create(AConnection);


end;

function TMegaMigration.GenerateScript: string;
var
  str : string;
  Stream : TFileStream;

begin
  SystemTables.Load;


  str :=  SystemTables.Tables.First(function(table : TDBTable) : boolean
  begin
     result :=  table.Name = 'NFCE';
  end).DDLCreate;

//  str :=  SystemTables.Views.First(function(table : TDBView) : boolean
//  begin
//     result := TRUE;// table.Name = 'TESTANDO';
//  end).DDLCreate;

//  str :=  SystemTables.Functions.First(function(table : TDBFunction) : boolean
//  begin
//     result := TRUE;// table.Name = 'TESTANDO';
//  end).DDLCreate;



   TFile.WriteAllText('C:\SQL\SQL.sql', str, TEncoding.Default);



  result := str;

end;

procedure TMegaMigration.Migrate;
var
 str : string;
begin
  SystemTables.Load;


  str :=  SystemTables.Tables.First(function(table : TDBTable) : boolean
  begin
     result := table.Name = 'TESTANDO';
  end).DDLCreate;

  Clipboard.AsText := str;


end;

procedure TMegaMigration.SaveClasses;
var
   Writer : TMigrationClassWriter;
begin
  Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBTable>( SystemTables.Tables );

  Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBView>( SystemTables.Views );

   Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBProcedure>( SystemTables.Procedures );

   Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBFunction>( SystemTables.Functions );

   Writer := TMigrationClassWriter.Create(CLASS_PATH);
  Writer.SavePas<TDBGenerator>( SystemTables.Generators );
end;

end.
