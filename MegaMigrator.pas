unit MegaMigrator;

interface

 uses System.classes,FireDAC.Comp.Client,Model.DBView, Model.DBFunction,  System.SysUtils, DBSystemTables, ClipBrd,
 Model.DBTable,System.IOUtils,Migration.ClassWriter, Model.DBObject, DCollections;

 type TMegaMigration = class

 private
   Connection : TFDConnection;
   SystemTables : TDBSystemTables;
   Writer : TMigrationClassWriter;
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
   Writer := TMigrationClassWriter.Create('C:\Fontes\Labs\lab-migration\_migrations');;

end;

function TMegaMigration.GenerateScript: string;
var
 str : string;
  Stream : TFileStream;

begin
  SystemTables.Load;


  str :=  SystemTables.Tables.First(function(table : TDBTable) : boolean
  begin
     result :=  table.Name = 'ABASTECIMENTO';
  end).DDLCreate;

//  str :=  SystemTables.Views.First(function(table : TDBView) : boolean
//  begin
//     result := TRUE;// table.Name = 'TESTANDO';
//  end).DDLCreate;

//  str :=  SystemTables.Functions.First(function(table : TDBFunction) : boolean
//  begin
//     result := TRUE;// table.Name = 'TESTANDO';
//  end).DDLCreate;

  // TFile.WriteAllText('C:\SQL\SQL.sql', str, TEncoding.Default);


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
begin
  Writer.SavePas<TDBTable>( SystemTables.Tables );
end;

end.
