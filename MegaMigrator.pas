unit MegaMigrator;

interface

 uses System.classes,FireDAC.Comp.Client,  System.SysUtils, DBSystemTables, ClipBrd, Model.DBTable,System.IOUtils;

 type TMegaMigration = class

 private
   Connection : TFDConnection;
   SystemTables : TDBSystemTables;
 public
    procedure Migrate;
    function GenerateScript : string;

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
     result := TRUE;// table.Name = 'TESTANDO';
  end).CreateCommand;

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
  end).CreateCommand;

  Clipboard.AsText := str;


end;

end.
