unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Stan.Def,WaitScreen,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
   FireDAC.VCLUI.Wait,FireDAC.DApt,Model.DBProcedure,
  Data.DB, FireDAC.Comp.Client,MegaMigrator, ClipBrd,Model.DBView,System.RegularExpressions,
  Vcl.StdCtrls, DCollections,TypInfo, Vcl.ExtCtrls,Splitters,
  Parser.Triggers,DelphiUnitWriter,TaskTimer, Model.DBTrigger,System.Generics.Collections, Parser.Procedures, Parser.Functions, Model.DBFunction, Parser.Views, Firebird.Types,
  FireDAC.Phys.Intf, FireDAC.Phys.FBDef;

type
  TForm1 = class(TForm)
    FDConnection1: TFDConnection;
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Memo2: TMemo;
    lbTempo: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    MegaMigrator : TMegaMigration;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses SqlResources, Migration.Tabelas;

procedure TForm1.Button1Click(Sender: TObject);
var tempo: string;
begin

    TTaskTimer.Execute(procedure()
     begin

        Memo1.Lines.Text := MegaMigrator.GenerateScript;
     end,tempo);

     lbTempo.Caption := 'Tempo: '+tempo;
     ShowMessage('Concluido');

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 lbTempo.Caption := '';

  MegaMigrator := TMegaMigration.Create('localhost:C:\MFX\DADOS\TESTE_MIGRACAO\DEV_DB.FDB');
  MegaMigrator.AddMigration(TMigrationTabelas.Create);
end;

end.
