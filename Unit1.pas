unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
   FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait,FireDAC.DApt,
  Data.DB, FireDAC.Comp.Client,MegaMigrator,
  Vcl.StdCtrls, Sql.Builder, ClipBrd,Sql.Script.Builder,DCollections,TypInfo, Vcl.ExtCtrls,Parser;

type
  TForm1 = class(TForm)
    FDConnection1: TFDConnection;
    Memo1: TMemo;
    Panel1: TPanel;
    Button3: TButton;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

uses SqlResources;

procedure TForm1.Button1Click(Sender: TObject);

begin
  FDConnection1.Open();
  MegaMigrator := TMegaMigration.Create(self, FDConnection1);
  Memo1.Lines.Text :=  MegaMigrator.GenerateScript;
  ShowMessage('Concluido');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
 parser : TParser;
 comandos : TList<TDDLCommand>;
 comando : TDDLCommand;
 sql : string;
  objectType: string;
  commandType: string;
begin
  parser := TParser.Create;
  sql := SqlResources.TSqlResources.Read('SQL_sql');
  comandos :=  parser.Parse(sql);

  Memo1.Clear;

  for comando in  comandos do begin
    objectType := GetEnumName(TypeInfo(TDBObjectType),
    integer(comando.ObjectType));

    commandType :=  GetEnumName(TypeInfo(TDDLCommandType),
    integer(comando.CommandType));

     Memo1.Lines.Add('Name: '+comando.ObjectName+' | Command Type: '+commandType+' | Object Type: '+objectType
     //+' | Text: '+comando.CommandText
     );

  end;

end;

end.
