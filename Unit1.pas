unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
   FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait,FireDAC.DApt,Model.DBField,
  Data.DB, FireDAC.Comp.Client,MegaMigrator, ClipBrd,Model.DBTable,
  Vcl.StdCtrls, Sql.Builder,Sql.Script.Builder,DCollections,TypInfo, Vcl.ExtCtrls,Splitters,
  Parser.Tables, Parser.Constraints, Parser.Indices, Model.DBObject;

type
  TForm1 = class(TForm)
    FDConnection1: TFDConnection;
    Memo1: TMemo;
    Panel1: TPanel;
    Button3: TButton;
    Button1: TButton;
    Memo2: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.Button2Click(Sender: TObject);
var
 splitter : TCommandSplitter;
 sql : string;
 comandos : TList<TDDLCommand>;
  comando : TDDLCommand;
  objectType: string;
  commandType: string;
begin
   splitter := TCommandSplitter.Create;
   sql :=  SqlResources.TSqlResources.Read('SQL_sql');
   comandos :=  splitter.Split(sql);

   Memo1.Clear;
   Memo2.Clear;
   Memo2.Lines.Text := sql;

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

procedure TForm1.Button3Click(Sender: TObject);
var
 splitter : TCommandSplitter;
 comandos : TList<TDDLCommand>;
 comando : TDDLCommand;
 sql : string;
  objectType: string;
  commandType: string;

  Tabela : TDBTable;
  obj : TDBObject;
begin
  splitter := TCommandSplitter.Create;
  sql :=  SqlResources.TSqlResources.Read('SQL_sql');
  comandos :=  splitter.Split(sql);

  comando := comandos.First(function(c : TDDLCommand) : boolean
  begin
     result := c.ObjectType = objPrimaryKey
  end);


  Memo1.Clear;


  Memo1.Lines.Text := sql;



  obj := TConstraintParser.Parse(comando.CommandText);

  Memo2.Clear;
  Memo2.Lines.Text := obj.DDLCreate;

//  for comando in  comandos do begin
//    objectType := GetEnumName(TypeInfo(TDBObjectType),
//    integer(comando.ObjectType));
//
//    commandType :=  GetEnumName(TypeInfo(TDDLCommandType),
//    integer(comando.CommandType));
//
//     Memo1.Lines.Add('Name: '+comando.ObjectName+' | Command Type: '+commandType+' | Object Type: '+objectType
//     //+' | Text: '+comando.CommandText
//     );
//
//  end;

end;

end.
