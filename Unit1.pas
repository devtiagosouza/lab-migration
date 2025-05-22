unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Stan.Def,
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
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    lbTempo: TLabel;
    Button3: TButton;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
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
var tempo: string;
begin
 TTaskTimer.Execute(procedure()
 begin
    FDConnection1.Open();
    MegaMigrator := TMegaMigration.Create(self, FDConnection1);
    Memo1.Lines.Text := MegaMigrator.GenerateScript;
    ShowMessage('Concluido');
 end,tempo);

 lbTempo.Caption := 'Tempo: '+tempo;


end;

procedure TForm1.Button2Click(Sender: TObject);
var
 splitter : TCommandSplitter;
 sql : string;
 comandos : TList<TDDLCommand>;
  comando : TDDLCommand;
  objectType: string;
  commandType: string;
  ddlType : string;
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

    ddlType := GetEnumName(TypeInfo(TDDLType),
    integer(comando.DDLType));

//     Memo1.Lines.Add('Name: '+comando.ObjectName+' | Command Type: '+commandType+' | Object Type: '+objectType+' | DDL Type: '+ddlType
//     //+' | Text: '+comando.CommandText
//     );

      Memo1.Lines.Add('/*** '+ddlType+' ***/');
      Memo1.Lines.Add(comando.CommandText+sLineBreak);

  end;

end;

procedure TForm1.Button3Click(Sender: TObject);
var tempo: string;
begin
 TTaskTimer.Execute(procedure()
 begin
    FDConnection1.Open();
    MegaMigrator := TMegaMigration.Create(self, FDConnection1);
    Memo1.Lines.Text := MegaMigrator.GenerateScript;
    MegaMigrator.SaveClasses;
    ShowMessage('Concluido');
 end,tempo);

 lbTempo.Caption := 'Tempo: '+tempo;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
 parser : TViewParser;
 procParser : TProcedureParser;
 funcParser : TFunctionParser;
 triggerParser : TTriggerParser;
 sql : string;
 view : TDBView;
 proc : TDBProcedure;
 func : TDBFunction;
 trigger : TDBTrigger;
 splitter : TCommandSplitter;
 comm : TDDLCommand;
begin
  parser := TViewParser.Create;
  procParser := TProcedureParser.Create;
  funcParser := TFunctionParser.Create;
  triggerParser := TTriggerParser.Create;

  splitter := TCommandSplitter.Create;


  sql :=  SqlResources.TSqlResources.Read('SQL_sql');


  Memo1.Clear;
  Memo2.Clear;

  Memo1.Text := sql;

  for comm in splitter.Split(sql) do begin
     trigger := triggerParser.Parse(comm.commandText);
     Memo2.Lines.Add(trigger.DDLCreate);
  end;

end;

procedure TForm1.Button5Click(Sender: TObject);
var
 Writer: TDelphiUnitWriter;
begin
  Writer := TDelphiUnitWriter.Create;
  try
    Writer.UnitName := 'Migrations.Tabelas';
    Writer.ClassName := 'TMigrationTables';
    Writer.UsesDeclaration := 'System.SysUtils, System.Classes, Model.DBTable, Migration';
    Writer.ConstructorBody := '';
    Writer.MainClass := 'TMigration';


    Writer.AddMethod(mehodProcedure,'tabela_ABASTECIMENTO',
        '//Adição de script'+sLineBreak+
        'AddScript('+QuotedStr('')+');'
        );

    Writer.SaveUnitFile('C:\Fontes\Labs\lab-migration\_migrations');
    ShowMessage('Salvo com sucesso');
  finally
    Writer.Free;
  end;
end;



procedure TForm1.Button6Click(Sender: TObject);
 var
 sql : string;
   Match: TMatch;
   a : string;
  Regex: TRegEx;
  Generators: TDictionary<string, Boolean>;  // Usando um dicionário para filtrar duplicados
begin
 Generators := TDictionary<string, Boolean>.Create;
   sql :=  SqlResources.TSqlResources.Read('SQL_sql');
    Regex := TRegEx.Create('GEN_ID\((\w+),\d+\)');

    Memo1.Clear;
    for Match in Regex.Matches(sql) do
    begin
      Generators.AddOrSetValue(Match.Groups[1].Value, True);  // Adiciona o nome do generator ao dicionário
    end;

      for a in Generators.Keys do
          Memo1.Lines.Add(a);  // Exibe o nome do generator sem repetições

//     for Match in Regex.Matches(sql) do
//        Memo1.Lines.Add(Match.Groups[1].Value);  // Exibe o nome do generator encontrado

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 lbTempo.Caption := '';
end;

end.
