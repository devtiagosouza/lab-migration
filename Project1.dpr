program Project1;

{$R *.dres}

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Model.DBField in 'Engine\Models\Model.DBField.pas',
  Model.DBTable in 'Engine\Models\Model.DBTable.pas',
  Model.DBObject in 'Engine\Models\Model.DBObject.pas',
  DCollections in 'DelphiCollectionsLib\DCollections.pas',
  Model.DBIndex in 'Engine\Models\Model.DBIndex.pas',
  Model.DBProcedure in 'Engine\Models\Model.DBProcedure.pas',
  Model.DBFunction in 'Engine\Models\Model.DBFunction.pas',
  Model.DBTrigger in 'Engine\Models\Model.DBTrigger.pas',
  Model.DBGenerator in 'Engine\Models\Model.DBGenerator.pas',
  Model.DBView in 'Engine\Models\Model.DBView.pas',
  Database in 'DatabaseMigrator\Database.pas',
  MegaMigrator in 'DatabaseMigrator\MegaMigrator.pas',
  SqlResources in 'Engine\SqlResources.pas',
  FirebirdKeywords in 'Engine\FirebirdKeywords.pas',
  Sql.Builder in 'Engine\SQLBuilders\Sql.Builder.pas',
  Sql.Builder.SqlTemplate in 'Engine\SQLBuilders\Sql.Builder.SqlTemplate.pas',
  Sql.Script.Builder in 'Engine\SQLBuilders\Sql.Script.Builder.pas',
  Migration in 'Migration.pas',
  Parser.Interfaces in 'Engine\Parsers\Parser.Interfaces.pas',
  Parser.Procedures in 'Engine\Parsers\Parser.Procedures.pas',
  Splitters in 'Engine\Parsers\Splitters.pas',
  Parser.Tables in 'Engine\Parsers\Parser.Tables.pas',
  Firebird.Types in 'Engine\Firebird.Types.pas',
  Parser.Fields in 'Engine\Parsers\Parser.Fields.pas',
  Parser.Constraints in 'Engine\Parsers\Parser.Constraints.pas',
  Parser.Indices in 'Engine\Parsers\Parser.Indices.pas',
  Parser.Views in 'Engine\Parsers\Parser.Views.pas',
  Parser.Functions in 'Engine\Parsers\Parser.Functions.pas',
  Parser.Triggers in 'Engine\Parsers\Parser.Triggers.pas',
  DelphiUnitWriter in 'Engine\DelphiUnitWriter.pas',
  Migration.ClassWriter in 'Migration.ClassWriter.pas',
  TaskTimer in 'TaskTimer.pas',
  DatabaseInterfaces in 'DatabaseMigrator\DatabaseInterfaces.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
