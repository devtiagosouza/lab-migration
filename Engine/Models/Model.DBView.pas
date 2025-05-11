unit Model.DBView;

interface

uses Model.DBObject, System.StrUtils, System.Classes, Sql.Builder;

type TDBView = class(TDBObject)

private
  FViewSource: string;
    FFieldList: TStringList;

public

   property ViewSource : string read FViewSource write FViewSource;
   property FieldList : TStringList read FFieldList write FFieldList;

   function DDLCreate: string; override;
end;


implementation

{ TDBView }

function TDBView.DDLCreate: string;
begin
  Result :=   TSQLBuilder.Create.Append('CREATE OR ALTER VIEW '+GetFormatedName+'(')
                .IncIndent
                .AppendLine(FieldList,','+sLineBreak)
                .DecIndent
                .Append(')')
                .AppendLine('AS')
                .AppendLine(ViewSource)
                .AppendLine(';')
                .asString;


end;

End.
