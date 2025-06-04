unit Model.DBView;

interface

uses Model.DBObject, System.Classes, Sql.Builder;

type TDBView = class(TDBObject)

private
  FViewSource: string;
  FFieldList: TStringList;

public

   property ViewSource : string read FViewSource write FViewSource;
   property FieldList : TStringList read FFieldList write FFieldList;

   function DDLCreate: string; override;
   function EqualityScript(Obj: TDBObject; args : array of TObject) : string; override;

   constructor Create(AName : string);
end;


implementation

{ TDBView }

constructor TDBView.Create(AName : string);
begin
   inherited Create(AName);
   FFieldList := TStringList.Create;

end;

function TDBView.DDLCreate: string;
begin
  Result :=   TSQLBuilder.Create.Append('CREATE OR ALTER VIEW '+GetFormatedName+'(')
                .IncIndent
                .AppendLine(FieldList,','+sLineBreak)
                .DecIndent
                .Append(')')
                .AppendLine('AS')
                .AppendLine(ViewSource)
                .AsString(';');


end;

function TDBView.EqualityScript(Obj: TDBObject; args : array of TObject): string;
begin
 result := '';
 if (isSameObject(Obj)) then begin
   if (not isSameText(DDLCreate, obj.DDLCreate)) then begin
      result := DDLCreate;
   end;
 end;
end;

End.
