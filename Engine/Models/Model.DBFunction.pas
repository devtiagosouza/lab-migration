unit Model.DBFunction;

interface

  uses Model.DBObject,DCollections, Model.DBField, Sql.Builder;

  type TDBFunction = class(TDBObject)

  private
    FFunctionSource: string;
    FReturnType: string;



    FInputFields: TList<TDBField>;
  public
      property FunctionSource : string read FFunctionSource write FFunctionSource;
      property ReturnType : string read FReturnType write FReturnType;
      property InputFields : TList<TDBField> read FInputFields write FInputFields;

      function DDLCreate: string; override;

      function EqualityScript(Obj: TDBObject) : string; override;

      constructor Create();
  end;


implementation

{ TDBFunction }

constructor TDBFunction.Create();
begin
   inherited Create;
   FInputFields := TList<TDBField>.Create;
   ObjectTypeFriendlyName := 'Function';
end;

function TDBFunction.DDLCreate: string;
var
 sql : TSQLBuilder;
 i : integer;
 vField : TDBField;
begin
   sql := TSQLBuilder.Create;
  sql.Append('CREATE OR ALTER FUNCTION '+GetFormatedName);

  if (InputFields.Count > 0) then
  begin
    sql.Append(' (')
       .IncIndent;

    for I := 0 to  Pred(InputFields.Count) do
    begin
       vField := InputFields[i];
       sql.AppendLine(vField.GetFullFieldSet);

       if (i < Pred(InputFields.Count)) then
          Sql.DecIndent.Append(',').IncIndent
       else  Sql.DecIndent.Append(')')
    end;
  end;

   sql.AppendLine('RETURNS '+ReturnType);


   sql.AppendLine('AS')
      .AppendLine(FunctionSource);


  result := sql.AsString('^');
end;

function TDBFunction.EqualityScript(Obj: TDBObject): string;
begin
 result := '';
 if (isSameObject(Obj)) then begin
     if (not isSameText(DDLCreate, obj.DDLCreate)) then begin
         result := DDLCreate;
     end;
 end;


end;

End.

