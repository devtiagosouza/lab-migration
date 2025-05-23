unit Model.DBProcedure;

interface

  uses Model.DBObject,DCollections, Model.DBField, Sql.Builder;

  type TDBProcedure = class(TDBObject)

  private
    FProcedureSource: string;
    FInputFields: TList<TDBField>;
    FOutputFields: TList<TDBField>;

  public
      property ProcedureSource : string read FProcedureSource write FProcedureSource;
      property InputFields : TList<TDBField> read FInputFields write FInputFields;
      property OutputFields : TList<TDBField> read FOutputFields write FOutputFields;


     function DDLCreate: string; override;

     function EqualityScript(Obj: TDBObject) : string; override;

     constructor Create();
  end;


implementation

{ TDBProcedure }

constructor TDBProcedure.Create();
begin
  InputFields := TList<TDBField>.Create;
  OutputFields := TList<TDBField>.Create;

end;

function TDBProcedure.DDLCreate: string;
var
 sql : TSQLBuilder;
 i : integer;
 vField : TDBField;
begin
  sql := TSQLBuilder.Create;
  sql.Append('CREATE OR ALTER PROCEDURE '+GetFormatedName);

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

   sql.ResetIndent;

   if (OutputFields.Count > 0) then
   begin
      sql.AppendLine('RETURNS (')
         .IncIndent;

      for I := 0 to  Pred(OutputFields.Count) do
      begin
         vField := OutputFields[i];
         sql.AppendLine(vField.GetFullFieldSet);

         if (i < Pred(OutputFields.Count)) then
            Sql.DecIndent.Append(',').IncIndent
         else  Sql.DecIndent.Append(')')
      end;

   end;

   sql.ResetIndent;

   sql.AppendLine('AS')
      .AppendLine(ProcedureSource);


  result := sql.AsString('^');
end;

function TDBProcedure.EqualityScript(Obj: TDBObject): string;
begin
 result := '';
 if (isSameObject(Obj)) then begin
     if (not isSameText(DDLCreate, Obj.DDLCreate)) then begin
        result := DDLCreate;
     end;
 end;
end;

End.
