unit Model.DBProcedure;

interface

  uses Model.DBObject;

  type TDBProcedure = class(TDBObject)

  private
     FProcedureSource: string;


  public
      property ProcedureSource : string read FProcedureSource write FProcedureSource;

  end;


implementation

end.
