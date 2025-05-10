unit Model.DBFunction;

interface

  uses Model.DBObject;

  type TDBFunction = class(TDBObject)

  private
    FFunctionSource: string;


  public
      property FunctionSource : string read FFunctionSource write FFunctionSource;

  end;


implementation

end.

