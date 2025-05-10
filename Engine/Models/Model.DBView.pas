unit Model.DBView;

interface

uses Model.DBObject;

type TDBView = class(TDBObject)

private
  FViewSource: string;

public
   property ViewSource : string read FViewSource write FViewSource;

end;


implementation

end.
