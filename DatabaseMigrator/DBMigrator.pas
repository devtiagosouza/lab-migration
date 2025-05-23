unit DBMigrator;

interface

type IDBMigrator = interface
['{ED1996A6-0C1A-46F6-A3C4-5FE39161C073}']
   procedure Execute;

end;


type TDBMigrator = class

private


public

   constructor Create();
end;


implementation

{ TDBMigrator }

constructor TDBMigrator.Create();
begin
  inherited;

end;

end.
