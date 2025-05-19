unit Migration.ClassWriter;

interface

uses DelphiUnitWriter, Model.DBObject, DCollections, System.Generics.Collections, System.SysUtils;

Type TMigrationClassWriter = class(TDelphiUnitWriter)

private
   FPath : string;

public
    constructor Create(APath : string);

     procedure SavePas<T : TDBObject>(ADBObjects : TList<T>);
end;

implementation

{ TMigrationClassWriter }

constructor TMigrationClassWriter.Create(APath : string);
begin
  inherited Create;
  FPath := APath;
end;

procedure TMigrationClassWriter.SavePas<T>(ADBObjects: TList<T>);
var
 dbObject : TDBObject;
 script: string;
 line : string;
 i : integer;
 comms : TArray<string>;
 tipoObjeto : string;
begin

    UsesDeclaration := 'System.SysUtils, System.Classes, Model.DBTable, Migration';
    ConstructorBody := '';
    MainClass := 'TMigration';



    for dbObject in ADBObjects do begin

        UnitName := 'Migrations.Tabelas';
        ClassName := 'TMigrationTables';

       comms := dbObject.DDLCreate.Split([sLineBreak]);
       for i := 0 to length(comms) - 1 do begin

          if (i < length(comms) - 1) then
             script := script + QuotedStr(comms[i])+'+sLineBreak+'+sLineBreak
          else script := script + QuotedStr(comms[i]);


       end;


       AddMethod(mehodProcedure,'_'+dbObject.Name,
        'AddScript('+script+');'
        );

    end;

    SaveUnitFile(FPath);
end;

end.
