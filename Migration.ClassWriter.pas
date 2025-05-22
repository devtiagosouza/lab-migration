unit Migration.ClassWriter;

interface

uses DelphiUnitWriter,System.RegularExpressions, Model.DBObject, System.Rtti,
    System.Classes, System.Generics.Collections, System.SysUtils;

Type TMigrationClassWriter = class(TDelphiUnitWriter)

private
   FPath : string;
   function MaxLengthFromLines(lines : TArray<string>) : integer;
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

function TMigrationClassWriter.MaxLengthFromLines(
  lines: TArray<string>): integer;
  var
  line: string;
  MaxLength, LengthNome: Integer;
begin
  MaxLength := 0;

  for line in lines do
  begin
    LengthNome := Length(line);
    if LengthNome > MaxLength then
      MaxLength := LengthNome;
  end;

  Result := MaxLength;
end;

procedure TMigrationClassWriter.SavePas<T>(ADBObjects: TList<T>);
var
 dbObject : TDBObject;
 script: string;
 line : string;
 i : integer;
 comms : TArray<string>;
 tipoObjeto : string;
 maxLength : integer;
 whiteSpace : integer;
 space : string;
 className : string;
 objectTypeName : string;
 Regex : TRegEx;
 Context: TRttiContext;
 RttiType: TRttiType;
 TypeName: string;
begin
    Context := TRttiContext.Create;
    RttiType := Context.GetType(TypeInfo(T));

    TypeName := RttiType.Name;



    UsesDeclaration := 'System.SysUtils, System.Classes, Model.'+Copy(TypeName,2)+', Migration';
    ConstructorBody := '';
    MainClass := 'TMigration';

    for dbObject in ADBObjects do begin

       UnitName := 'Migration.'+dbObject.ObjectTypeFriendlyName+'s';
       self.ClassName := 'TMigration'+dbObject.ObjectTypeFriendlyName+'s';

       comms := dbObject.DDLCreate.Split([sLineBreak]);
       maxLength := MaxLengthFromLines(comms);
       for i := 0 to length(comms) - 1 do begin
          line := comms[i];

           Regex := TRegEx.Create('/\*[\s\S]*?\*/', [roIgnoreCase]);
           line := Regex.Replace(line, '');




          whiteSpace := maxLength - (QuotedStr(line).length - 2);

          space := StringOfChar(' ',whiteSpace);

          if (i < length(comms) - 1) then
             script := script + QuotedStr(line+space)+'+sLineBreak+'+sLineBreak
          else script := script + QuotedStr(line+space);

       end;

       AddMethod(mehodProcedure,dbObject.ObjectTypeFriendlyName+'_'+dbObject.Name,
        'AddScript('+slineBreak+
          script+');'
        );

    end;



    SaveUnitFile(FPath);
end;

end.
