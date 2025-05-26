unit DebugFilter;

interface

uses IniFiles, System.SysUtils, System.Classes;

type IDebugFilter = interface
['{6045F083-688F-404A-A859-30DEF62AF535}']
   function Listar(ObjectName : string) : boolean;

end;

type TDebugFilter = class



public
   class function Listar(ObjectName : string) : boolean; static;

end;

implementation

{ TDebugFilter }

class function TDebugFilter.Listar(ObjectName: string): boolean;
var
   DebugIniFile: TIniFile;
   valores : string;
   list : TStringList;
   index : integer;
   exists : boolean;
begin
  result := true;

   if (FileExists('debug.ini')) then
   begin


      DebugIniFile := TIniFile.Create('.\debug.ini');
      exists := DebugIniFile.ValueExists('Filters','Objects');


      if (exists) then begin
          valores :=  DebugIniFile.ReadString('Filters', 'Objects', '').Trim;
          if (valores <> '') then begin
              list := TStringList.Create;
              list.DelimitedText := valores;
              list.Delimiter := ',';
              index := list.IndexOf(ObjectName);
              result := index >= 0;
          end
          else result := false;
      end;
   end;
end;

end.
