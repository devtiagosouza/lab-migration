unit Model.DBObject;

interface

uses System.SysUtils,FirebirdKeywords;



type


  TDBObject = class

   protected
    function IsSameObject(Obj: TDBObject) : boolean;
    function IsSameText(const text1,text2 : string) : boolean;


   private
    FName: string;
    FObjectTypeFriendlyName: string;
    function GetObjectTypeFriendlyName: string;

   public
       property Name : string read FName write FName;
       property ObjectTypeFriendlyName : string read GetObjectTypeFriendlyName write FObjectTypeFriendlyName;


       function GetFormatedName: string;
       function DDLCreate : string; virtual;
       function DDLDrop : string; virtual;

       function EqualityScript(Obj: TDBObject) : string; virtual;
   end;




implementation

{ TDBObject }




function TDBObject.DDLCreate: string;
begin
  raise Exception.Create('Implemente o método CreateCommand');
end;

function TDBObject.DDLDrop: string;
begin
  result := '';
end;

function TDBObject.EqualityScript(Obj: TDBObject): string;
begin
  raise Exception.Create('Implemente o método EqualityScript');
end;


function TDBObject.GetFormatedName: string;
begin
 if (TFirebirdKeywords.IsReservedWord(Name)) then
     Result := '"'+Name.ToUpper()+'"'
 else Result := Name.ToUpper();
end;

function TDBObject.GetObjectTypeFriendlyName: string;
begin
  if (String.IsNullOrEmpty(FObjectTypeFriendlyName)) then begin
    result := Self.ClassType.ClassName;
    result :=  result.Replace('TDB','');
  end
  else Result := FObjectTypeFriendlyName;

end;



function TDBObject.IsSameObject(Obj: TDBObject): boolean;
begin
  if Pointer(Self) = Pointer(Obj) then
    Exit(True);

  if Obj = nil then
    Exit(False);

  if Obj.ClassType <> Self.ClassType then
    Exit(False);

   Result := (Self.Name = Obj.Name);
end;

function TDBObject.IsSameText(const text1,text2 : string): boolean;
 var
  CleanStr1, CleanStr2: string;
begin

  CleanStr1 := StringReplace(text1, ' ', '', [rfReplaceAll]);
  CleanStr1 := StringReplace(CleanStr1, #13#10, '', [rfReplaceAll]);
  CleanStr1 := StringReplace(CleanStr1, #10, '', [rfReplaceAll]);

  CleanStr2 := StringReplace(text2, ' ', '', [rfReplaceAll]);
  CleanStr2 := StringReplace(CleanStr2, #13#10, '', [rfReplaceAll]);
  CleanStr2 := StringReplace(CleanStr2, #10, '', [rfReplaceAll]);

  Result := CleanStr1.ToLower = CleanStr2.ToLower;
end;

end.
