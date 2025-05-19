unit Model.DBObject;

interface

uses System.SysUtils,FirebirdKeywords, Sql.Builder.SqlTemplate;

   type TDBObject = class

   private
    FName: string;
    FObjectTypeFriendlyName: string;
    function GetObjectTypeFriendlyName: string;

   public
       property Name : string read FName write FName;
       property ObjectTypeFriendlyName : string read GetObjectTypeFriendlyName write FObjectTypeFriendlyName;


       function GetFormatedName: string;
       function DDLCreate : string; virtual;


   end;

implementation

{ TDBObject }




function TDBObject.DDLCreate: string;
begin
  raise Exception.Create('Implemente o método CreateCommand');
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

end.
