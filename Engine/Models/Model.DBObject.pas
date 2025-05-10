unit Model.DBObject;

interface

uses System.SysUtils,FirebirdKeywords, Sql.Query.Builder.CommandTemplate;

   type TDBObject = class

   published
       function CommandBuilder(commandTemplate: string) : TCommandTemplate;

   private
     FName: string;

   public
       property Name : string read FName write FName;


       function GetFormatedName: string;
       function CreateCommand : string; virtual;


   end;

implementation

{ TDBObject }


function TDBObject.CommandBuilder(commandTemplate: string): TCommandTemplate;
begin
   result := TCommandTemplate.Create(commandTemplate);
end;



function TDBObject.CreateCommand: string;
begin
  raise Exception.Create('Implemente o método CreateCommand');
end;

function TDBObject.GetFormatedName: string;
begin
 if (TFirebirdKeywords.IsReservedWord(Name)) then
     Result := '"'+Name.ToUpper()+'"'
 else Result := Name.ToUpper();
end;

end.
