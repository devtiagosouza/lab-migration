unit Model.DBObject;

interface

uses System.SysUtils,FirebirdKeywords;

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


   type IBuilder<T : TDBObject> = interface
   ['{64A369F8-9A59-48A8-BD86-FD38CB4DE4B0}']
   function New(const AName : string) : IBuilder<T>;

   end;

   type TBuilder<T : TDBObject> = class(TInterfacedObject, IBuilder<T>)

   private
       FModel : T;
   public 
      function New(const AName : string) : IBuilder<T>;  
      function AsDBObject : T;
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

{ Builder<T> }

function TBuilder<T>.AsDBObject: T;
begin
  Result := FModel;
end;

function TBuilder<T>.New(const AName: string): IBuilder<T>;
begin
  FModel := TDBObject.Create() as T;
  FModel.Name := AName; 
  Result := Self;
end;

end.
