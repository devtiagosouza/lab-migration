unit Trigger.Builder;

interface

uses System.Classes,Model.DBTrigger, DCollections;

type ITriggerBuilder = interface
['{B3B2531F-482F-4DD6-87EA-9879548FC349}']
   function New(const aName : string; const aTableName: string): ITriggerBuilder;
   function Column(const aColumnName, aTypeAndDefs: string): ITriggerBuilder;
   function Build: TDBTrigger;

   function GetTriggerName : string;
end;

type TTriggerBuilder = class(TInterfacedObject,ITriggerBuilder)

private
   FTrigger : TDBTrigger;

public
   function New(const aName : string; const aTableName: string): ITriggerBuilder;
   function Column(const aColumnName, aTypeAndDefs: string): ITriggerBuilder;
   function Build: TDBTrigger;

   function GetTriggerName : string;
end;

implementation

{ TTriggerBuilder }

function TTriggerBuilder.Build: TDBTrigger;
begin

end;

function TTriggerBuilder.Column(const aColumnName,
  aTypeAndDefs: string): ITriggerBuilder;
begin

end;

function TTriggerBuilder.GetTriggerName: string;
begin
   Result := FTrigger.Name;
end;

function TTriggerBuilder.New(const aName, aTableName: string): ITriggerBuilder;
begin

end;

end.
