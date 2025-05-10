unit DCollections.Grid;

interface

  uses Vcl.DBGrids,Data.DB, System.Generics.Collections, Datasnap.DBClient,
  DCollections.ClientDataSet, System.Classes,DBGrids.Helper;

type
  TDBGrid = class(Vcl.DBGrids.TDBGrid)

  private
    FClientDataSet: TClientDataSet;
    FDataSource: TDataSource;
    FDefaultLayout: Boolean;
    FAutoWidthColumns: Boolean;
    procedure GetDefaultLayout(const Value: Boolean);
    procedure GetAutoWidthColumns(const Value: Boolean);

  public
    property ClientDataSet : TClientDataSet read FClientDataSet write FClientDataSet;

    property DefaultLayout : Boolean read FDefaultLayout write GetDefaultLayout;
    property AutoWidthColumns: Boolean read FAutoWidthColumns write GetAutoWidthColumns;

    procedure LoadFromList<T>(AList: TList<T>);
    function Current<T>: T;

    constructor Create(AOwner: TComponent); override;

  end;


implementation

{ TDBGrid }

constructor TDBGrid.Create(AOwner: TComponent);
begin
  inherited;

  FDefaultLayout := false;
  FAutoWidthColumns := false;

  Self.Options := [dgTitles,
    dgColumnResize, dgColLines, dgRowLines, dgTabs,
    dgConfirmDelete, dgCancelOnExit,
    dgTitleClick, dgTitleHotTrack];


end;

function TDBGrid.Current<T>: T;
begin
  result := FClientDataSet.Current<T>;
end;

procedure TDBGrid.GetAutoWidthColumns(const Value: Boolean);
begin
  FAutoWidthColumns := Value;
  if Value then
    Self.BestFitColumns;
end;

procedure TDBGrid.GetDefaultLayout(const Value: Boolean);
begin
  FDefaultLayout := Value;

  if (FDefaultLayout = true) then begin
    Self.Options := [dgEditing, dgTitles, dgIndicator,
      dgColumnResize, dgColLines, dgRowLines, dgTabs,
      dgConfirmDelete, dgCancelOnExit,
      dgTitleClick, dgTitleHotTrack];
  end;

end;

procedure TDBGrid.LoadFromList<T>(AList: TList<T>);
begin
   if not Assigned(FClientDataSet) then begin
      FClientDataSet := TClientDataSet.Create(Self);
      FDataSource := TDataSource.Create(self);
   end;
   FDataSource.DataSet := FClientDataSet;
   self.DataSource := FDataSource;

   FClientDataSet.LoadFromList<T>(AList, self);
   if AutoWidthColumns then
      Self.BestFitColumns;
end;

end.
