unit DBGrids.Helper;

interface

uses
  Vcl.DBGrids, DB, VCL.Graphics;

  type
  TDBGridHelper = class helper for TDBGrid
  public
    procedure BestFitColumns;
  end;


implementation

{ TDBGridHelper }

procedure TDBGridHelper.BestFitColumns;
var
  i, MaxWidth, CellWidth: Integer;
  Dataset: TDataSet;
  Coluna: TColumn;
  Canvas: TCanvas;
  Texto: string;
begin
  if not Assigned(Self) or not Assigned(Self.DataSource) or not Assigned(Self.DataSource.DataSet) then
    Exit;

  Dataset := Self.DataSource.DataSet;
  Canvas := Self.Canvas;

  for i := 0 to Self.Columns.Count - 1 do
  begin
    Coluna := Self.Columns[i];
    MaxWidth := Canvas.TextWidth(Coluna.Title.Caption) + 10;

    Dataset.DisableControls;
    try
      Dataset.First;
      while not Dataset.Eof do
      begin
        Texto := Dataset.FieldByName(Coluna.FieldName).AsString;
        CellWidth := Canvas.TextWidth(Texto) + 10;
        if CellWidth > MaxWidth then
          MaxWidth := CellWidth;
        Dataset.Next;
      end;
    finally
      Dataset.EnableControls;
    end;

    Coluna.Width := MaxWidth;
  end;
end;

end.
