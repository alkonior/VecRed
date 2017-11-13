unit SFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, Menus, ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType,
  Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL, Windows, UScale;

type
  { Classes }
  { TFigure }

  TFigure = class
    Points: array of TFloatPoint;
    Selected: boolean;
    C1: TColor;
    W: integer;
    P: TPenStyle;
    C2: TColor;
    B: TBrushStyle;
    RY, RX: integer;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure GetParams();
  end;

  TPolyline = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TLine = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRectangle = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TEllipse = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRectZoom = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRoundRect = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

var      { Var }
  Figures: array of TFigure;
  PenColor: TColor = clBlack;
  BrushColor: TColor = clWhite;
  Width: integer = 1;
  PenStyle: TPenStyle = psSolid;
  BrushStyle: TBrushStyle = bsSolid;
  Drawing: boolean = False;
  RadX: integer = 0;
  RadY: integer = 0;

implementation

{ Porocedures }
procedure Tfigure.GetParams();
begin
  C1 := PenColor;
  C2 := BrushColor;
  W := Width;
  P := PenStyle;
  B := BrushStyle;
  RY := RadY;
  RX := RadX;
end;

{ Drow }

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := W;
  Canvas.Pen.Color := C1;
  Canvas.Brush.Color := C2;
  Canvas.Pen.Style := P;
  Canvas.Brush.Style := B;
  Canvas.RoundRect(WorldToScrn(points[0]).x, WorldToScrn(points[0]).y,
    WorldToScrn(points[1]).x, WorldToScrn(points[1]).y, RY, RY);
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := W;
  Canvas.Pen.Color := C1;
  Canvas.Brush.Color := C2;
  Canvas.Pen.Style := P;
  Canvas.Brush.Style := B;
  Canvas.Rectangle(WorldToScrn(points[0]).x, WorldToScrn(points[0]).y,
    WorldToScrn(points[1]).x, WorldToScrn(points[1]).y);
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := W;
  Canvas.Pen.Color := C1;
  Canvas.Brush.Color := C2;
  Canvas.Pen.Style := P;
  Canvas.Brush.Style := B;
  Canvas.Ellipse(WorldToScrn(points[0]).x, WorldToScrn(points[0]).y,
    WorldToScrn(points[1]).x, WorldToScrn(points[1]).y);
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var
  i: integer;
begin
  Canvas.Pen.Color := C1;
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := P;
  for i := 0 to length(points) - 2 do
  begin
    Canvas.Line(WorldToScrn(Points[i]), WorldToScrn(points[i + 1]));
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := C1;
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := P;
  Canvas.Line(WorldToScrn(Points[0]), WorldToScrn(points[1]));
end;

procedure TRectZoom.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psDashDot;
  Canvas.Rectangle(WorldToScrn(points[0]).x, WorldToScrn(points[0]).y,
    WorldToScrn(points[1]).x, WorldToScrn(points[1]).y);
  Canvas.Brush.Style := bsSolid;
end;

end.
