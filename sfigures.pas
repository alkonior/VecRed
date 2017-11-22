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
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure DrawoutLine(Canvas: TCanvas); virtual; abstract;
    procedure GetParams(); virtual; abstract;
  end;

  TPolyline = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    procedure GetParams(); override;
  end;

  TLine = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    procedure GetParams(); override;
  end;

  TRectangle = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    C2: TColor;
    B: TBrushStyle;
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    procedure GetParams(); override;
  end;

  TEllipse = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    C2: TColor;
    B: TBrushStyle;
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    procedure GetParams(); override;
  end;

  TRectZoom = class(TFigure)
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    procedure GetParams(); override;
  end;

  TRoundRect = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    C2: TColor;
    B: TBrushStyle;
    RY, RX: integer;
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    procedure GetParams(); override;
  end;

var      { Var }
  Figures: array of TFigure;
  PenColor: TColor = clBlack;
  BrushColor: TColor = clWhite;
  WidthOfFigure: integer = 1;
  PenStyle: TPenStyle = psSolid;
  BrushStyle: TBrushStyle = bsSolid;
  Drawing: boolean = False;
  RadXOfFigure: integer = 0;
  RadYOfFigure: integer = 0;
  SelectedNumber: integer = 0;

implementation

{ Porocedures }

{ GetParams }

procedure TPolyline.GetParams();
begin
  C1 := PenColor;
  W := WidthOfFigure;
  P := PenStyle;
end;

procedure TLine.GetParams();
begin
  C1 := PenColor;
  W := WidthOfFigure;
  P := PenStyle;
end;

procedure TRectangle.GetParams();
begin
  C1 := PenColor;
  C2 := BrushColor;
  W := WidthOfFigure;
  P := PenStyle;
  B := BrushStyle;
end;

procedure TEllipse.GetParams();
begin
  C1 := PenColor;
  C2 := BrushColor;
  W := WidthOfFigure;
  P := PenStyle;
  B := BrushStyle;
end;

procedure TRoundRect.GetParams();
begin
  C1 := PenColor;
  C2 := BrushColor;
  W := WidthOfFigure;
  P := PenStyle;
  B := BrushStyle;
  RY := RadYOfFigure;
  RX := RadXOfFigure;
end;

procedure TRectZoom.GetParams();
begin
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
    WorldToScrn(points[1]).x, WorldToScrn(points[1]).y, Rx, RY);
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

{ DrawOutLine }

procedure TRoundRect.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x - w,
    WorldToScrn(min(points[0], points[1])).y - w,
    WorldToScrn(max(points[0], points[1])).x + w,
    WorldToScrn(max(points[0], points[1])).y + w, Rx, RY);
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x + w,
    WorldToScrn(min(points[0], points[1])).y + w,
    WorldToScrn(max(points[0], points[1])).x - w,
    WorldToScrn(max(points[0], points[1])).y - w, Rx, RY);
  Canvas.Pen.Mode := pmCopy;
end;

procedure TRectangle.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.Rectangle(WorldToScrn(min(points[0], points[1])).x - w,
    WorldToScrn(min(points[0], points[1])).y - w,
    WorldToScrn(max(points[0], points[1])).x + w,
    WorldToScrn(max(points[0], points[1])).y + w);
  Canvas.Rectangle(WorldToScrn(min(points[0], points[1])).x + w,
    WorldToScrn(min(points[0], points[1])).y + w,
    WorldToScrn(max(points[0], points[1])).x - w,
    WorldToScrn(max(points[0], points[1])).y - w);
  Canvas.Pen.Mode := pmCopy;
end;

procedure TEllipse.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.Ellipse(WorldToScrn(min(points[0], points[1])).x - w,
    WorldToScrn(min(points[0], points[1])).y - w,
    WorldToScrn(max(points[0], points[1])).x + w,
    WorldToScrn(max(points[0], points[1])).y + w);
  Canvas.Ellipse(WorldToScrn(min(points[0], points[1])).x + w,
    WorldToScrn(min(points[0], points[1])).y + w,
    WorldToScrn(max(points[0], points[1])).x - w,
    WorldToScrn(max(points[0], points[1])).y - w);
  Canvas.Pen.Mode := pmCopy;
end;

procedure TPolyline.DrawOutLine(Canvas: TCanvas);
var
  i: integer;
begin
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  for i := 0 to length(points) - 2 do
  begin
    Canvas.Line((WorldToScrn(Points[i]) + Tpoint.Create(0, W)),
      WorldToScrn(points[i + 1]) + Tpoint.Create(0, W));
    Canvas.Line(WorldToScrn(Points[i]) + Tpoint.Create(0, -W),
      WorldToScrn(points[i + 1]) + Tpoint.Create(0, -W));
  end;
  Canvas.Pen.Mode := pmCopy;
end;

procedure TLine.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.Line(WorldToScrn(Points[0]) + Tpoint.Create(0, W),
    WorldToScrn(points[1]) + Tpoint.Create(0, W));
  Canvas.Line(WorldToScrn(Points[0]) + Tpoint.Create(0, -W),
    WorldToScrn(points[1]) + Tpoint.Create(0, -W));
  Canvas.Pen.Mode := pmCopy;
end;

procedure TRectZoom.DrawOutLine(Canvas: TCanvas);
begin
end;

{ Create }

constructor TPolyline.Create(point: TFloatPoint);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
end;

constructor TLine.Create(point: TFloatPoint);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
end;

constructor TRectangle.Create(point: TFloatPoint);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
end;

constructor TEllipse.Create(point: TFloatPoint);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
end;

constructor TRectZoom.Create(point: TFloatPoint);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
end;

constructor TRoundRect.Create(point: TFloatPoint);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
end;

end.
