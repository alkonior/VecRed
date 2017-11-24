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
    function PointInFigure(point: TFloatPoint): boolean; virtual; abstract;
    procedure GetParams(); virtual; abstract;
  end;

  TPolyline = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    MinP, MaxP: TFloatPoint;
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    procedure GetParams(); override;
  end;

  TLine = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
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
    function PointInFigure(point: TFloatPoint): boolean; override;
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
    function PointInFigure(point: TFloatPoint): boolean; override;
    procedure GetParams(); override;
  end;

  TRectZoom = class(TFigure)
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
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
    function PointInFigure(point: TFloatPoint): boolean; override;
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
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(points[0]).x -
    WorldToScrn(points[1]).x) div 2 + 1, abs(WorldToScrn(points[0]).y -
    WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.Pen.Color := C1;
  Canvas.Brush.Color := C2;
  Canvas.Pen.Style := P;
  Canvas.Brush.Style := B;
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x + (Canvas.Pen.Width div 2),
    WorldToScrn(min(points[0], points[1])).y + (Canvas.Pen.Width div 2),
    WorldToScrn(max(points[0], points[1])).x - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(points[0], points[1])).y - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    round((RX - (Canvas.Pen.Width div 2)) * zoom / 100),
    round((RY - (Canvas.Pen.Width div 2)) * zoom / 100));
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(points[0]).x -
    WorldToScrn(points[1]).x) div 2 + 1, abs(WorldToScrn(points[0]).y -
    WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.Pen.Color := C1;
  Canvas.Brush.Color := C2;
  Canvas.Pen.Style := P;
  Canvas.Brush.Style := B;
  Canvas.Rectangle(WorldToScrn(min(points[0], points[1])).x + (Canvas.Pen.Width div 2),
    WorldToScrn(min(points[0], points[1])).y + (Canvas.Pen.Width div 2),
    WorldToScrn(max(points[0], points[1])).x - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(points[0], points[1])).y - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)));
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(points[0]).x -
    WorldToScrn(points[1]).x) div 2 + 1, abs(WorldToScrn(points[0]).y -
    WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.Pen.Color := C1;
  Canvas.Brush.Color := C2;
  Canvas.Pen.Style := P;
  Canvas.Brush.Style := B;
  Canvas.Ellipse(WorldToScrn(min(points[0], points[1])).x +
    (Canvas.Pen.Width div 2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(min(points[0], points[1])).y + (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(points[0], points[1])).x - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(points[0], points[1])).y - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)));
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
var
  Wid: integer;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  wid := min(W, min(abs(WorldToScrn(points[0]).x - WorldToScrn(points[1]).x) div
    2 + 1, abs(WorldToScrn(points[0]).y - WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x - 1,
    WorldToScrn(min(points[0], points[1])).y - 1,
    WorldToScrn(max(points[0], points[1])).x + 1,
    WorldToScrn(max(points[0], points[1])).y + 1,
    round((RX + (Wid)) * zoom / 100),
    round((RY + (Wid)) * zoom / 100));
  Canvas.Pen.Color := clWhite;
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x - 2,
    WorldToScrn(min(points[0], points[1])).y - 2,
    WorldToScrn(max(points[0], points[1])).x + 2,
    WorldToScrn(max(points[0], points[1])).y + 2,
    round((RX + Wid) * zoom / 100),
    round((RY + Wid) * zoom / 100));
  Canvas.Pen.Mode := pmCopy;
end;

procedure TRectangle.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.Rectangle(WorldToScrn(min(points[0], points[1])).x - 1,
    WorldToScrn(min(points[0], points[1])).y - 1,
    WorldToScrn(max(points[0], points[1])).x + 1,
    WorldToScrn(max(points[0], points[1])).y + 1);
  Canvas.Pen.Color := clWhite;
  Canvas.Rectangle(WorldToScrn(min(points[0], points[1])).x - 2,
    WorldToScrn(min(points[0], points[1])).y - 2,
    WorldToScrn(max(points[0], points[1])).x + 2,
    WorldToScrn(max(points[0], points[1])).y + 2);
  Canvas.Pen.Mode := pmCopy;
end;

procedure TEllipse.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 2;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.Ellipse(WorldToScrn(min(points[0], points[1])).x - 2,
    WorldToScrn(min(points[0], points[1])).y - 2,
    WorldToScrn(max(points[0], points[1])).x + 2,
    WorldToScrn(max(points[0], points[1])).y + 2);
  Canvas.Pen.Color := clWhite;
  Canvas.Ellipse(WorldToScrn(min(points[0], points[1])).x - 4,
    WorldToScrn(min(points[0], points[1])).y - 4,
    WorldToScrn(max(points[0], points[1])).x + 4,
    WorldToScrn(max(points[0], points[1])).y + 4);
  Canvas.Pen.Mode := pmCopy;
end;

procedure TPolyline.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 2;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.rectangle(WorldToScrn(MinP).x - 2, WorldToScrn(MinP).y - 2,
    WorldToScrn(MaxP).x + 2, WorldToScrn(MaxP).y + 2);
  Canvas.Pen.Color := clWhite;
  Canvas.rectangle(WorldToScrn(MinP).x - 3, WorldToScrn(MinP).y - 3,
    WorldToScrn(MaxP).x + 3, WorldToScrn(MaxP).y + 3);
  Canvas.Pen.Mode := pmCopy;
end;

procedure TLine.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 2;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 2,
    WorldToScrn(min(points[0], points[1])).y - 2,
    WorldToScrn(max(points[0], points[1])).x + 2,
    WorldToScrn(max(points[0], points[1])).y + 2);
  Canvas.Pen.Color := clWhite;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 2,
    WorldToScrn(min(points[0], points[1])).y - 2,
    WorldToScrn(max(points[0], points[1])).x + 2,
    WorldToScrn(max(points[0], points[1])).y + 2);
  Canvas.Pen.Mode := pmCopy;
end;

procedure TRectZoom.DrawOutLine(Canvas: TCanvas);
begin
end;

{ Create }

function TPolyline.PointInFigure(point: TFloatPoint): boolean;
var
  i: integer;
begin
  for i := 0 to length(points) - 2 do
  begin
    Result :=
      ((IsPointOnLine(points[i], points[i + 1], point)) or
      (IsPointInEllipse(points[i], point, 3, 3)) or
      (IsPointInEllipse(points[i + 1], point, 3, 3)));
    if Result then
      break;
  end;
end;

function TLine.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := ((IsPointOnLine(points[0], points[1], point)) or
    (IsPointInEllipse(points[0], point, 3, 3)) or
    (IsPointInEllipse(points[1], point, 3, 3)));
end;

function TRectangle.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := IsPointInRect(min(points[0], points[1]), max(points[0], points[1]), point);
end;

function TEllipse.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := IsPointInEllipse((max(points[1], points[0]) + min(points[1], points[0])) /
    2, point, abs((max(points[1], points[0]) - min(points[1], points[0])).x / 2),
    abs((max(points[1], points[0]) - min(points[1], points[0])).y / 2));
end;

function TRoundRect.PointInFigure(point: TFloatPoint): boolean;
var
  ryy, rxx: real;
  ww: integer;
begin
  ryy := ry / 2;
  rxx := rx / 2;
  if ((abs(points[0].x - points[1].x)) > (rxx * 2)) and
    ((abs(points[0].y - points[1].y)) > (ryy * 2)) then
  begin
    Result := IsPointInRect((min(points[0], points[1]) + FloatPoint(rxx, 0)),
      (max(points[0], points[1]) - FloatPoint(rxx, 0)), point) or
      (IsPointInRect((min(points[0], points[1]) + FloatPoint(0, ryy)),
      (max(points[0], points[1]) - FloatPoint(0, ryy)), point)) or
      (IsPointInEllipse((min(points[0], points[1]) + FloatPoint(rxx, ryy)), point, rxx, ryy)) or
      (IsPointInEllipse((max(points[0], points[1]) - FloatPoint(rxx, ryy)), point, rxx, ryy)) or
      (IsPointInEllipse((FloatPoint(min(points[0], points[1]).x, max(points[0], points[1]).y) +
      FloatPoint(rxx, -ryy)), point, rxx, ryy)) or
      (IsPointInEllipse((FloatPoint(max(points[0], points[1]).x, min(points[0], points[1]).y) +
      FloatPoint(-rxx, ryy)), point, rxx, ryy));
  end;
  if ((abs(points[0].x - points[1].x)) < (rxx * 2)) and
    ((abs(points[0].y - points[1].y)) < (ryy * 2)) then
  begin
    Result := IsPointInEllipse((max(points[1], points[0]) + min(points[1], points[0])) /
      2, point, abs((max(points[1], points[0]) - min(points[1], points[0])).x / 2),
      abs((max(points[1], points[0]) - min(points[1], points[0])).y / 2));
  end;
  if ((abs(points[0].x - points[1].x)) > (rxx * 2)) and
    ((abs(points[0].y - points[1].y)) < (ryy * 2)) then
  begin
    Result := IsPointInRect((min(points[0], points[1]) + FloatPoint(rxx, 0)),
      (max(points[0], points[1]) - FloatPoint(rxx, 0)), point)
      or (IsPointInEllipse((min(points[0], points[1])+floatpoint(rxx,abs(points[1].y-points[0].y)/2)),point,rxx,abs(points[0].y-points[1].y)/2))
      or (IsPointInEllipse(max(points[0], points[1])+floatpoint(-rxx,-(abs(points[1].y-points[0].y)/2)),point,rxx,abs(points[0].y-points[1].y)/2));
  end;
  if ((abs(points[0].x - points[1].x)) < (rxx * 2)) and
    ((abs(points[0].y - points[1].y)) > (ryy * 2)) then
  begin
    Result := IsPointInRect((min(points[0], points[1]) + FloatPoint(0, ryy)),
      (max(points[0], points[1]) - FloatPoint(0, ryy)), point)
      or (IsPointInEllipse((min(points[0], points[1])+floatpoint(abs(points[1].x-points[0].x)/2,ryy)),point,abs(points[1].x-points[0].x)/2,ryy))
      or (IsPointInEllipse((max(points[0], points[1])-floatpoint(abs(points[1].x-points[0].x)/2,ryy)),point,abs(points[1].x-points[0].x)/2,ryy));
  end
end;

function TRectZoom.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := False;
end;

{ Create }

constructor TPolyline.Create(point: TFloatPoint);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
  MinP := Point;
  MaxP := Point;
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
